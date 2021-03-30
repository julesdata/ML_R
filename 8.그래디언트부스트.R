8. 그래디언트 부스트 

#데이타로딩
library(tidyverse)
list.files()
load( file = 'Bank_DataSet.RDA')

#gbm 패키지 
install.packages('gbm')
library(gbm)

parallel::detectCores()

#gbm 적합
set.seed(1234)
fit1 <- gbm(formula = PersonalLoan~.,
            data = trainSet,
            distribution = 'multinomial',
            n.trees = 5000,
            interaction.depth = 3,
            shrinkage = 0.01,
            n.minobsinnode = 10,
            bag.fraction = 0.5,
            cv.folds = 5,
            n.cores = 7,
            verbose = TRUE)

print(x = fit1)

#변수의 중요도 시각화
par(mar = c(5,8,4,2))
summary(object = fit1, las = 2)
title(main = 'Vriable Importance')

par(mar = c(5,4,4,2))
dev.off()

#최적나무 수 확인
gbm.perf(object = fit1, 
         method = 'cv')
title(main = 'Number of Trees')

#목표변수 추정확률 및 추정값 생성
real <- testSet$PersonalLoan
prob1 <- predict(object = fit1, newdata = testSet, type = 'response')
head(x = prob1) #  " , , 1543" 3차원의 array. 5000개중 1543번째 행렬
prob1 <-prob1[,2,1]
head(x=prob1)
boxplot(formula = prob1 ~ real)

pred1 <- ifelse(test = prob1 >= 0.5, yes = '1', no= '0') %>% as.factor()
head(x=pred1)

#랜덤포레스트 모형과 비교
list.files()
bestRF <- readRDS(file = 'RandomForest.RDS')
class(x = bestRF)
library(randomForest)
pred0 <- predict(object = bestRF, newdata = testSet, type = 'response')
prob0 <- predict(object = bestRF, newdata = testSet, type = 'vote')[, 2]

library(caret)
confusionMatrix(data = pred0, reference = real, positive = '1')
confusionMatrix(data = pred1, reference = real, positive = '1')

library(MLmetrics)
F1_Score(y_true = real, y_pred = pred0, positive = '1')
F1_Score(y_true = real, y_pred = pred1, positive = '1')

library(pROC)
roc(response = real, predictor = prob0) %>% 
  plot(col = 'red', lty = 1)

roc(response = real, predictor = prob1) %>% 
  plot(col = 'blue', lty = 2, add = TRUE)

auc(response = real, predictor = prob0)
auc(response = real, predictor = prob1)



#최적모델 튜닝

grid <- expand.grid(depth = c(1,3,5),
                    learn = c(0.01, 0.05, 0.10),
                    min = c(5,7,10),
                    bag = c(0.5, 0.7, 1.0),
                    verr = NA,
                    tree = NA)

n<-nrow(x=grid)
for(i in 1:n) {
  cat(str_glue('현재 {i}행 실행 중!'), '\n')
  set.seed(seed = 1234)
  fit <- gbm(formula = PersonalLoan ~ ., 
             data = trainSet, 
             distribution = 'multinomial', 
             n.trees = 5000, 
             interaction.depth = grid$depth[i], 
             shrinkage = grid$learn[i], 
             n.minobsinnode = grid$min[i], 
             bag.fraction = grid$bag[i],
             train.fraction = 0.75,
             n.cores = NULL)
  grid$verr[i] <- min(fit$valid.error)
  grid$tree[i] <- which.min(x = fit$valid.error)
}

loc <- which.min(x = grid$verr)
print(x = loc)

bestPara <- grid[loc, ]
print(x = bestPara)

set.seed(seed = 1234)
best <- gbm(formula = PersonalLoan ~ ., 
            data = trainSet, 
            distribution = 'multinomial', 
            n.trees = bestPara$tree, 
            interaction.depth = bestPara$depth, 
            shrinkage = bestPara$learn, 
            n.minobsinnode = bestPara$min, 
            bag.fraction = bestPara$bag,
            n.cores = NULL)

prob2 <- predict(object = best, newdata = testSet, type = 'response', 
                 n.trees = bestPara$tree)
head(x = prob2)
prob2 <- prob2[, 2, 1]
head(x = prob2)
boxplot(formula = prob2 ~ real)
pred2 <- ifelse(test = prob2 >= 0.5, yes = '1', no = '0') %>% as.factor()

confusionMatrix(data = pred1, reference = real, positive = '1')
confusionMatrix(data = pred2, reference = real, positive = '1')

F1_Score(y_true = real, y_pred = pred1, positive = '1')
F1_Score(y_true = real, y_pred = pred2, positive = '1')
F1_Score(y_true = real, y_pred = pred0, positive = '1')

roc(response = real, predictor = prob0) %>% 
  plot(col = 'red', lty = 1)

roc(response = real, predictor = prob1) %>% 
  plot(col = 'blue', lty = 2, add = TRUE)

roc(response = real, predictor = prob2) %>% 
  plot(col = 'purple', lwd = 2, add = TRUE)

auc(response = real, predictor = prob0)
auc(response = real, predictor = prob1)
auc(response = real, predictor = prob2)