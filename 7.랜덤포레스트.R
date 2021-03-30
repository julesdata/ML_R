###### 7. 랜덤포레스트 #######

library(tidyverse)
getwd()
list.files()
load(file = 'Bank_DataSet.RDA')

#분류모형 적합
install.packages('randomForest')
library(randomForest)
set.seed(seed = 1234)
fit1 <- randomForest(formula = PersonalLoan~.,
                     data = trainSet,
                     ntree = 1000,
                     mtry = 3,
                     importance = TRUE,
                     do.trace = 50,
                     keep.forest = TRUE)
print(x= fit1)

#OOB 오차의 추정
print(x = fit1$err.rate) #모형 전체 및 목표변수의 레벨별 OOB 오차
print(x = fit1$err.rate[,1]) # 모형 전체의  OOB 오차
tail(x=fit1$err.rate[,1], n=1) # 최종  OOB 오차: 최적 모형 탐색 시 성능평가의 기준
plot(x=fit1) #오차모형 시각화. 각 레벨별 오차도 함께 그려짐. 

#변수의 중요도
importance(x= fit1, type = 1) #값
varImpPlot(x = fit1, type = 1) #시각화

#개별 나무모형의 끝마디 수 시각화
treesize(x = fit1, terminal = TRUE) %>% hist()

#목표변수의 추정값 생성
real <- testSet$PersonalLoan
pred1 <- predict(object = fit1, newdata = testSet, type = 'response')
table(pred1, real)
prob1 <- predict(object = fit1, newdata = testSet, type = 'vote')[, 2]
head(x = prob1)

#분류모형의 성능평가
#혼동행렬, F1점수
library(caret)
confusionMatrix(data = pred1, reference = real, positive = '1')
library(MLmetrics)
F1_Score(y_true = real, y_pred = pred1, positive = '1')


#ROC곡선, AUC 출력
library(pROC)
roc(response = real, predictor = prob1) %>%
  plot(main = 'ROC Curve', col = 'red', lty = 1)
auc(response = real, predictor = prob1)

#의사결정나무 분류모형과 성능 비교

list.files()
fitDT <- readRDS(file = 'DecisionTree.RDS')


pred0 <- predict(object = fitDT, newdata = testSet, type = 'class') #의사결정나무 추정값

confusionMatrix(data = pred1, reference = real, positive = '1')
confusionMatrix(data = pred0, reference = real, positive = '1')

F1_Score(y_true = real, y_pred = pred1, positive = '1')
F1_Score(y_true = real, y_pred = pred0, positive = '1')

prob0 <- predict(object = fitDT, newdata = testSet, type = 'prob')[, 2] #의사결정나무 추정확률

roc(response = real, predictor = prob0) %>%
  plot(col = 'blue', lty = 2, add = TRUE)  #roc곡선 추가하여 비교

auc(response = real, predictor = prob0)

####최적모형 탐색을 위한 튜님
#grid 생성
grid <- expand.grid(ntree = c(300, 500, 700, 1000),
                    mtry = 3:7,
                    error = NA)
print(grid)

#반복문 실행
n <- nrow(x=grid)

for(i in 1:n){
  ntree <- grid$ntree[i]
  mtry <- grid$mtry[i]
  disp <- str_glue('현재 {i}행 실행중! [ntree: {ntree}, mtry: {mtry}]')
  cat(disp, '\n\n')
  
  set.seed(seed = 1234)
  fit <- randomForest(formula = PersonalLoan~.,
                      data = trainSet,
                      ntree = ntree,
                      mtry= mtry)
  grid$error[i] <- tail(x = fit$err.rate[,1], n = 1)
}

#튜닝결과 시각화
plot(x = grid$error, type = 'b', pch = 19, col = 'red',
         main = 'grid search result')

abline(h = min(grid$error), col = 'red', lty = 2) #오차 최소값을 수평선으로 추가

#최적의 파라미터 설정
loc <- which.min(x = grid$error) #오차가 최소인 행번호 확인
print(x = loc)

bestPara<- grid[loc,]  # 오차가 최소일 떄의 하이퍼 파라미터 조합 설정
print(x = bestPara)

#최적 분류모형의 적합
best <- randomForest(formula = PersonalLoan~.,
                    data = trainSet,
                    ntree = bestPara$ntree,
                    mtry= bestPara$mtry,
                    importance = TRUE)
print(x = best)
print(x = fit1)

#최적모형 적합 결과 확인
plot(x = best) #OOB 오차 시각화
importance(x = best, type = 1) # 변수 중요도 값
varImpPlot(x= best, type = 1) #시각화

#중요 변수 별로 상자그림 그려보기
#Income
boxplot(formula = Income ~ PersonalLoan, data = trainSet) 
avg <- trainSet %>% 
        group_by(PersonalLoan) %>% 
        summarise(m = mean(x = Income))

points(formula = m ~ PersonalLoan, 
       data = avg, 
       pch = 19, 
       col = 'red', 
       cex = 1.2)

#Education
boxplot(formula = Education ~ PersonalLoan, data = trainSet)  #에러가 난다. 이유는? Education이 'factor'이기 떄문
avg <- trainSet %>% 
  group_by(PersonalLoan) %>% 
  summarise(m = mean(x = Education))

points(formula = m ~ PersonalLoan, 
       data = avg, 
       pch = 19, 
       col = 'red', 
       cex = 1.2)

#범주형에 대한 사분위수 확인? 크로스 테이블
install.packages('gmodels')
library(gmodels)
CrossTable(x = trainSet$Education, y = trainSet$PersonalLoan)
##결과 해석, 인사이트도출 예시: 강의듣고 다시 적어보기 .... 


#최적 모형의 성능 평가
pred2 <- predict(object = best, newdata = testSet, type = 'response') #시험셋으로 목표변수 추정값 생성
confusionMatrix(data = pred2, reference = real, positive = '1') #혼동행렬 출력
confusionMatrix(data = pred1, reference = real, positive = '1') # 훈련셋 추정값과 비교 

F1_Score(y_true = real, y_pred = pred1, positive = '1')
F1_Score(y_true = real, y_pred = pred2, positive = '1') #F1 점수

prob2 <- predict(object = best, newdata = testSet, type = 'prob')[, 2] #시험셋으로 목표변수 추정확률 생성

#ROC곡선 추가하기 
roc(response = real, predictor = prob1) %>% 
  plot(main = 'ROC Curve', col = 'red', lty = 1)  #훈련셋

roc(response = real, predictor = prob0) %>% 
  plot(col = 'blue', lty = 2, add = TRUE)         #의사결정나무 

roc(response = real, predictor = prob2) %>% 
  plot(col = 'purple', lwd = 2, add = TRUE)       #시험셋

#AUC 출력
auc(response = real, predictor = prob1)
auc(response = real, predictor = prob2)   #모형에 적합하기 위해 가공된 데이터기 때문에 차이가 없다, 실제 데이터에서는 성능이 훨씬 좋아짐. 

#최적 분류모형 저장
saveRDS(object = best, file = 'RandomForest.RDS')

# 시험셋으로 추정라벨 뽑은거 
print(x = pred2)