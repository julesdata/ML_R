6.회귀나무

url<-'https://bit.ly/median_house_value'
guess_encoding(file=url)
df<-read.csv(file=url)
str(object=df)
head(x=df,n=10L)
summary(object=df)

#목표변수 히스토그램그리기 
hist(x=df$MedianHouseValue,
     col='white')

df<-df%>%
  filter(MedianHouseValue<500000)

#분석데이터 분할

n<-nrow(x=df)
set.seed(seed=1234)
index<-sample(x=n,size=n*0.3,replace=FALSE)
trainSet<-df%>%slice(index)
testSet<-df%>%slice(-index)
mean(x=trainSet$MedianHouseValue)
mean(x=testSet$MedianHouseValue)


#정지규칙
library(rpart)
ctrl <- rpart.control(minsplit = 20, 
                      cp = 0.01, 
                      maxdepth = 10)
set.seed(seed = 1234)
fit1 <- rpart(formula = MedianHouseValue ~ ., 
              data = trainSet, 
              control = ctrl)
printcp(x = fit1)
plotcp(x = fit1)

ctrl <- rpart.control(minsplit = 10, 
                      cp = 0.001, 
                      maxdepth = 30)
set.seed(seed = 1234)
fit1 <- rpart(formula = MedianHouseValue ~ ., 
              data = trainSet, 
              control = ctrl)
printcp(x = fit1)
plotcp(x = fit1)

str(object = fit1)

# xerror만 벡터로 선택
fit1$cptable[, 4]

# 최소값의 위치 확인
which.min(x = fit1$cptable[, 4])

# cp만 벡터로 선택
cp <- fit1$cptable[76, 1]
print(x = cp)

# 가지치기
fit2 <- prune.rpart(tree = fit1, cp = cp)


real <- testSet$MedianHouseValue
pred1 <- predict(object = fit1, newdata = testSet, type = 'vector')
pred2 <- predict(object = fit2, newdata = testSet, type = 'vector')

library(MLmetrics)
MSE(y_pred = pred1, y_true = real)
RMSE(y_pred = pred1, y_true = real)
MAE(y_pred = pred1, y_true = real)
MAPE(y_pred = pred1, y_true = real)

MSE(y_pred = pred2, y_true = real)
RMSE(y_pred = pred2, y_true = real)
MAE(y_pred = pred2, y_true = real)
MAPE(y_pred = pred2, y_true = real)

#회귀모형의 성능 평가하는 사용자 정의 함수 생성
regMeasure <- function(real, pred) {
  library(MLmetrics)
  result <- data.frame(
    RMSE = RMSE(y_pred = pred, y_true = real),
    MSE = MSE(y_pred = pred, y_true = real),
    MAE = MAE(y_pred = pred, y_true = real),
    MAPE = MAPE(y_pred = pred, y_true = real)
  )
  return(result)
}

regMeasure(real = real, pred = pred1)
regMeasure(real = real, pred = pred2)

