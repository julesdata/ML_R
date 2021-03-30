5. 의사결정나무

#1. 데이터 불러오기
library(tidyverse)
url <-'https://bit.ly/universal_bank'
guess_encoding(file = url)
df <- read.csv(file = url)

#2. 데이터 확인하기
str(object = df)
summary(object= df)
head(x=df, n= 10L)
###'L'을 붙히는 이유
#n에 명확하게 정수 (실수가 아닌, 정수)를 입력한다는 의미로..
  class(10)
  class(10L)
#3. 필요 없는 열 삭제하기  
df<- df %>% select(-ID,-ZIP.Code) %>% filter(Experience >0)

#4. 데이터타입 변환하기  > map_df: apply 와 비슷한 역할. (.x)= 벡터, (.f)=function
cols <- c(6, 8:12)
df[,cols] <-map_df(.x = df[,cols], .f= as.factor)
summary(object = df)


#5. 목표변수 확인하기(분포 비중 확인)
df$PersonalLoan %>% table() %>% prop.table() %>% round(digits = 4L) * 100

#6. 분석데이터셋분할(훈련셋 70%, 테스트셋 30%)
n <- nrow(x=df)
set.seed(seed = 1234)
index <- sample(x=n, size = n*0.7, replace = FALSE)  #true/false는 대문자로 써줘야함!!
trainSet<-df%>%slice(index)
testSet<-df%>%slice(-index)
trainSet$PersonalLoan%>%table()%>%prop.table()*100
testSet$PersonalLoan%>%table()%>%prop.table()*100

#7. RDA파일로 저장하기

#8. 의사결정 함수 적합하기
> 패키지 설치 및 함수 호출
install.packages('rpart')
library(rpart)

>정지규칙 설정
ctrl <- rpart.control(minsplit = 20,  #각 마디에 속하는 최소 관측값의 수
                      cp = 0.01,      # 비용 복잡도 파라미터
                      maxdepth = 10)  # 최대 나무의 깊이
set.seed(seed = 1234)  # 교차검증을 자동으로 실행하므로, 재현 간으한 결과를 생성하려면 'seed'를 지정해주어야 함. 
fit1 <- rpart(formula = PersonalLoan~., data = trainSet, control = ctrl)
summary(object = fit1)

#9. 가지치기 필요여부 확인 (비용복잡도 테이블)
printcp(x=fit1) #숫자확인
plotcp(x=fit1)  #그래프로 확인  >할필요 없는 결과가 나옴


#10. 가지치기 실습 위해 정지규칙 변경

ctrl <- rpart.control(minsplit = 10,  #20 ->10
                      cp = 0.001,      # 0.01->0.001
                      maxdepth = 30)  # 10->30
set.seed(seed = 1234)  # 교차검증을 자동으로 실행하므로, 재현 간으한 결과를 생성하려면 'seed'를 지정해주어야 함. 
fit1 <- rpart(formula = PersonalLoan~., 
              data = trainSet, 
              control = ctrl)

printcp(x=fit1)
plotcp(x=fit1)

#11. 가지치기
fit2 <- prune.rpart(tree = fit1, cp = 0.0098)

printcp(x = fit2)
plotcp(x= fit2)

#12. 나무모형 시각화하기

>rpart.plot으로 시각화하기
install.packages('rpart.plot')
library(rpart.plot)
rpart.plot(x = fit1,
           type = 2,
           extra = 101,
           fallen.leaves = FALSE)

>rattle 패키지: 칼라 변형 등 좀더 세련된 시각화가 가능하다고한다....인터넷에 찾아보면 거의 이거로 하는듯. 
install.packages('rattle')
library(rattle)
fancyRpartPlot(model = fit1)


#13. 목표변수 추정값 생성 (가지치기 전 vs 후)
real <- testSet$PersonalLoan
pred1 <- predict(object = fit1, newdata = testSet, type = 'class')
pred2 <- predict(object = fit2, newdata = testSet, type = 'class')

table(pred1, real)
table(pred2, real)

#14. 분류모형 성능평가

##혼동행렬

install.packages('caret')
library(caret)
install.packages('e1071')
library('e1071')

confusionMatrix(data = pred1, reference = real, positive = '1')
confusionMatrix(data = pred2, reference = real, positive = '1')

##F1점수

install.packages('MLmetrics')
library(MLmetrics)
F1_Score(y_true = real, y_pred = pred1, positive = '1')
F1_Score(y_true = real, y_pred = pred2, positive = '1')

##목표변수 추정확률 생성
prob1 <- predict(object = fit1, newdata = testSet, type = 'prob')[, 2]
prob2 <- predict(object = fit2, newdata = testSet, type = 'prob')[, 2]

##ROC곡선 그리기
install.packages('pROC')
library(pROC)
roc(response = real, predictor = prob1) %>% 
  plot(main = 'ROC Curve', col = 'red', lty = 1)

roc(response = real, predictor = prob2) %>% 
  plot(col = 'blue', lty = 2, add = TRUE)    #가지치기 후 모형 추가

##AUC출력
auc(response = real, predictor = prob1)
auc(response = real, predictor = prob2)


