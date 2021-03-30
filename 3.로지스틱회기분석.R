#데이터 불러오기
library(tidyverse)
url <- 'https://bit.ly/university_admit'

#인코딩
guess_encoding(file = url)
#guess_encoding(file = 'https://www.naver.com')
#guess_encoding(file = 'https://finance.naver.com')

df <- read.csv(file = url)

#각 컬럼의 데이터타입이 잘 들어와있는지 확인하기
#컬럼별 벡터 자료형을 확인하기 위함.
str(object = df)
head(x=df, n=10L)

#데이터 타입 변환하기
#한줄씩: df$admit<-as.factor(df$admit)
#한꺼번에 바꾸기
vars <-c('admit', 'rank')
df[vars] <-map_df(.x = df[vars], .f= as.factor)


#컬럼별 기술통계량 빠르게 확인하기 
summary(df)

#목표변수 비율 확인하기 
#prop.table = 벡터간의 비율
#한번에 쓰면:prop.table(x= table(df$admit)) * 100

#이해 쉽게, 파이프문으로 쓰면?
#파이프문 단축키: shift +ctrl +m
df$admit %>% table() %>% prop.table() * 100

# 각 입력변수 내에서 차이가 있는지 그림으로 확인->박스플랏
# admit 여부에, GRE점수 차이가 있을까?
boxplot(formula = gre ~ admit, 
        data = df, 
        col = 'white', 
        pch = 19, 
        outcol = 'red')

avg <- df %>% group_by(admit) %>% summarise(m = mean(x = gre))

points(formula = m ~ admit,     #평균 표시
       data = avg, 
       pch = 19, 
       col = 'blue', 
       cex = 1.2)

#gpa 차이는 있을까?
boxplot(formula = gpa ~ admit, 
        data = df, 
        col = 'white', 
        pch = 19, 
        outcol = 'red')

avg <- df %>% group_by(admit) %>% summarise(m = mean(x = gpa))

points(formula = m ~ admit, 
       data = avg, 
       pch = 19, 
       col = 'blue', 
       cex = 1.2)

#입력변수의 정규성 검정 (T test위해.)
by(data = df$gpa, INDICES = df$admit, FUN = shapiro.test)

# 정규분포를 따르지 않을때는, wicox test 실행
wilcox.test(formula = gpa ~ admit, data = df)
#> true location shift is not equal to 0 : 두 집단의 이동이 0이 아니다. 다르게 움직인다 = 차이가 있다. 

#범주형 입력변수의 검정 (카이제곱검정)
install.packages('gmodels')
library(gmodels)

CrossTable(x = df$rank, y = df$admit)    #이거 어떤표인지 다시 복습하기...
chisq.test(x = df$rank, y = df$admit)
#결과: rank와 admit이 독립이 아니다. rank와 admit은 관계가 있다.

#훈련셋, 시험셋 나누기

n <- nrow(x=df)
set.seed(seed=1234)
index <- sample(x=n, size=n*0.7, replace=FALSE)
trainSet <- df %>% slice(index)
testSet <- df %>% slice(-index)
trainSet$admit %>% table() %>% prop.table() *100
testSet$admit %>% table() %>% prop.table() *100


#회귀모형 적합도
fit1 <-glm(formula = admit ~.,
           data = trainSet,
           family = binomial(link = 'logit'))
summary(object = fit1)



강의내용복습.....



#ROC곡선 그리기
library(MLmetrics)
F1_Score(y_true = real, y_pred = pred1, positive = '1')

library(pROC)
roc(response = real, predictor = prob1) %>% 
  plot(main = 'ROC Curve', col = 'red', lty = 1)

#p의 확률을넣어야지, p의 label을 넣으면 안된다. 라벨로 넣었을떄는 어떻게 되는지 파란 점선으로 추가해보면..?
roc(response = real, predictor = as.numeric(x = pred1)) %>% 
  plot(col = 'blue', lty = 2, lwd = 2, add = TRUE)

#모형의 성능은 멍텅구리를 겨우 모면한 수준이나....로지스틱은 
해설을위한 모형이지 고성능 및 예측을 위한 모형이 아니므로 그냥 수준파악정도로 본다.

#auc 구하기

