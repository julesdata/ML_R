#4. k-nearest neighbors k-최근접이웃

library(tidyverse)
getwd()
setwd(dir='./data')
list.files()
cust <- readRDS(file="Dataset for Cust.RDS")
str(object=cust)

dist( x= cust)

scaled <- scale(x=cust)
summary (object = scaled)
apply(X = scaled, MARGIN = 2, FUN = sd) #표준편차보기..?

dist(x = scaled)

set.seed(seed = 1234)
heights <- rnorm(n = 10000, mean = 172.4, sd = 5.7)
scaled1 <- scale(x = heights)
mean(x = scaled1)
sd(x = scaled1)
range(scaled1)

Min <- min(heights)
Max<- max(heights)
scaled2 <- scale(x=heights, center=Min, scale= Max-Min)
mean(x=scaled2)
sd(x = scaled2)
range(scaled2)


#wine data
#데이터 불러와서 확인하기
url <- 'https://bit.ly/white_wine_quality'
guess_encoding(file = url)
df <- read.csv(file = url, sep = ';') #데이터 구분자. csv파일은 쉼표로, 인터넷에 있는 파일은 주로 세미콜론으로 되어있다.
str(object = df)
head(x = df, n= 10)
summary(object = df)

#목표변수 탐색하기
tbl <- table(df$quality) #table:빈도수계산 함수
print(x= tbl)
tbl %>% prop.table() %>% cumsum() %>% round(digits=4L) * 100     #cumsum: 누적합 계산 함수

#막대그래프로 분포확인                                                                 #prop.table: 비율 계산 함수
bp <- barplot(height= tbl,     
              ylim = c(0,2400),   #y축 높이 (텍스트가 가려지지 않도록)
              xlab = 'Quality Score',  #x축 이름
              main = 'White Wine Quality') # 표 제목

text추가


