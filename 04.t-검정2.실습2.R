### Ch08.t-검정2

### 2020/05/04 keonwoo park

## 04.t-검정2 실습 2###############

# 알파파와 베타파 간에는 차이가 있을까 




# 01. 데이터 불러오기.
breath <- read.csv("Ch0804.호흡과 뇌파.csv",
               header = T,
               na.strings = '.')

str(breath)


## 02.기본 통계량 확인: describe(psych패키지 이용)
library(psych)
describe(breath)
dif <- c(breath$ch1be-breath$ch1al) # 몸무게 차이
describe(dif)


# 03.그래프그리기(박스그래프, 히스토그램)

opar <- par(no.readonly =TRUE) #디폴트 par 값을 미리 할당--> par(opar)

layout(matrix(c(1,2,3,3), 2, 2, byrow = TRUE))
hist(breath$ch1al, main='ch1al')
hist(breath$ch1be, main='ch1be')
boxplot(dif, main='뇌파차이')
par(opar) #분할 복구


## 04.통계분석

# t-검정
options("scipen" = 20) # 지수 표기법 수정: 2.2e-4 = 0.00022 소수점20자리까지는 보여줘라
t.test(breath$ch1be, breath$ch1al,
       alternative= c("two.sided"), #양측검정
       paired = TRUE,
       conf.level = 0.95) # 신뢰도 설정


# 05.통계결과 그래프

# A자동차 회사
mu = 0
se= 0.05/sqrt(143) # 표본이므로 표준편차sd 대신 표준오차 se사용
inter = qt(p=0.025, df=143)
data <- rnorm(1000, mu, se)
data <- sort(data) 
plot(data,
     dnorm(data, mu, se),
     type = 'l',
     main = '뇌파 차이 검정',
     xlim=c(-0.09, 0.03))
abline(v=mu, col='green', lty=3) #수직라인 초록색평균
abline(v=mu+inter*se,col='blue',lty=5) #신뢰구간 상한
abline(v=mu-inter*se,col='blue',lty=5) #신뢰구간 하한
abline(v=-0.08, col='red', lty=5) #표본의 평균

