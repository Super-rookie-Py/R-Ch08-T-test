### Ch08.t-검정2

### 2020/05/04 keonwoo park

## 02.대응표본 t-검정###############

# k제약에서 다이어트약이 효과가 있는지 없는지를 분석

# 새로운 약은 효과가 있는가?

# 귀무가설H0: 차이의 평균 = 0 전과 후의 체중은 변화가 없다.
# 대립가설H1: 차이가 있다.
# 등분산 검정 필요없음.



# 01. 데이터 불러오기.
pst<- read.csv("Ch0802.PST.csv",
               header = T,
               na.strings = '.')

str(pst)


## 02.기본 통계량 확인: describe(psych패키지 이용)
library(psych)
describe(pst)
dif <- c(pst$post-pst$pre) # 몸무게 차이
describe(dif)


# 03.그래프그리기(박스그래프, 히스토그램)

opar <- par(no.readonly =TRUE) #디폴트 par 값을 미리 할당--> par(opar)

layout(matrix(c(1,2,3,3), 2, 2, byrow = TRUE))
hist(pst$pre, main='사전 몸무게')
hist(pst$pre, main='사후 몸무게')
boxplot(dif, main='몸무게 차이')
par(opar) #분할 복구


## 04.통계분석

# t-검정
options("scipen" = 20) # 지수 표기법 수정: 2.2e-4 = 0.00022 소수점20자리까지는 보여줘라
t.test(pst$post, pst$pre,
       alternative= c("two.sided"), #양측검정
       paired = TRUE,
       conf.level = 0.95) # 신뢰도 설정


# 05.통계결과 그래프

# A자동차 회사
mu = 0
se= 0.7 # 표본이므로 표준편차sd 대신 표준오차 se사용
inter = qt(p=0.025, df=19)
data <- rnorm(1000, mu, se)
data <- sort(data) 
plot(data,
     dnorm(data, mu, se),
     type = 'l',
     main = '몸무게 차이 검정',
     xlim=c(-3, 3))
abline(v=mu, col='green', lty=3) #수직라인 초록색평균
abline(v=mu+inter*se,col='blue',lty=5) #신뢰구간 상한
abline(v=mu-inter*se,col='blue',lty=5) #신뢰구간 하한
abline(v=-2.55, col='red', lty=5) #표본의 평균

