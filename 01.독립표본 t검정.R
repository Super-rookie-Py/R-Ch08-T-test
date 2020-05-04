###  Ch08.t-검정2

###  2020/05/04 keonwoo park

## 01.독립표본 t-검정###############

# 그룹간의 차이 분석을 할 때 사용.
# 타이어를 교체할 때 A타이어와 B타이어
# 중 어느 타이어를 골라야 하는가.

# 귀무가설 H0: 차이의 평균 = 0 (차이가없다.)

# 검정통계량 등분산일경우 공통분산사용.
# 이분산인 경우 자유도 지정.

# 01. 데이터 불러오기.
ist<- read.csv("Ch0801.IST.csv",
               header = T,
               na.strings = '.')
ist$t_group <- factor(ist$t_group,
                      levels = c(1,2),
                      labels = c("A자동차","B자동차"))

str(ist)


## 02.기본 통계량 확인: describe(psych패키지 이용)
library(psych)
describeBy(ist$t_time, ist$t_group, mat=F) #두 그룹일때 보는거.


# 03.그래프그리기(박스그래프, 히스토그램)
opar <- par(no.readonly =TRUE) #디폴트 par 값을 미리 할당--> par(opar)

par(mfrow=c(1,2)) #화면 분할 행, 열로 화면을 쪼갠다.
layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))
boxplot(ist$t_time ~ ist$t_group)
hist(ist$t_time[ist$t_group == 'A자동차'])
hist(ist$t_time[ist$t_group == 'B자동차'])     
par(opar) #분할 복구


## 04.통계분석
# 등분산 검정(같은 분산을 따를 때)
var.test(ist$t_time ~ ist$t_group, data = ist)
# 등분산의 귀무가설 분산이 같다에 대한 검정.
# p-value 0.4192 -> 귀무가설 기각못함.(등분산이다)

# t-검정
# 등분산이면 var.equal = TRUE, 이분산이면 var.equal=FALSE
options("scipen" = 20) # 지수 표기법 수정: 2.2e-4 = 0.00022 소수점20자리까지는 보여줘라
t.test(ist$t_time~ ist$t_group,
       data = ist,
       alternative= c("two.sided"), #양측검정
       var.equal = TRUE, #등분산
       conf.level = 0.95) # 신뢰도 설정


# 05.통계결과 그래프

# A자동차 회사
x = 48670.57
se=658.5 # 표본이므로 표준편차sd 대신 표준오차 se사용
data <- rnorm(1000, x, se)
data <- sort(data) 
plot(data,
     dnorm(data, x, se),
     col = 'blue',
     type = 'l',
     main = '자동차 회사별 타이어 수명',
     xlim=c(45000,55000),
     ylim=c(0,0.0006))
abline(v=x, col='blue', lty=3) #수직라인 초록색평균

# 그래프를 곂쳐서 표현하기
par(new=T) #x값, y값 경계를 같게 지정해야한다. xlim, ylim 두그래프 같게

# B자동차 회사
x = 51377.6
se=766.37 # 표본이므로 표준편차sd 대신 표준오차 se사용
data <- rnorm(1000, x, se)
data <- sort(data) 
plot(data,
     dnorm(data, x, se),
     col = 'red',
     type = 'l',
     main = '자동차 회사별 타이어 수명',
     xlim=c(45000,55000),
     ylim=c(0,0.0006))
abline(v=x, col='blue', lty=3) #수직라인 초록색평균



