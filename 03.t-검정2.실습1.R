### Ch08.t-검정2

### 2020/05/04 keonwoo park

## 03.t-검정2 실습 1###############

# 재학생과 교원을 대상으로 교육과정에대한 현황을 실시하였다.
# 교육과정, 강의 및 학습, 행정서비스, 교육환경 및 시설, 종합점수 
# 재학생과 교원이 차이가 있는가?


# 01. 데이터 불러오기.
edu<- read.csv("Ch0803.edu.csv",
               header = T,
               na.strings = '.')

edu$구분 <- factor(edu$구분,
                 levels=c(1,2),
                 labels=c('재학생','교원'))


## 02.기본 통계량 확인: describe(psych패키지 이용)
library(psych)
describeBy(edu$종합, edu$구분, mat=T)


# 03.그래프그리기(박스그래프, 히스토그램)

opar <- par(no.readonly =TRUE) #디폴트 par 값을 미리 할당--> par(opar)

  layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))
  boxplot(edu$종합 ~ edu$구분)
  hist(edu$종합[edu$구분=='재학생'])
  hist(edu$종합[edu$구분=='교원'])

par(opar) #분할 복구


## 04.통계분석

# t-검정
options("scipen" = 20) # 지수 표기법 수정: 2.2e-4 = 0.00022 소수점20자리까지는 보여줘라
t.test(pst$post, pst$pre,
       alternative= c("two.sided"), #양측검정
       paired = TRUE,
       conf.level = 0.95) # 신뢰도 설정


## 04.통계분석
# 등분산 검정(같은 분산을 따를 때)
var.test(edu$종합 ~ edu$구분, data = edu)

# t-검정
# 등분산이면 var.equal = TRUE, 이분산이면 var.equal=FALSE
t.test(edu$종합~edu$구분,
       data = edu,
       alternative= c("greater"), #단측검정
       var.equal = TRUE, #등분산
       conf.level = 0.95) # 신뢰도 설정


# 05.통계결과 그래프

# 재학생
x = 61.76
se=2.803068 # 표본이므로 표준편차sd 대신 표준오차 se사용
data <- rnorm(1000, x, se)
data <- sort(data) 
plot(data,
     dnorm(data, x, se),
     col = 'blue',
     type = 'l',
     main = '재학생 만족도',
     xlim=c(55,80),
     ylim=c(0,0.20))
abline(v=x, col='blue', lty=3) #수직라인 초록색평균

# 그래프를 곂쳐서 표현하기
par(new=T) #x값, y값 경계를 같게 지정해야한다. xlim, ylim 두그래프 같게

# 교원의 만족도
x = 74.72
se= 2.337317 # 표본이므로 표준편차sd 대신 표준오차 se사용
data <- rnorm(1000, x, se)
data <- sort(data) 
plot(data,
     dnorm(data, x, se),
     col = 'red',
     type = 'l',
     main = '교원의 만족도',
     xlim=c(55,80),
     ylim=c(0,0.20))
abline(v=x, col='blue', lty=3) #수직라인 초록색평균

par(opar)
