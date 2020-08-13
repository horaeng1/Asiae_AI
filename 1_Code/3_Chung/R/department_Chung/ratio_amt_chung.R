str(data$brd_nm)
str(data_m$brd_nm)
str(data_f$brd_nm)
data_brd <- list(data$brd_nm)
install.packages(c('dplyr','psych','Hmisc','skimr'))
install.packages('fBasics')
library(dplyr)
library(psych)
library(Hmisc)
library(skimr)
library(fBasics)

Sys.setlocale("LC_ALL","korean")#os가 한글이 아닐시에 꼭 써야함

x_data <- read.csv("C:/Users/seokm/OneDrive/Documents/project_data/X_train.csv",header = TRUE, sep = ',', stringsAsFactors = FALSE,encoding = "CP949")
y_data <- read.csv("C:/Users/seokm/OneDrive/Documents/project_data/y_train.csv",header = TRUE, sep = ',',stringsAsFactors = FALSE,encoding = "CP949")

data <- merge(x = y_data, y = x_data, by = 'custid')

unique(data$brd_nm)
#1882
unique(data$goodcd)
#997+9427
unique(data$corner_nm)
#309
unique(data$pc_nm)
#77
unique(data$team_nm)
#4

###비율형 척도 변수컬럼 특성파악

#매출 기술 통계량
summary(data$tot_amt)
summary(data$dis_amt)
summary(data$net_amt)

library(Hmisc)
Hmisc::describe(data$tot_amt)
Hmisc::describe(data$dis_amt)
Hmisc::describe(data$net_amt)

#산술평균
mean(data$tot_amt)
mean(data$dis_amt)
mean(data$net_amt)

#중앙값
median(data$tot_amt)
median(data$dis_amt)
median(data$net_amt)

ctg_names <- c('custid','gender','sales_date','sales_time','str_nm','goodcd','brd_nm','corner_nm','pc_nm','part_nm','team_nm','buyer_nm','import_flg','tot_amt','dis_amt','net_amt','inst_mon','inst_fee')
ctg_names
ctg<-data[ctg_names]
ctg


#최빈값
tot_amt_freq <-table(ctg$tot_amt)
tot_amt_freq
sort(tot_amt_freq, decreasing = TRUE)
mean(tot_amt_freq)#11.76758
names(which.max(tot_amt_freq))#20000

dis_amt_freq <- table(ctg$dis_amt)
dis_amt_freq
sort(dis_amt_freq, decreasing = TRUE)
mean(dis_amt_freq)#124.6876
names(which.max(dis_amt_freq))#0


net_amt_freq <- table(ctg$net_amt)
net_amt_freq
sort(net_amt_freq, decreasing = TRUE)
mean(net_amt_freq)#10.85364
names(which.max(net_amt_freq))#20000



###비율형 척도 변수컬럼 요약집계

#분산(variance)
var(ctg$tot_amt)
var(ctg$dis_amt)
var(ctg$net_amt)

#표준편차(std)
sd(ctg$tot_amt)
sd(ctg$dis_amt)
sd(ctg$net_amt)

#범위 (range)
range(ctg$tot_amt)
range(ctg$dis_amt)
range(ctg$net_amt)

#최대&최소 (max&min)
max(ctg$tot_amt)
max(ctg$dis_amt)
max(ctg$net_amt)

min(ctg$tot_amt)
min(ctg$dis_amt)
min(ctg$net_amt)

#왜도 (skewness)
skewness(ctg$tot_amt)
skewness(ctg$dis_amt)
skewness(ctg$net_amt)

#첨도(kurtosis)
kurtosis(ctg$tot_amt)
kurtosis(ctg$dis_amt)
kurtosis(ctg$net_amt)


###비율형척도 변수컬럼 시각화

#simple plot
par("mar")

par(mar=c(1,1,1,1))

par(mfrow = c(2,2))

plot(ctg$tot_amt, type ='p',pch=21, bg='blue')
plot(ctg$dis_amt, type ='p',pch=21, bg='red')
plot(ctg$net_amt, type ='p',pch=21, bg='magenta')

#히스토그램과 확률 밀도 곡선
par(mfrow=c(2,2))
hist(ctg$tot_amt,main='hist(), Frequency 옵션')
hist(ctg$tot_amt, probability=TRUE, main='hist(), Porbability 옵션')
plot(density(ctg$tot_amt),main='density() 확률밀도 옵션')
hist(ctg$tot_amt,probability=TRUE, main = 'hist() 히스토그램과 density() 확률밀도 함수 통합')
lines(density(ctg$tot_amt))

#박스플롯
par(mfrow=c(1,1))
boxplot(ctg$tot_amt,main='박스플롯', ylab ='지출경비')


#비율척도 변수간 특성 분석

# amt 변수 기본 특성 파악
str(ctg$tot_amt)
str(ctg$dis_amt)
str(ctg$net_amt)

#상관성 분석
cor(ctg$tot_amt, ctg$net_amt, method = 'spearman')
cor(ctg$tot_amt, ctg$net_amt, method = 'pearson')

###비율척도 변수간 시각화

#기본 graphics::plot() 함수이용:직선과 곡선 최적선 추가(아마 디멘션 추가 필요 )
par(mfrow=c(1,1))
plot(tot_amt~net_amt, data = ctg, pch=19,
     main = '총매출과 실매출 관련성',
     xlab = '실매출',
     ylab = '총매출')

#최적의 추세직선(optimum trendline)추가
abline(lm(tot_amt~net_amt, data = ctg),col = 'blue', lwd = 2, lty =2)

##최적의 추세곡선(optimum curve)추가
lines(lowess(ctg$tot_amt ~ ctg$net_amt),
      col=blue, lwd=2 ,lty=2)



