d_data <- read.csv('C:/KK/1st_X_train.csv')
class(d_data)
str(d_data)
colnames(d_data)
summary(d_data)
d_data
c_data <- d_data[1:9999,]

k <- as.POSIXlt(c_data$sales_date) 
dc_rate <- round(c_data$dis_amt/c_data$tot_amt,2)*100
stime <- c_data$sales_time%/%100
sale <- c_data$net_amt
k$year
k$mon      # 월 (0 = January)
k$mday     # 일
k$wday     # 요일
stime     # 시간 
max(k)

kk <- data.frame(year=k$year+1900, mon=k$mon+1, day=k$mday, hour=stime,
                 wday=k$wday, sale=sale, dc_rate=dc_rate)

allk <- as.POSIXlt(d_data$sales_date) 
alldc_rate <- round(d_data$dis_amt/d_data$tot_amt,2)*100
allstime <- d_data$sales_time%/%100
allsale <- d_data$net_amt

kkk <- data.frame(year=allk$year+1900, mon=allk$mon+1, day=allk$mday, hour=allstime,
                 wday=allk$wday, sale=allsale, dc_rate=alldc_rate)

summary(kk)
class(kk)
str(kk)

install.packages("psych")
install.packages("Hmisc")
install.packages("skimr")
install.packages("PerformanceAnalytics")
install.packages("corrplot")

library(psych)
library(Hmisc)
library(skimr)
library(PerformanceAnalytics)

-------------------------------------------------------------------------
  
#변수간 상관관계 매트릭스

round(cor(kk), 3)

#범주형 변수간 산점도

plot(kk)

plot(kk$hour, kk$sale)

plot(kk$wday, kk$sale)

plot(kk$hour, kk$sale)

cor(kk$hour, kk$sale, method='pearson')

cor(kk, method='pearson')#순위

pairs.panels(kk,
             method = "spearman",  #선형
             hist.col = "blue",
             density = TRUE,
             ellipses = TRUE
             )


chart.Correlation(kk, histogram = T)

-------------------------------------------------------------------------

# 변수 기술통계량

psych::describe(kk$hour) 
Hmisc::describe(kk$hour)
skim(kk$hour)


#1 판매건수


# 다항-시간

  ## 빈도
object_freq <- table(kk$hour)
object_freq

obj_df <- as.data.frame(object_freq)
obj_df
View(obj_df)

  ## 비율
object_prop <- prop.table(object_freq)
round(object_prop,3)

obj_prop_df <- as.data.frame(object_freq)
obj_prop_df
View(obj_prop_df)

  ## 백분율
object_pect <- round(object_prop, 3) * 100
object_pect

obj_pect_df <- as.data.frame(object_pect)
obj_pect_df
View(obj_pect_df)

# 다항형 명목척도 변수컬럼 시각화

  ## barplot() 범주형 데이터 그래프

par(mfrow=c(2,2)) #멀티 캔버스 프레임 2X2

barplot(object_freq,
        main="시간별 판매건수 분포 비교",
        xlab="시간대", ylab="판매건수")

barplot(object_freq,
        main="시간별 판매건수 분포 비교",
        xlab="시간대", ylab="판매건수", las=1, horiz=TRUE)

barplot(object_freq,
        main="시간별 판매건수 분포 비교",
        xlab="시간대", ylab="판매건수", density=c(20,30))

barplot(object_freq,
        main="시간별 판매건수 분포 비교",
        xlab="시간대", ylab="판매건수", las=1,
        horiz=TRUE, col=c("light blue", "pink"),
        beside=TRUE, legend=rownames(object_freq))

par(mfrow=c(1,1)) #멀티 캔버스 프레임 리셋



# 다항-요일

## 빈도
object_freq <- table(kk$wday)
object_freq

obj_df <- as.data.frame(object_freq)
obj_df
View(obj_df)

## 비율
object_prop <- prop.table(object_freq)
round(object_prop,3)

obj_prop_df <- as.data.frame(object_freq)
obj_prop_df
View(obj_prop_df)

## 백분율
object_pect <- round(object_prop, 3) * 100
object_pect

obj_pect_df <- as.data.frame(object_pect)
obj_pect_df
View(obj_pect_df)

# 다항형 명목척도 변수컬럼 시각화

  ## barplot() 범주형 데이터 그래프

barplot(object_freq,
        main="요일별 판매건수 분포 비교",
        xlab="요일", ylab="판매건수")

barplot(object_freq,
        main="요일별 판매건수 분포 비교",
        xlab="요일", ylab="판매건수", las=1, horiz=TRUE)

barplot(object_freq,
        main="요일별 판매건수 분포 비교",
        xlab="요일", ylab="판매건수", density=c(20,30))

barplot(object_freq,
        main="요일별 판매건수 분포 비교",
        xlab="요일", ylab="판매건수", las=1,
        horiz=TRUE, col=c("light blue", "pink"),
        beside=TRUE, legend=rownames(object_freq))




# 다항-월별

## 빈도
object_freq <- table(kk$mon)
object_freq

obj_df <- as.data.frame(object_freq)
obj_df
View(obj_df)

## 비율
object_prop <- prop.table(object_freq)
round(object_prop,3)

obj_prop_df <- as.data.frame(object_freq)
obj_prop_df
View(obj_prop_df)

## 백분율
object_pect <- round(object_prop, 3) * 100
object_pect

obj_pect_df <- as.data.frame(object_pect)
obj_pect_df
View(obj_pect_df)

# 다항형 명목척도 변수컬럼 시각화

## barplot() 범주형 데이터 그래프

barplot(object_freq,
        main="월별 판매건수 분포 비교",
        xlab="월", ylab="판매건수")

barplot(object_freq,
        main="월별 판매건수 분포 비교",
        xlab="월", ylab="판매건수", las=1, horiz=TRUE)

barplot(object_freq,
        main="월별 판매건수 분포 비교",
        xlab="월", ylab="판매건수", density=c(20,30))

barplot(object_freq,
        main="월별 판매건수 분포 비교",
        xlab="월", ylab="판매건수", las=1,
        horiz=TRUE, col=c("light blue", "pink"),
        beside=TRUE, legend=rownames(object_freq))


# 다항-일별

## 빈도
object_freq <- table(kk$day)
object_freq

obj_df <- as.data.frame(object_freq)
obj_df
View(obj_df)

## 비율
object_prop <- prop.table(object_freq)
round(object_prop,3)

obj_prop_df <- as.data.frame(object_freq)
obj_prop_df
View(obj_prop_df)

## 백분율
object_pect <- round(object_prop, 3) * 100
object_pect

obj_pect_df <- as.data.frame(object_pect)
obj_pect_df
View(obj_pect_df)

# 다항형 명목척도 변수컬럼 시각화

## barplot() 범주형 데이터 그래프

barplot(object_freq,
        main="일별 판매건수 분포 비교",
        xlab="일", ylab="판매건수")

barplot(object_freq,
        main="일별 판매건수 분포 비교",
        xlab="일", ylab="판매건수", las=1, horiz=TRUE)

barplot(object_freq,
        main="일별 판매건수 분포 비교",
        xlab="일", ylab="판매건수", density=c(20,30))

barplot(object_freq,
        main="일별 판매건수 분포 비교",
        xlab="일", ylab="판매건수", las=1,
        horiz=TRUE, col=c("light blue", "pink"),
        beside=TRUE, legend=rownames(object_freq))



------------------------------------------------------------------------
# - `nrow()`: 행의 개수를 계산하는 함수  
# - `ncol()`: 열의 개수를 계산하는 함수  
  
    
#2 순매출
  ## 판매액<요일>


wday_sales1 <- data.frame(wday=0:6, wsales=c(0,0,0,0,0,0,0)) 
                           
kk$wday
  
for (i in kk$wday) {
  if (i==0) {
    wday_sales1$wsales[1] <- wday_sales1$wsales[1] + kk$sale
  }
  else if (i==1) {
    wday_sales1$wsales[2] <- wday_sales1$wsales[2] + kk$sale
  }
  else if (i==2) {
    wday_sales1$wsales[3] <- wday_sales1$wsales[3] + kk$sale
  }
  else if (i==3) {
    wday_sales1$wsales[4] <- wday_sales1$wsales[4] + kk$sale
  }
  else if (i==4) {
    wday_sales1$wsales[5] <- wday_sales1$wsales[5] + kk$sale
  }
  else if (i==5) {
    wday_sales1$wsales[6] <- wday_sales1$wsales[6] + kk$sale
  }
  else{
    wday_sales1$wsales[7] <- wday_sales1$wsales[7] + kk$sale
  }
}

obj_wd1 <- array(wday_sales1$wsales)

## barplot() 범주형 데이터 그래프

par(mfrow=c(2,2)) #멀티 캔버스 프레임 2X2

barplot(obj_wd1,
        main="요일별 판매액",
        xlab="요일", ylab="판매액")

barplot(obj_wd1,
        main="요일별 판매액",
        xlab="요일", ylab="판매액", las=1, horiz=TRUE)

barplot(obj_wd1,
        main="요일별 판매액",
        xlab="요일", ylab="판매액", density=c(20,30))

barplot(obj_wd1,
        main="요일별 판매액",
        xlab="요일", ylab="판매액", las=1,
        horiz=TRUE, col=c("light blue", "pink"),
        beside=TRUE, legend=rownames(object_freq))

par(mfrow=c(1,1)) #멀티 캔버스 프레임 리셋



  ## 판매액<시간>


hour_sales1 <- data.frame(hour=10:22, hsales=c(0,0,0,0,0,0,0,0,0,0,0,0,0)) 
hour_sales1
kk$hour

for (i in kkk$hour) {
  if (i==10) {
    hour_sales1$hsales[1] <- hour_sales1$hsales[1] + kkk$sale
  }
  else if (i==11) {
    hour_sales1$hsales[2] <- hour_sales1$hsales[2] + kkk$sale
  }
  else if (i==12) {
    hour_sales1$hsales[3] <- hour_sales1$hsales[3] + kkk$sale
  }
  else if (i==13) {
    hour_sales1$hsales[4] <- hour_sales1$hsales[4] + kkk$sale
  }
  else if (i==14) {
    hour_sales1$hsales[5] <- hour_sales1$hsales[5] + kkk$sale
  }
  else if (i==15) {
    hour_sales1$hsales[6] <- hour_sales1$hsales[6] + kkk$sale
  }
  else if (i==16) {
    hour_sales1$hsales[7] <- hour_sales1$hsales[7] + kkk$sale
  }
  else if (i==17) {
    hour_sales1$hsales[8] <- hour_sales1$hsales[8] + kkk$sale
  }
  else if (i==18) {
    hour_sales1$hsales[9] <- hour_sales1$hsales[9] + kkk$sale
  }
  else if (i==19) {
    hour_sales1$hsales[10] <- hour_sales1$hsales[10] + kkk$sale
  }
  else if (i==20) {
    hour_sales1$hsales[11] <- hour_sales1$hsales[11] + kkk$sale
  }
  else if (i==21) {
    hour_sales1$hsales[12] <- hour_sales1$hsales[12] + kkk$sale
  }
  else {
    hour_sales1$hsales[13] <- hour_sales1$hsales[13] + kkk$sale
  }
}

obj_hour1 <- array(hour_sales1$hsales)
obj_hour1

# 다항형 명목척도 변수컬럼 시각화

## barplot() 범주형 데이터 그래프

barplot(obj_hour1,
        main="시간별 판매액",
        xlab="시간", ylab="판매액")

barplot(obj_hour1,
        main="시간별 판매액",
        xlab="시간", ylab="판매액", las=1, horiz=TRUE)

barplot(obj_hour1,
        main="시간별 판매액",
        xlab="시간", ylab="판매액", density=c(20,30))

barplot(obj_hour1,
        main="시간별 판매액",
        xlab="시간", ylab="판매액", las=1,
        horiz=TRUE, col=c("light blue", "pink"),
        beside=TRUE, legend=rownames(object_freq)) # legend는 범례

------------------------------------------------------------------------------
