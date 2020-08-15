d_data <- read.csv('C:/KK/1st_X_train.csv')
class(d_data)
str(d_data)
colnames(d_data)
summary(d_data)
d_data
c_data <- d_data[1:9999,]

str(k)
str(c_data)
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

allk$wday

allk <- as.POSIXlt(d_data$sales_date) 
alldc_rate <- round(d_data$dis_amt/d_data$tot_amt,2)*100
allstime <- d_data$sales_time%/%100
allsale <- d_data$net_amt

kkk <- data.frame(year=allk$year+1900, mon=allk$mon+1, day=allk$mday, hour=allstime,
                 wday=allk$wday, sale=allsale, dc_rate=alldc_rate)

summary(kkk)
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
  
  kkk <- data.frame(year=allk$year+1900, mon=allk$mon+1, day=allk$mday, hour=allstime,
                    wday=allk$wday, sale=allsale, dc_rate=alldc_rate)

#3 기간별 할인순매출
## 할인율<요일>



wday_sales_dc <- data.frame(wday=0:6, wsales=c(0,0,0,0,0,0,0)) 


rate_grade <- data.frame(rate=c('0%','5%','10%'), wratesale=c(1,2,3))
rate_grade
str(rate_grade)
rate_grade[1,]$wratesale  # 할인X
rate_grade[2,]$wratesale  # 할인 5%
rate_grade[3,]$wratesale  # 할인 10%
str(kkk)

result <- table(kkk$wday, kkk$dc_rate)
rate_grade <- as.data.frame(result)
colnames(rate_grade) = c('요일', '할인율', '판매건수')
rate_grade$할인율별판매율 <- c()      
rate_grade[1,1,]$'판매건수' 
unique(kkk$dc_rate)


result11 <- table(kkk$dc_rate, kkk$wday)
result1 <- result11[c(1,6,11),]
rate_grade1 <- as.data.frame(result1)

result1

result1_prop <- prop.table(result1, 2)
result1_prop
result1_100 <- round(result1_prop, 5) *100
result1_100

barplot(result1_100,
        main= '요일별 할인율에 따른 판매건수 분표 비교',
        xlab='요일', ylab='판매건수', col = rainbow(3))
#legend=c('0%', '5%' ,'10%'))

----------------------------------------------------------------------------------
##
  
# 카테고리별 할인율에 따른 판매건수 분표 비교

kkk$category <- d_data$buyer_nm
cat_table <- table(kkk$dc_rate, kkk$category)
cat_table <- cat_table[c(1,6,11),]
cat_table
cat_prop <- prop.table(cat_table, 2)
cat_prop
cat_100 <- round(cat_prop, 5) * 100
cat_100


str(kkk$dc_rate)

barplot(cat_100,
        main= '카테고리별 할인율에 따른 판매건수 분표 비교',
        xlab='카테고리', ylab='판매건수', col = rainbow(3), cex.names=1, las=2)


##

kkk$category=='본점'

kkk$store <- d_data$str_nm
cat_table1 <- table(kkk[,,,,,,,,'본점']$dc_rate, kkk[,,,,,,,,'본점']$category)

kkk <- data.frame(year=allk$year+1900, mon=allk$mon+1, day=allk$mday, hour=allstime,
                  wday=allk$wday, sale=allsale, dc_rate=alldc_rate)


kkk$store
kkk1 <- kkk[kkk$store == '본점',]
kkk2 <- kkk[kkk$store == '무역점',]
kkk3 <- kkk[kkk$store == '천호점',]
kkk4 <- kkk[kkk$store == '신촌점',]

str(kkk1)
str(kkk2)
str(kkk3)
str(kkk4)

par(mfrow=c(1,1)) #멀티 캔버스 프레임 리셋
# 본점

cat_table1 <- table(kkk1$dc_rate, kkk1$category)
cat_table1 <- cat_table1[c(1,6,11),]
cat_table1
cat_prop1 <- prop.table(cat_table1, 2)
cat_prop1
cat_100_1 <- round(cat_prop1, 5) * 100
cat_100_1

barplot(cat_100_1,
        main= '본점의 카테고리별 할인율에 따른 판매건수 분표 비교',
        xlab='카테고리', ylab='판매건수', col = rainbow(3), cex.names=1, las=2)

# 무역점

cat_table2 <- table(kkk2$dc_rate, kkk2$category)
cat_table2 <- cat_table2[c(1,6,11),]
cat_table2
cat_prop2 <- prop.table(cat_table2, 2)
cat_prop2
cat_100_2 <- round(cat_prop2, 5) * 100
cat_100_2

barplot(cat_100_2,
        main= '무역점의 카테고리별 할인율에 따른 판매건수 분표 비교',
        xlab='카테고리', ylab='판매건수', col = rainbow(3), cex.names=1, las=2)

# 천호점

cat_table3 <- table(kkk3$dc_rate, kkk3$category)
cat_table3 <- cat_table3[c(1,6,11),]
cat_table3
cat_prop3 <- prop.table(cat_table3, 2)
cat_prop3
cat_100_3 <- round(cat_prop3, 5) * 100
cat_100_3

barplot(cat_100_3,
        main= '천호점의 카테고리별 할인율에 따른 판매건수 분표 비교',
        xlab='카테고리', ylab='판매건수', col = rainbow(3), cex.names=1, las=2)

# 신촌점

cat_table4 <- table(kkk4$dc_rate, kkk4$category)
cat_table4 <- cat_table4[c(1,6,11),]
cat_table4
cat_prop4 <- prop.table(cat_table4, 2)
cat_prop4
cat_100_4 <- round(cat_prop4, 5) * 100
cat_100_4

barplot(cat_100_4,
        main= '신촌점의 카테고리별 할인율에 따른 판매건수 분표 비교',
        xlab='카테고리', ylab='판매건수', col = rainbow(3), cex.names=1, las=2)

kkk[kkk$category=='화장품',]

######

barplot(cat_100_1,
        main= '본점의 카테고리별 할인율에 따른 판매건수 분표 비교',
        xlab='카테고리', ylab='판매건수', col = c('blue','green','yellow'), cex.names=1, las=2)

barplot(cat_100_2,
        main= '무역점의 카테고리별 할인율에 따른 판매건수 분표 비교',
        xlab='카테고리', ylab='판매건수', col = c('blue','green','yellow'), cex.names=1, las=2)

barplot(cat_100_3,
        main= '천호점의 카테고리별 할인율에 따른 판매건수 분표 비교',
        xlab='카테고리', ylab='판매건수', col = c('blue','green','yellow'), cex.names=1, las=2)

barplot(cat_100_4,
        main= '신촌점의 카테고리별 할인율에 따른 판매건수 분표 비교',
        xlab='카테고리', ylab='판매건수', col = c('blue','green','yellow'), cex.names=1, las=2)

# ----------------------------------------------
install.packages('gtools')

library(gtools)


cat_100_mix <- smartbind(cat_100_1, cat_100_2)
cat_100_mix <- smartbind(cat_100_1, cat_100_2, cat_100_3, cat_100_4) 
cat_100_mix


cat_100_mix <- as.table(cat_100_mix)
cat_100_mix <- row.names(c(1,2,3,4,5,6,7,8,9))

class(cat_100_mix)
class(cat_100_1)
barplot(cat_100_mix,
        main= '본점의 카테고리별 할인율에 따른 판매건수 분표 비교',
        xlab='카테고리', ylab='판매건수', col = rainbow(3), cex.names=1, las=2)






