data <-read.csv("C:\\Users\\ASIAE_24\\Documents\\data\\X_train.csv")
colnames(data)
summary(data)

install.packages(c('dplyr','psych', 'Hmisc', 'skimr'))
library(dplyr)
library(psych)
library(Hmisc)
library(skimr)


# custid 명목척도 분석
# custid 기술 통계량
psych::describe(data$custid)
Hmisc::describe(data$custid)
skim(data$custid)
# custid 거래 빈도 분석
custid_freq <- table(data$custid)
custid_freq

# 빈도 분석 결과를 데이터프레임형태로 변환
custid_df <- as.data.frame(custid_freq)
custid_df

# 비율 분석
custid_prop <- prop.table(custid_freq)
custid_prop

# 비율 분석 결과를 데이터프레임형태로 변환
custid_prop_df <- as.data.frame(custid_prop)
custid_prop_df

# 백분율 분석
custid_pect <- round(custid_prop, 10) * 100
custid_pect

# 백분율 분석 결과를 데이터프레임형태로 변환
custid_pect_df <- as.data.frame(custid_pect)
custid_pect_df

# barplot() 함수를 이용한 범주형 데이터 그래프
par(mfrow=c(2,2))

barplot(custid_freq,
        main = '고객별 거래 횟수 분포 비교',
        xlab = '고객', ylab='거래 횟수')
barplot(custid_freq,
        main = '고객별 거래 횟수 분포 비교',
        xlab = '고객', ylab='거래 횟수', las = 1, horiz=TRUE)
barplot(custid_prop,
        main = '고객별 거래 횟수 분포 비교',
        xlab = '고객', ylab='거래 횟수', density = c(20, 30))
barplot(custid_prop,
        main = '고객별 거래 횟수 분포 비교',
        xlab = '고객', ylab='거래 횟수', las = 1,
        horiz = TRUE, col = c('lightblue', 'red'),
        beside=TRUE)
par(mfrow=c(1,1))



# store 명목척도 분석
# store 기술 통계량
psych::describe(data$store)
Hmisc::describe(data$store)
skim(data$store)
# store 거래 빈도 분석
store_freq <- table(data$store)
store_freq

# 빈도 분석 결과를 데이터프레임형태로 변환
store_df <- as.data.frame(store_freq)
store_df

# 비율 분석
store_prop <- prop.table(store_freq)
store_prop

# 비율 분석 결과를 데이터프레임형태로 변환
store_prop_df <- as.data.frame(store_prop)
store_prop_df

# 백분율 분석
store_pect <- round(store_prop, 10) * 100
store_pect

# 백분율 분석 결과를 데이터프레임형태로 변환
store_pect_df <- as.data.frame(store_pect)
store_pect_df

# barplot() 함수를 이용한 범주형 데이터 그래프
par(mfrow=c(2,2))

barplot(store_freq,
        main = '지점별 거래 횟수 분포 비교',
        xlab = '지점', ylab='거래 횟수')
barplot(store_freq,
        main = '지점별 거래 횟수 분포 비교',
        xlab = '지점', ylab='거래 횟수', las = 1, horiz=TRUE)
barplot(store_prop,
        main = '지점별 거래 횟수 분포 비교',
        xlab = '지점', ylab='거래 횟수', density = c(20, 30))
barplot(store_prop,
        main = '지점별 거래 횟수 분포 비교',
        xlab = '지점', ylab='거래 횟수', las = 1,
        horiz = TRUE, col = c('lightblue', 'red'),
        beside=TRUE)
par(mfrow=c(1,1))

# product 명목척도 분석
# product 기술 통계량
psych::describe(data$product)
Hmisc::describe(data$product)
skim(data$product)
# product 거래 빈도 분석
product_freq <- table(data$product)
product_freq

# 빈도 분석 결과를 데이터프레임형태로 변환
product_df <- as.data.frame(product_freq)
product_df

# 비율 분석
product_prop <- prop.table(product_freq)
product_prop

# 비율 분석 결과를 데이터프레임형태로 변환
product_prop_df <- as.data.frame(product_prop)
product_prop_df

# 백분율 분석
product_pect <- round(product_prop, 10) * 100
product_pect

# 백분율 분석 결과를 데이터프레임형태로 변환
product_pect_df <- as.data.frame(product_pect)
product_pect_df

# barplot() 함수를 이용한 범주형 데이터 그래프
par(mfrow=c(2,2))

barplot(product_freq,
        main = '품목별 거래 횟수 분포 비교',
        xlab = '품목', ylab='거래 횟수')
barplot(product_freq,
        main = '품목별 거래 횟수 분포 비교',
        xlab = '품목', ylab='거래 횟수', las = 1, horiz=TRUE)
barplot(product_prop,
        main = '품목별 거래 횟수 분포 비교',
        xlab = '품목', ylab='거래 횟수', density = c(20, 30))
barplot(product_prop,
        main = '품목별 거래 횟수 분포 비교',
        xlab = '품목', ylab='거래 횟수', las = 1,
        horiz = TRUE, col = c('lightblue', 'red'),
        beside=TRUE)
par(mfrow=c(1,1))

# brand 명목척도 분석
# brand 기술 통계량
psych::describe(data$brand)
Hmisc::describe(data$brand)
skim(data$brand)
# brand 거래 빈도 분석
brand_freq <- table(data$brand)
brand_freq

# 빈도 분석 결과를 데이터프레임형태로 변환
brand_df <- as.data.frame(brand_freq)
brand_df

# 비율 분석
brand_prop <- prop.table(brand_freq)
brand_prop

# 비율 분석 결과를 데이터프레임형태로 변환
brand_prop_df <- as.data.frame(brand_prop)
brand_prop_df

# 백분율 분석
brand_pect <- round(brand_prop, 10) * 100
brand_pect

# 백분율 분석 결과를 데이터프레임형태로 변환
brand_pect_df <- as.data.frame(brand_pect)
brand_pect_df

# barplot() 함수를 이용한 범주형 데이터 그래프
par(mfrow=c(2,2))

barplot(brand_freq,
        main = '브랜드별 거래 횟수 분포 비교',
        xlab = '브랜드', ylab='거래 횟수')
barplot(brand_freq,
        main = '브랜드별 거래 횟수 분포 비교',
        xlab = '브랜드', ylab='거래 횟수', las = 1, horiz=TRUE)
barplot(brand_prop,
        main = '브랜드별 거래 횟수 분포 비교',
        xlab = '브랜드', ylab='거래 횟수', density = c(20, 30))
barplot(brand_prop,
        main = '브랜드별 거래 횟수 분포 비교',
        xlab = '브랜드', ylab='거래 횟수', las = 1,
        horiz = TRUE, col = c('lightblue', 'red'),
        beside=TRUE)
par(mfrow=c(1,1))

# corner 명목척도 분석
# corner 기술 통계량
psych::describe(data$corner)
Hmisc::describe(data$corner)
skim(data$corner)
# corner 거래 빈도 분석
corner_freq <- table(data$corner)
corner_freq

# 빈도 분석 결과를 데이터프레임형태로 변환
corner_df <- as.data.frame(corner_freq)
corner_df

# 비율 분석
corner_prop <- prop.table(corner_freq)
corner_prop

# 비율 분석 결과를 데이터프레임형태로 변환
corner_prop_df <- as.data.frame(corner_prop)
corner_prop_df

# 백분율 분석
corner_pect <- round(corner_prop, 10) * 100
corner_pect

# 백분율 분석 결과를 데이터프레임형태로 변환
corner_pect_df <- as.data.frame(corner_pect)
corner_pect_df

# barplot() 함수를 이용한 범주형 데이터 그래프
par(mfrow=c(2,2))

barplot(corner_freq,
        main = '코너별 거래 횟수 분포 비교',
        xlab = '코너', ylab='거래 횟수')
barplot(corner_freq,
        main = '코너별 거래 횟수 분포 비교',
        xlab = '코너', ylab='거래 횟수', las = 1, horiz=TRUE)
barplot(corner_prop,
        main = '코너별 거래 횟수 분포 비교',
        xlab = '코너', ylab='거래 횟수', density = c(20, 30))
barplot(corner_prop,
        main = '코너별 거래 횟수 분포 비교',
        xlab = '코너', ylab='거래 횟수', las = 1,
        horiz = TRUE, col = c('lightblue', 'red'),
        beside=TRUE)
par(mfrow=c(1,1))

# pc 명목척도 분석
# pc 기술 통계량
psych::describe(data$pc)
Hmisc::describe(data$pc)
skim(data$pc)
# pc 거래 빈도 분석
pc_freq <- table(data$pc)
pc_freq

# 빈도 분석 결과를 데이터프레임형태로 변환
pc_df <- as.data.frame(pc_freq)
pc_df

# 비율 분석
pc_prop <- prop.table(pc_freq)
pc_prop

# 비율 분석 결과를 데이터프레임형태로 변환
pc_prop_df <- as.data.frame(pc_prop)
pc_prop_df

# 백분율 분석
pc_pect <- round(pc_prop, 10) * 100
pc_pect

# 백분율 분석 결과를 데이터프레임형태로 변환
pc_pect_df <- as.data.frame(pc_pect)
pc_pect_df

# barplot() 함수를 이용한 범주형 데이터 그래프
par(mfrow=c(2,2))

barplot(pc_freq,
        main = 'pc별 거래 횟수 분포 비교',
        xlab = 'pc', ylab='거래 횟수')
barplot(pc_freq,
        main = 'pc별 거래 횟수 분포 비교',
        xlab = 'pc', ylab='거래 횟수', las = 1, horiz=TRUE)
barplot(pc_prop,
        main = 'pc별 거래 횟수 분포 비교',
        xlab = 'pc', ylab='거래 횟수', density = c(20, 30))
barplot(pc_prop,
        main = 'pc별 거래 횟수 분포 비교',
        xlab = 'pc', ylab='거래 횟수', las = 1,
        horiz = TRUE, col = c('lightblue', 'red'),
        beside=TRUE)
par(mfrow=c(1,1))

# part 명목척도 분석
# part 기술 통계량
psych::describe(data$part)
Hmisc::describe(data$part)
skim(data$part)
# part 거래 빈도 분석
part_freq <- table(data$part)
part_freq

# 빈도 분석 결과를 데이터프레임형태로 변환
part_df <- as.data.frame(part_freq)
part_df

# 비율 분석
part_prop <- prop.table(part_freq)
part_prop

# 비율 분석 결과를 데이터프레임형태로 변환
part_prop_df <- as.data.frame(part_prop)
part_prop_df

# 백분율 분석
part_pect <- round(part_prop, 10) * 100
part_pect

# 백분율 분석 결과를 데이터프레임형태로 변환
part_pect_df <- as.data.frame(part_pect)
part_pect_df

# barplot() 함수를 이용한 범주형 데이터 그래프
par(mfrow=c(2,2))

barplot(part_freq,
        main = '파트별 거래 횟수 분포 비교',
        xlab = '파트', ylab='거래 횟수')
barplot(part_freq,
        main = '파트별 거래 횟수 분포 비교',
        xlab = '파트', ylab='거래 횟수', las = 1, horiz=TRUE)
barplot(part_prop,
        main = '파트별 거래 횟수 분포 비교',
        xlab = '파트', ylab='거래 횟수', density = c(20, 30))
barplot(part_prop,
        main = '파트별 거래 횟수 분포 비교',
        xlab = '파트', ylab='거래 횟수', las = 1,
        horiz = TRUE, col = c('lightblue', 'red'),
        beside=TRUE)
par(mfrow=c(1,1))

# imported 명목척도 분석
# imported 기술 통계량
psych::describe(data$imported)
Hmisc::describe(data$imported)
skim(data$imported)
# imported 거래 빈도 분석
imported_freq <- table(data$imported)
imported_freq

# 빈도 분석 결과를 데이터프레임형태로 변환
imported_df <- as.data.frame(imported_freq)
imported_df

# 비율 분석
imported_prop <- prop.table(imported_freq)
imported_prop

# 비율 분석 결과를 데이터프레임형태로 변환
imported_prop_df <- as.data.frame(imported_prop)
imported_prop_df

# 백분율 분석
imported_pect <- round(imported_prop, 10) * 100
imported_pect

# 백분율 분석 결과를 데이터프레임형태로 변환
imported_pect_df <- as.data.frame(imported_pect)
imported_pect_df

# barplot() 함수를 이용한 범주형 데이터 그래프
par(mfrow=c(2,2))

barplot(imported_freq,
        main = '수입품 거래 횟수 분포 비교',
        xlab = '수입 유무', ylab='거래 횟수')
barplot(imported_freq,
        main = '수입품 거래 횟수 분포 비교',
        xlab = '수입 유무', ylab='거래 횟수', las = 1, horiz=TRUE)
barplot(imported_prop,
        main = '수입품 거래 횟수 분포 비교',
        xlab = '수입 유무', ylab='거래 횟수', density = c(20, 30))
barplot(imported_prop,
        main = '수입품 거래 횟수 분포 비교',
        xlab = '수입 유무', ylab='거래 횟수', las = 1,
        horiz = TRUE, col = c('lightblue', 'red'),
        beside=TRUE)
par(mfrow=c(1,1))



# installment 명목척도 분석
# installment 기술 통계량
psych::describe(data$installment)
Hmisc::describe(data$installment)
skim(data$installment)
# installment 거래 빈도 분석
installment_freq <- table(data$installment)
installment_freq

# 빈도 분석 결과를 데이터프레임형태로 변환
installment_df <- as.data.frame(installment_freq)
installment_df

# 비율 분석
installment_prop <- prop.table(installment_freq)
installment_prop

# 비율 분석 결과를 데이터프레임형태로 변환
installment_prop_df <- as.data.frame(installment_prop)
installment_prop_df

# 백분율 분석
installment_pect <- round(installment_prop, 10) * 100
installment_pect

# 백분율 분석 결과를 데이터프레임형태로 변환
installment_pect_df <- as.data.frame(installment_pect)
installment_pect_df

# barplot() 함수를 이용한 범주형 데이터 그래프
par(mfrow=c(2,2))

barplot(installment_freq,
        main = '할부 월별 거래 횟수 분포 비교',
        xlab = '월', ylab='거래 횟수')
barplot(installment_freq,
        main = '할부 월별 거래 횟수 분포 비교',
        xlab = '월', ylab='거래 횟수', las = 1, horiz=TRUE)
barplot(installment_prop,
        main = '할부 월별 거래 횟수 분포 비교',
        xlab = '월', ylab='거래 횟수', density = c(20, 30))
barplot(installment_prop,
        main = '할부 월별 거래 횟수 분포 비교',
        xlab = '월', ylab='거래 횟수', las = 1,
        horiz = TRUE, col = c('lightblue', 'red'),
        beside=TRUE)
par(mfrow=c(1,1))



# discount 명목척도 분석
# discount 범주화
discount_n <- cut(data$discount, breaks = c(-Inf, (seq(-100000, 100000, 1000)), Inf ))
discount_n

data$discount_n <-discount_n
str(data)

# discount 기술 통계량
psych::describe(data$discount_n)
Hmisc::describe(data$discount_n)
skim(data$discount_n)
# discount 거래 빈도 분석
discount_freq <- table(data$discount_n)
discount_freq

# 빈도 분석 결과를 데이터프레임형태로 변환
discount_df <- as.data.frame(discount_freq)
discount_df

# 비율 분석
discount_prop <- prop.table(discount_freq)
discount_prop

# 비율 분석 결과를 데이터프레임형태로 변환
discount_prop_df <- as.data.frame(discount_prop)
discount_prop_df

# 백분율 분석
discount_pect <- round(discount_prop, 10) * 100
discount_pect

# 백분율 분석 결과를 데이터프레임형태로 변환
discount_pect_df <- as.data.frame(discount_pect)
discount_pect_df

# barplot() 함수를 이용한 범주형 데이터 그래프
par(mfrow=c(2,2))

barplot(discount_freq,
        main = '할인금액별 거래 횟수 분포 비교',
        xlab = '할인금액', ylab='거래 횟수')
barplot(discount_freq,
        main = '할인금액별 거래 횟수 분포 비교',
        xlab = '할인금액', ylab='거래 횟수', las = 1, horiz=TRUE)
barplot(discount_prop,
        main = '할인금액별 거래 횟수 분포 비교',
        xlab = '할인금액', ylab='거래 횟수', density = c(20, 30))
barplot(discount_prop,
        main = '할인금액별 거래 횟수 분포 비교',
        xlab = '할인금액', ylab='거래 횟수', las = 1,
        horiz = TRUE, col = c('lightblue', 'red'),
        beside=TRUE)
par(mfrow=c(1,1))


# amount 명목척도 분석
# amount 범주화
amount_n <- cut(data$amount, breaks = c(-Inf, (seq(-1000000, 1000000, 5000)), Inf ))
amount_n

data$amount_n <-amount_n
str(data)

# amount 기술 통계량
psych::describe(data$amount_n)
Hmisc::describe(data$amount_n)
skim(data$amount_n)
# amount 거래 빈도 분석
amount_freq <- table(data$amount_n)
amount_freq

# 빈도 분석 결과를 데이터프레임형태로 변환
amount_df <- as.data.frame(amount_freq)
amount_df

# 비율 분석
amount_prop <- prop.table(amount_freq)
amount_prop

# 비율 분석 결과를 데이터프레임형태로 변환
amount_prop_df <- as.data.frame(amount_prop)
amount_prop_df

# 백분율 분석
amount_pect <- round(amount_prop, 10) * 100
amount_pect

# 백분율 분석 결과를 데이터프레임형태로 변환
amount_pect_df <- as.data.frame(amount_pect)
amount_pect_df

# barplot() 함수를 이용한 범주형 데이터 그래프
par(mfrow=c(2,2))

barplot(amount_freq,
        main = '금액별 거래 횟수 분포 비교',
        xlab = '금액', ylab='거래 횟수')
barplot(amount_freq,
        main = '금액별 거래 횟수 분포 비교',
        xlab = '금액', ylab='거래 횟수', las = 1, horiz=TRUE)
barplot(amount_prop,
        main = '금액별 거래 횟수 분포 비교',
        xlab = '금액', ylab='거래 횟수', density = c(20, 30))
barplot(amount_prop,
        main = '금액별 거래 횟수 분포 비교',
        xlab = '금액', ylab='거래 횟수', las = 1,
        horiz = TRUE, col = c('lightblue', 'red'),
        beside=TRUE)
par(mfrow=c(1,1))



# date_time_date 명목척도 분석
# date_time 에서 일별로 데이터 가공
data$date_time
date_time_date <- substr(data$date_time,0, 10)
date_time_date
data$date_time_date <- date_time_date
str(data)
# date_time_date 기술 통계량
psych::describe(data$date_time_date)
Hmisc::describe(data$date_time_date)
skim(data$date_time_date)
# date_time_date 거래 빈도 분석
date_time_date_freq <- table(data$date_time_date)
date_time_date_freq

# 빈도 분석 결과를 데이터프레임형태로 변환
date_time_date_df <- as.data.frame(date_time_date_freq)
date_time_date_df

# 비율 분석
date_time_date_prop <- prop.table(date_time_date_freq)
date_time_date_prop

# 비율 분석 결과를 데이터프레임형태로 변환
date_time_date_prop_df <- as.data.frame(date_time_date_prop)
date_time_date_prop_df

# 백분율 분석
date_time_date_pect <- round(date_time_date_prop, 10) * 100
date_time_date_pect

# 백분율 분석 결과를 데이터프레임형태로 변환
date_time_date_pect_df <- as.data.frame(date_time_date_pect)
date_time_date_pect_df

# barplot() 함수를 이용한 범주형 데이터 그래프
par(mfrow=c(2,2))

barplot(date_time_date_freq,
        main = '일자별 거래 횟수 분포 비교',
        xlab = '일자', ylab='거래 횟수')
barplot(date_time_date_freq,
        main = '일자별 거래 횟수 분포 비교',
        xlab = '일자', ylab='거래 횟수', las = 1, horiz=TRUE)
barplot(date_time_date_prop,
        main = '일자별 거래 횟수 분포 비교',
        xlab = '일자', ylab='거래 횟수', density = c(20, 30))
barplot(date_time_date_prop,
        main = '일자별 거래 횟수 분포 비교',
        xlab = '일자', ylab='거래 횟수', las = 1,
        horiz = TRUE, col = c('lightblue', 'red'),
        beside=TRUE)
par(mfrow=c(1,1))


# date_time_month 명목척도 분석
# date_time 에서 월별로 데이터 가공
data$date_time
date_time_month <- substr(data$date_time,0, 7)
date_time_month
data$date_time_month <- date_time_month
str(data)
# date_time_month 기술 통계량
psych::describe(data$date_time_month)
Hmisc::describe(data$date_time_month)
skim(data$date_time_month)
# date_time_month 거래 빈도 분석
date_time_month_freq <- table(data$date_time_month)
date_time_month_freq

# 빈도 분석 결과를 데이터프레임형태로 변환
date_time_month_df <- as.data.frame(date_time_month_freq)
date_time_month_df

# 비율 분석
date_time_month_prop <- prop.table(date_time_month_freq)
date_time_month_prop

# 비율 분석 결과를 데이터프레임형태로 변환
date_time_month_prop_df <- as.data.frame(date_time_month_prop)
date_time_month_prop_df

# 백분율 분석
date_time_month_pect <- round(date_time_month_prop, 10) * 100
date_time_month_pect

# 백분율 분석 결과를 데이터프레임형태로 변환
date_time_month_pect_df <- as.data.frame(date_time_month_pect)
date_time_month_pect_df

# barplot() 함수를 이용한 범주형 데이터 그래프
par(mfrow=c(2,2))

barplot(date_time_month_freq,
        main = '월별 거래 횟수 분포 비교',
        xlab = '년월', ylab='거래 횟수')
barplot(date_time_month_freq,
        main = '월별 거래 횟수 분포 비교',
        xlab = '년월', ylab='거래 횟수', las = 1, horiz=TRUE)
barplot(date_time_month_prop,
        main = '월별 거래 횟수 분포 비교',
        xlab = '년월', ylab='거래 횟수', density = c(20, 30))
barplot(date_time_month_prop,
        main = '월별 거래 횟수 분포 비교',
        xlab = '년월', ylab='거래 횟수', las = 1,
        horiz = TRUE, col = c('lightblue', 'red'),
        beside=TRUE)
par(mfrow=c(1,1))


# date_time_time 명목척도 분석
# date_time 에서 시간대별로 데이터 가공
data$date_time
date_time_time <- substr(data$date_time,12, 13)
date_time_time
data$date_time_time <- date_time_time
str(data)
# date_time_time 기술 통계량
psych::describe(data$date_time_time)
Hmisc::describe(data$date_time_time)
skim(data$date_time_time)
# date_time_time 거래 빈도 분석
date_time_time_freq <- table(data$date_time_time)
date_time_time_freq

# 빈도 분석 결과를 데이터프레임형태로 변환
date_time_time_df <- as.data.frame(date_time_time_freq)
date_time_time_df

# 비율 분석
date_time_time_prop <- prop.table(date_time_time_freq)
date_time_time_prop

# 비율 분석 결과를 데이터프레임형태로 변환
date_time_time_prop_df <- as.data.frame(date_time_time_prop)
date_time_time_prop_df

# 백분율 분석
date_time_time_pect <- round(date_time_time_prop, 10) * 100
date_time_time_pect

# 백분율 분석 결과를 데이터프레임형태로 변환
date_time_time_pect_df <- as.data.frame(date_time_time_pect)
date_time_time_pect_df

# barplot() 함수를 이용한 범주형 데이터 그래프
par(mfrow=c(2,2))

barplot(date_time_time_freq,
        main = '시간대별 거래 횟수 분포 비교',
        xlab = '시', ylab='거래 횟수')
barplot(date_time_time_freq,
        main = '시간대별 거래 횟수 분포 비교',
        xlab = '시', ylab='거래 횟수', las = 1, horiz=TRUE)
barplot(date_time_time_prop,
        main = '시간대별 거래 횟수 분포 비교',
        xlab = '시', ylab='거래 횟수', density = c(20, 30))
barplot(date_time_time_prop,
        main = '시간대별 거래 횟수 분포 비교',
        xlab = '시', ylab='거래 횟수', las = 1,
        horiz = TRUE, col = c('lightblue', 'red'),
        beside=TRUE)
par(mfrow=c(1,1))

# date_time_day 명목척도 분석
# date_time 에서 요일별로 데이터 가공
data$date_time
date_time_day <- substr(data$date_time,0, 10)
date_time_day
date_time_day <- weekdays(as.Date(date_time_day))
date_time_day
data$date_time_day <- date_time_day
str(data)
# date_time_day 기술 통계량
psych::describe(data$date_time_day)
Hmisc::describe(data$date_time_day)
skim(data$date_time_day)
# date_time_day 거래 빈도 분석
date_time_day_freq <- table(data$date_time_day)
date_time_day_freq

# 빈도 분석 결과를 데이터프레임형태로 변환
date_time_day_df <- as.data.frame(date_time_day_freq)
date_time_day_df

# 비율 분석
date_time_day_prop <- prop.table(date_time_day_freq)
date_time_day_prop

# 비율 분석 결과를 데이터프레임형태로 변환
date_time_day_prop_df <- as.data.frame(date_time_day_prop)
date_time_day_prop_df

# 백분율 분석
date_time_day_pect <- round(date_time_day_prop, 10) * 100
date_time_day_pect

# 백분율 분석 결과를 데이터프레임형태로 변환
date_time_day_pect_df <- as.data.frame(date_time_day_pect)
date_time_day_pect_df

# barplot() 함수를 이용한 범주형 데이터 그래프
par(mfrow=c(2,2))

barplot(date_time_day_freq,
        main = '요일별 거래 횟수 분포 비교',
        xlab = '요일', ylab='거래 횟수')
barplot(date_time_day_freq,
        main = '요일별 거래 횟수 분포 비교',
        xlab = '요일', ylab='거래 횟수', las = 1, horiz=TRUE)
barplot(date_time_day_prop,
        main = '요일별 거래 횟수 분포 비교',
        xlab = '요일', ylab='거래 횟수', density = c(20, 30))
barplot(date_time_day_prop,
        main = '요일별 거래 횟수 분포 비교',
        xlab = '요일', ylab='거래 횟수', las = 1,
        horiz = TRUE, col = c('lightblue', 'red'),
        beside=TRUE)
par(mfrow=c(1,1))



# part의 value들을 중복제거해서 저장
part_list <- data[-which(duplicated(data$part)),]
part_list <- part_list$part
part_list
str(part_list)


# part 별로 숫자를 부여해서 part_f를 만듬
data$part_f <- factor(data$part, levels = part_list,
                      labels = c(0:30))
data

# part별로 amount를 더한 값을 만듬
part_sum <- aggregate(data$amount, by=list(data$part), FUN=sum)
part_sum







part_prop <- prop.table(part_freq)
part_prop

part_prop_df <- as.data.frame(part_prop)
part_prop_df
View(part_prop_df)


arrange(part_prop_df, desc(Freq))

round(part_prop); round(part_prop, 3)

part_pect <- round(part_prop, 3)*100
part_pect

part_pect_df <- as.data.frame(part_pect)
part_pect_df


arrange(part_pect_df, desc(Freq))

par(mfrow=c(2,2))

barplot(part_freq,
        main='파트별 거래 횟수 비교',
        xlab = '파트', ylab='거래 횟수')
barplot(part_freq,
        main='파트별 거래 횟수 비교',
        xlab = '파트', ylab='거래 횟수', las = 1, horiz = TRUE)
barplot(part_prop,
        main='파트별 거래 비율 비교',
        xlab = '파트', ylab='거래 횟수', density = c(20, 30))
barplot(part_prop,
        main='파트별 거래 비율 비교',
        xlab = '파트', ylab='거래 횟수', las = 1,
        horiz = TRUE, col = c("lightblue", "pink"),
        beside = TRUE)

str(data)
str(part_list)

part_sum_list <- part_sum$x
barplot(part_sum_list,
        main = '파트별 거래 금액 비교',
        xlab = '파트', ylab = '거래 금액')

str(part_sum)
