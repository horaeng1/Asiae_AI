data <-read.csv("C:\\Users\\ASIAE_24\\Documents\\data\\X_train_new.csv")
g <- read.csv("C:\\Users\\ASIAE_24\\Documents\\data\\y_train.csv")
colnames(data)
summary(data)

install.packages(c('dplyr','psych', 'Hmisc', 'skimr'))
library(dplyr)
library(psych)
library(Hmisc)
library(skimr)
library(sm)

# ----------------------------------
# 범주형 데이터 팩터형으로 변환 
# custid 팩터형으로 변환
colnames(data)
(unique(data$custid))
data$custid_f <- factor(data$custid, levels=unique(data$custid), labels=c(0:29999))
data$custid_f <- factor(data$custid_f, levels=c(0:29999), labels=unique(data$custid))
data$custid_f
str(data)

# store 팩터형으로 변환
colnames(data)
(unique(data$str_nm))
data$str_nm_f <- factor(data$str_nm, levels=unique(data$str_nm), labels=c(0:3))
data$str_nm_f <- factor(data$str_nm_f, levels=c(0:3), labels=unique(data$str_nm))
data$str_nm_f
str(data)

# buyer_nm 팩터형으로 변환
colnames(data)
(unique(data$buyer_nm))
data$brd_nm
data$buyer_nm_f <- factor(data$buyer_nm, levels=unique(data$buyer_nm), labels=c(0:34))
data$buyer_nm_f <- factor(data$buyer_nm_f, levels=c(0:34), labels=unique(data$buyer_nm))
data$buyer_nm_f
str(data$buyer_nm_f)

# team_nm 팩터형으로 변환
colnames(data)
(unique(data$team_nm))
data$team_nm_f <- factor(data$team_nm, levels=unique(data$team_nm), labels=c(0:4))
data$team_nm_f <- factor(data$team_nm_f, levels=c(0:4), labels=unique(data$team_nm))
data$team_nm_f
str(data)

# pc_nm 팩터형으로 변환
colnames(data)
(unique(data$pc_nm))
data$pc_nm_f <- factor(data$pc_nm, levels=unique(data$pc_nm), labels=c(0:77))
data$pc_nm_f <- factor(data$pc_nm_f, levels=c(0:77), labels=unique(data$pc_nm))
data$pc_nm_f
str(data)

# corner_nm 팩터형으로 변환
colnames(data)
(unique(data$corner_nm))
data$corner_nm_f <- factor(data$corner_nm, levels=unique(data$corner_nm), labels=c(0:308))
data$corner_nm_f <- factor(data$corner_nm_f, levels=c(0:308), labels=unique(data$corner_nm))
data$corner_nm_f
str(data)

# brd_nm 팩터형으로 변환
colnames(data)
(unique(data$brd_nm))
data$brd_nm_f <- factor(data$brd_nm, levels=unique(data$brd_nm), labels=c(0:(length(unique(data$brd_nm))-1)))
data$brd_nm_f <- factor(data$brd_nm_f, levels=c(0:(length(unique(data$brd_nm))-1)), labels=unique(data$brd_nm))
data$brd_nm_f
str(data)

# import_flg 팩터형으로 변환
colnames(data)
data$import_flg_f <- factor(data$import_flg, levels=c(0:1), labels=c('수입', '국산'))
data$import_flg_f
str(data)

# ---------------------------------------------------------
# sales_time_hour 
# sales_tune 에서 거래 시간별로 데이터 가공
date_time_time <- transform(data$sales_time,
                                  sales_time_hour1 = sprintf("%04d", data$sales_time))
date_time_time$sales_time_hour1

date_time_time <- date_time_time$sales_time_hour1
date_time_time

date_time_time <- substr(date_time_time, 0, 2)
unique(date_time_time)
date_time_time
date_time_time <- as.integer(date_time_time)
data$sales_time_hour <- date_time_time
str(data)



# sales_date_month 
# sales_date 에서 월별로 데이터 가공
data$sales_date
date_time_month <- substr(data$sales_date, 0, 7)
date_time_month
data$sales_date_month <- date_time_month
str(data)

# sales_date_day
# sales_date 에서 일별로 데이터 가공
data$sales_date
date_time_day <- substr(data$sales_date, 9, 10)
date_time_day
data$sales_date_day <- date_time_day

data$sales_date_day <- as.integer(data$sales_date_day)
str(data)

# ----------------------------------------------------------
# sales_time_hour  팩터형으로 변환
colnames(data)
length(unique(data$sales_time_hour))
data$sales_time_hour_f <- factor(data$sales_time_hour, levels=unique(data$sales_time_hour), labels=c(0:17))
data$sales_time_hour_f <- factor(data$sales_time_hour_f, levels=c(0:17), labels=unique(data$sales_time_hour))
data$sales_time_hour_f
str(data)

# ----------------------------------------------------------
# sales_date_month  팩터형으로 변환
colnames(data)
(unique(data$sales_date_month))
data$sales_date_month_f <- factor(data$sales_date_month, levels=unique(data$sales_date_month), labels=c(0:11))
data$sales_date_month_f <- factor(data$sales_date_month_f, levels=c(0:11), labels=unique(data$sales_date_month))
data$sales_date_month_f
str(data)
# ----------------------------------------------------------
# custid 분석
# custid 명수 파악
length(unique(data$custid_f))

# custid 기술통계량
summary(data$custid)
psych::describe(data$custid)
Hmisc::describe(data$custid)
skim(data$custid)

# 고객별 거래 빈도 분석
custid_freq <- table(data$custid_f)
custid_freq

# 빈도 분석 결과를 데이터 프레임형태로 변환
cust_df <- as.data.frame(custid_freq)
cust_df

# 비율 분석
cust_prop <- prop.table(custid_freq)
cust_prop

# 비율 분석 결과를 데이터 프레임으로 변환
cust_prop_df <- as.data.frame(cust_prop)
cust_prop_df

# 백분율 분석
custid_pect <- round(cust_prop, 6) * 100
custid_pect

# 백분율 분석 결과를 데이터 프레임으로 변환
custid_pect_df <- as.data.frame(custid_pect)
custid_pect

barplot(custid_freq,
        main = "고객별 거래 빈도 비교",
        xlab = '고객', ylab = '건수')
# ----------------------------------------------------------
# sales_date_month 월별 분석
# 총 개월수 파악
length(unique(data$sales_date_month))

# sales_data_month 기술통계량
summary(data$sales_date_month_f)
psych::describe(data$sales_date_month_f)
Hmisc::describe(data$sales_date_month_f)
skim(data$sales_date_month_f)

# 고객별 거래 빈도 분석
sales_date_month_f_freq <- table(data$sales_date_month_f)
sales_date_month_f_freq

# 빈도 분석 결과를 데이터 프레임형태로 변환
sales_date_month_f_freq_df <- as.data.frame(sales_date_month_f_freq)
sales_date_month_f_freq_df

# 비율 분석
sales_date_month_f_prop <- prop.table(sales_date_month_f_freq)
sales_date_month_f_prop

# 비율 분석 결과를 데이터 프레임으로 변환
sales_date_month_f_prop_df <- as.data.frame(sales_date_month_f_prop)
sales_date_month_f_prop_df

# 백분율 분석
sales_date_month_f_pect <- round(sales_date_month_f_prop, 4) * 100
sales_date_month_f_pect

# 백분율 분석 결과를 데이터 프레임으로 변환
sales_date_month_f_pect_df <- as.data.frame(sales_date_month_f_pect)
sales_date_month_f_pect_df


barplot(sales_date_month_f_freq,
        main = "월별 거래 빈도 비교",
        xlab = '년월', ylab = '건수')
barplot(sales_date_month_f_prop,
        main = "월별 거래 빈도 비교",
        xlab = '년월', ylab = '건수')


# ----------------------------------------------------------
# sales_time_hour 시간대별 분석
# 시간대 파악
(unique(data$sales_time_hour))

# sales_time_hour 기술통계량
summary(data$sales_time_hour_f)
psych::describe(data$sales_time_hour_f)
Hmisc::describe(data$sales_time_hour_f)
skim(data$sales_time_hour_f)

# 고객별 거래 빈도 분석
sales_time_hour_f_freq <- table(data$sales_time_hour_f)
sales_time_hour_f_freq

# 빈도 분석 결과를 데이터 프레임형태로 변환
sales_time_hour_f_freq_df <- as.data.frame(sales_time_hour_f_freq)
sales_time_hour_f_freq_df

# 비율 분석
sales_time_hour_f_prop <- prop.table(sales_time_hour_f_freq)
sales_time_hour_f_prop
str(sales_time_hour_f_prop)

# 비율 분석 결과를 데이터 프레임으로 변환
sales_time_hour_f_prop_df <- as.data.frame(sales_time_hour_f_prop)
sales_time_hour_f_prop_df
arrange(sales_time_hour_f_prop_df, desc(Var1))

# 백분율 분석
sales_time_hour_f_pect <- round(sales_time_hour_f_prop, 4) * 100
sales_time_hour_f_pect

# 백분율 분석 결과를 데이터 프레임으로 변환
sales_time_hour_f_pect_df <- as.data.frame(sales_time_hour_f_pect)
sales_time_hour_f_pect_df

library(dplyr)
barplot(sales_time_hour_f_prop,
        main = "시간별 거래 빈도 비교",
        xlab = '시간', ylab = '건수')
barplot(sales_time_hour_f_freq,
        main = "시간별 거래 빈도 비교",
        xlab = '시간', ylab = '건수')

# ----------------------------------------------------------
# sales_date_day 일별 분석
# 총 일수 파악
length(unique(data$sales_date_day))

# sales_data_day 기술통계량
summary(data$sales_date_day)
psych::describe(data$sales_date_day)
Hmisc::describe(data$sales_date_day)
skim(data$sales_date_day)

# 고객별 거래 빈도 분석
sales_date_day_freq <- table(data$sales_date_day)
sales_date_day_freq

# 빈도 분석 결과를 데이터 프레임형태로 변환
sales_date_month_f_freq_df <- as.data.frame(sales_date_month_f_freq)
sales_date_month_f_freq_df

# 비율 분석
sales_date_month_f_prop <- prop.table(sales_date_month_f_freq)
sales_date_month_f_prop

# 비율 분석 결과를 데이터 프레임으로 변환
sales_date_month_f_prop_df <- as.data.frame(sales_date_month_f_prop)
sales_date_month_f_prop_df

# 백분율 분석
sales_date_month_f_pect <- round(sales_date_month_f_prop, 4) * 100
sales_date_month_f_pect

# 백분율 분석 결과를 데이터 프레임으로 변환
sales_date_month_f_pect_df <- as.data.frame(sales_date_month_f_pect)
sales_date_month_f_pect_df


barplot(sales_date_day_freq,
        main = "일별 거래 빈도 비교",
        xlab = '일', ylab = '건수')

# ----------------------------------------------------------
# 지점과 거래 시간대별 요약 집계 분석

# 지점변수 기술통계량
Hmisc::describe(data$str_nm_f)

# 시간대별 기술통계량
skim(data$sales_time_hour)
Hmisc::describe(data$sales_time_hour)

# 단순 프로팅 그래프
p1 <- ggplot(data, aes(str_nm_f, sales_time_hour))+
  geom_point(color = "red", shape=20, size=2)
p1

# 모든 데이터를 플로팅하는 형태로 분포모양을 나타냄
p2 <- ggplot(data, aes(str_nm_f, sales_time_hour))+
  geom_jitter(color = "blue", shape = 8, size = 0.8)
p2

#sm::sm.density.compare()을 사용한 밀도그래프
sm.density.compare(x = data$sales_time_hour, group = data$str_nm_f,
                   xlab = '시간대', ylab = '밀도')

# ----------------------------------------------------------
# 월별 브랜드별 거래 건수 확인
aaaaa <- table(data$sales_date_month, data$brd_nm)
str(aaaaa)
View(aaaaa)

aaaaa <- table(data$sales_date_month, data$brd_nm)

tapply(data$dis_amt, data$sales_date_month, count)

# ----------------------------------------------------------
str(data$custid)
summary(data$custid)

table(data$custid_f, data$buyer_nm_f)
aa <- table(data$custid_f, data$buyer_nm_f)


unique(data$buyer_nm_f)

# ----------------------------------------------------------
aggregate(formula = tot_amt ~ buyer_nm_f, data = data, FUN = mean, na.rm = TRUE)
aggregate(tot_amt ~ buyer_nm_f, data, mean, na.rm=TRUE, trim=0.05)




# ----------------------------------------------------------

Hmisc::describe(data$str_nm_f)
psych::describe(data$tot_amt)

df1 <- aggregate(formula = tot_amt ~ str_nm_f + custid, data = data, FUN = sum, na.rm = TRUE)



data %>%
  group_by(str_nm_f) %>%
  dplyr::summarize(Avg = sum(tot_amt), SD = sd(tot_amt)) %>%
  arrange(desc(Avg))

library(sm)
sm.density.compare(x = data$tot_amt, group = data$str_nm_f,
                   xlab = "총사용금액(tot_amt)", ylab = "밀도",
                   col = c(2,5), lty = c(2,5))

p1 <- ggplot(df1, aes(custid,tot_amt, color = str_nm_f, shape = str_nm_f))+
  geom_boxplot() +
  labs(title = "고객별 사용 금액을 지점별로 세분화해 비교분석")
p1
p2 <- ggplot(df1, aes(custid, tot_amt))+
  geom_boxplot() + facet_wrap(~str_nm_f)+
  labs(title = "고객별 사용 금액을 지점별로 분할그래프로 비교분석")
p2

  
  
aaaaa <- data.frame(x=c('a', 'b'), y=c(1,2))

aaaaa[aaaaa$x=='a',]$y

str(unique(data$brd_nm))


