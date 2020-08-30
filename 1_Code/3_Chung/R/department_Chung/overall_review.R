library(doBy)
library(dplyr)
library(psych)
library(Hmisc)
library(skimr)
library(fBasics)
library(ggplot2)

Sys.setlocale("LC_ALL","korean")#os가 한글이 아닐시에 꼭 써야함


x_data <- read.csv("C:/Users/seokm/OneDrive/Documents/project_data/X_train.csv",header = TRUE, sep = ',', stringsAsFactors = FALSE,encoding = "CP949")
y_data <- read.csv("C:/Users/seokm/OneDrive/Documents/project_data/y_train.csv",header = TRUE, sep = ',',stringsAsFactors = FALSE,encoding = "CP949")
data <- merge(x = y_data, y = x_data, by = 'custid')


table(data$part_nm)
#---------dc_rate--------------------------------
#--------------------------------
dc_rate <- round((data$dis_amt/ data$tot_amt)*100, 0)
dc_rate
unique(dc_rate)
summary(dc_rate)

from <- list(0, c(1:5), c(6:65))
from
to <- list(0, 5, 10)
to
library(doBy)
dc_rate <- recodeVar(dc_rate , from , to)

dc_rate_f <- factor(dc_rate, levels = c(0,5,10), labels = c('0%','5%','10%'))

data$dc_rate <- dc_rate

data$dc_rate_f <- dc_rate_f
dc_rate 
dc_rate_f

summary(data$dc_rate)
#-------------------------------------------------------------
#-------------------------------------------------------------
#-------------------------------------------------------------
# 할부요인에 대한 지정
#무이자에서는 1,2,3 개월에 집중적으로 포집해있음
#유이자에서는 나머지 개월에 포집 되어있다.

str(data)
data_tmp <- data

tmp <- as.data.frame(table(data_tmp$inst_mon, data_tmp$inst_fee))
tmp
names(tmp) <- c('inst_mon', 'inst_fee', 'inst_tot')
tmp
#할부요인
#inst_tot / 무이자 할부 = 1/ 유이자 할부 = 2/ 일시불  = 3 
# 데이터 별 이상치 제거 
tmp$inst_tot <- c(3, 1, 1, 1, 1, 1 ,1, 1 ,1 ,1 ,1, 1, 3, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2 )
tmp

data_tmp <- merge(data_tmp, tmp, by = c('inst_mon', 'inst_fee'))
data_inst <- data_tmp

data_inst
str(data_inst)
data_pos <- data_inst[data_inst$net_amt>=0,]
# inst_tot에 대하여 팩터형으로 변환 
str(data_pos)

data_pos$inst_tot_f <- as.factor(data_pos$inst_tot)

data_pos$inst_tot_f <- factor(data_pos$inst_tot_f, levels= c(1:3), labels = c('무이자할부', '유이자할부','일시불'))

#-------------------------------------------------------------
#-------------------------------------------------------------
#-------------------------------------------------------------
#시간에 대한 데이서 modification 


date_time_time <- transform(data$sales_time,
                           sales_time_one_hour = sprintf("%04d", data$sales_time))
date_time_time$sales_time_one_hour

date_time_time <- date_time_time$sales_time_one_hour
date_time_time

date_time_time <- substr(date_time_time, 0,2)
date_time_time
