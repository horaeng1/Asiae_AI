library(dplyr)
library(psych)
library(Hmisc)
library(skimr)
library(fBasics)
Sys.setlocale("LC_ALL","korean")#os가 한글이 아닐시에 꼭 써야함


#STORE 명목척도 분석
#STORE 기술통계량
psych::describe(data$str_nm)
Hmisc::describe(data$str_nm)


#지점 간단조회
str(ctg$str_nm)

#지점 변수 팩터형으로 변환
ctg$str_nm <- factor(ctg$str_nm, levels = c('본점','무역점','신촌점','천호점'),
                       labels = c('본점','무역점','신촌점','천호점'))
data_main <- filter(data, ctg$str_nm == '본점')


data_main <- data[which(data$str_nm=='본점'), ]
aggregate(tot_amt ~ part_nm, data_main, sum, trim=0)


str(data_main)

data_main <- data[data$str_nm=="본점",]
data_main
# part_nm / dc_rate / count 
dc_rate <- round((data$dis_amt/data$tot_amt),2)*100
data$dc_rate <- dc_rate
unique(dc_rate)

from <- list(0,c(1:5), c(6:100))
to <- list(0, 5, 10)
from
to
library(doBy)
data$dc_rate_re <- recodeVar(data$dc_rate, src = from, tgt = to)
unique(data$dc_rate_re)

tmp <- table(data$dc_rate_re , data$part_nm)
tmp <- tmp[c(1,2,4),]
tmp

barplot(tmp)
###팩터형으로(factor)
length(unique(data$part_nm))
tmp <- factor(data$part_nm, levels = unique(data$part_nm), labels = c(0:30))
data$part_nm_f <- factor(tmp , levels = c(0:30), labels = unique(data$part_nm))
data$part_nm_f
