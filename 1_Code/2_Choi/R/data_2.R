install.packages('ggplot2')
library(dplyr)
library(psych)
library(Hmisc)
library(skimr)
library(fBasics)
library(ggplot2)
Sys.setlocale("LC_ALL","korean")#os가 한글이 아닐시에 꼭 써야함


#STORE 명목척도 분석
#STORE 기술통계량
psych::describe(data$str_nm)
Hmisc::describe(data$str_nm)


#지점 간단조회
str(ctg$str_nm)

#지점 변수 팩터형으로 변환
ctg_names <- c('custid','gender','sales_date','sales_time','str_nm','goodcd','brd_nm','corner_nm','pc_nm','part_nm','team_nm','buyer_nm','import_flg','tot_amt','dis_amt','net_amt','inst_mon','inst_fee')
ctg_names
ctg<-data[ctg_names]
ctg

ctg$str_nm <- factor(ctg$str_nm, levels = c('본점','무역점','신촌점','천호점'),
                       labels = c('본점','무역점','신촌점','천호점'))
data_main <- filter(data, ctg$str_nm == '본점')


data_main <- data[which(data$str_nm=='본점'), ]
aggregate(dis_amt ~ part_nm, data_main, sum, trim=0)


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

#3차원 변수간 요약집계 분석
aggregate(dis_amt~dc_rate,data,
          sum, na.rm = TRUE, trim=0)
aggregate(dis_amt~dc_rate+part_nm,data,
          sum, na.rm = TRUE, trim=0)
par(mfrow=c(1,1))
p1 <- ggplot(data,aes(dis_amt, part_nm, colors = dc_rate, shape = dc_rate))+
  geom_boxplot()+
  labs(title = "파트네임 Vs 할인액")
p1
plot(cnt$dc_rate_f~ctg$part_nm_f, data=ctg,
     main="dddd",
     xlab="dd",ylab="dddd",las=2,
     col=rainbow(length(levels(ctg$part_nm_f))))

ggplot(data, aes(ctg$part_nm_f, cnt$tot_amt, color = cnt$dc_rate_f, shape = cnt$dc_rate_f))+
  geom_boxplot()+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  labs(title = "할인율에 따른 지출금액 분포를 카테고리별로 세분화해 비교분석")
