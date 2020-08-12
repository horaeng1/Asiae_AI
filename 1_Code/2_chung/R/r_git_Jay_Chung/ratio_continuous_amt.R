str(data$brd_nm)
str(data_m$brd_nm)
str(data_f$brd_nm)
data_brd <- list(data$brd_nm)
install.packages(c('dplyr','psych','Hmisc','skimr'))
library(dplyr)
library(psych)
library(Hmisc)
library(skimr)


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

###비율형 척도 변수컬럼 요약집계

#산술평균
mean(data$tot_amt)
mean(data$dis_amt)
mean(data$net_amt)

#중간값
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

#variance
var(ctg$tot_amt)
var(ctg)

