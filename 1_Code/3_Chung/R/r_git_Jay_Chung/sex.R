install.packages("dplyr")
library('dplyr')
Sys.setlocale("LC_ALL","korean")# 한글 OS 아닐시 꼭 써야함


data_m <- data[data$gender ==0,]
data_m
data_f <- data[data$gender ==1,]
data_f

ctg_names_m <- c('custid','gender','sales_date','sales_time','str_nm','goodcd','brd_nm','corner_nm','pc_nm','part_nm','team_nm','buyer_nm','import_flg','tot_amt','dis_amt','net_amt','inst_mon','inst_fee')
ctg_m<-data_m[ctg_names_m]
ctg_m

ctg_names_f <- c('custid','gender','sales_date','sales_time','str_nm','goodcd','brd_nm','corner_nm','pc_nm','part_nm','team_nm','buyer_nm','import_flg','tot_amt','dis_amt','net_amt','inst_mon','inst_fee')
ctg_f<-data_f[ctg_names_f]
ctg_f


length(ctg_m$part_nm)
NROW(ctg_m$part_nm)
str(ctg_m$part_nm)

summary(ctg_m$part_nm)

library(psych)
psych::describe(ctg_m$part_nm)

library(Hmisc)
Hmisc::describe(ctg_m$part_nm)
library(skimr)
skim(ctg_m$part_nm)


part_list_m <- data_m[-which(duplicated(data_m$part_nm)),]
part_list_m
customer_0_list <- data_m[-which(duplicated(data_m$custid)),]
customer_0_list
part_list <- part_list$part_nm
data_m$data_part <- factor(data$part_nm, levels = part_list,
                           labels = (1:30))

gender_freq_m_part <- table(ctg_m$part_nm)
gender_freq_m_part

gender_freq_f_part <- table(ctg_f$part_nm)
gender_freq_f_part



par(mfrow = c(2,1))

barplot(gender_freq_m_part,
        main="파트별 남성 인원수:Simple Bar Plot",
        xlab='파트별',ylab='인원')

barplot(gender_freq_f_part,
        main="파트별 여성고객수:Simple Bar Plot",
        xlab='파트별',ylab='인원')

