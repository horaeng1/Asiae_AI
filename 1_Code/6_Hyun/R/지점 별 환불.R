my<-read.csv(file='XY_train.csv',
             header = TRUE, sep = ',',
             stringsAsFactors = FALSE,
             strip.white = TRUE,
             na.strings = c(',','?','NA'))

ctg_names <- c('custid','gender','str_nm','goodcd','brd_nm','corner_nm','pc_nm',
               'part_nm','team_nm','buyer_nm','import_flg','inst_fee')

ctg<-my[ctg_names]

head(ctg)
str(ctg)
summary(ctg)
str(ctg$gender)
ctg$gender_f <- factor(ctg$gender,levels=c(0,1),labels=c('M','F'))
summary(ctg$gender_f)
str(ctg$str_nm)
ctg$str_nm_f<-factor(ctg$str_nm, levels=c('무역점','본점','신촌점','천호점'),
                     labels=c('무역점','본점','신촌점','천호점'))
summary(ctg$str_nm_f)

table(ctg$gender,ctg$str_nm)
table(ctg$gender_f,ctg$str_nm_f)

gd_ag_freq <-table(ctg$gender_f,ctg$str_nm_f)
gd_ag_freq

str_ag_freq <- table(ctg$str_nm_f,ctg$gender_f)
str_ag_freq 

addmargins(gd_ag_freq)
addmargins(gd_ag_freq,1)
addmargins(gd_ag_freq,2)

prop.table(gd_ag_freq,1) #지점에서 성별 분포비율 비교
prop.table(gd_ag_freq,2) #성별에서 지점 분포비율 비교

gd_ag_prop <- prop.table(gd_ag_freq,1)
gd_ag_prop

addmargins(round(gd_ag_prop,3),2) 
gd_ag_result <- round(gd_ag_prop,3)*100
gd_ag_result

addmargins(gd_ag_result,2)

barplot(gd_ag_prop,main="지점")


my$refund[my$net_amt<0]<-'0'
my$refund[my$net_amt>0]<-"1"
my
head(my[c('net_amt','refund')])

View(my)

write.csv(my, file='real_sibal.csv', row.names = FALSE)



sb<-read.csv(file='real_sibal.csv',
             header = TRUE, sep = ',',
             stringsAsFactors = FALSE,
             strip.white = TRUE,
             na.strings = c(',','?','NA'))
str(sb$refund)
sb$refund_f <- factor(sb$refund,levels=c(0,1), labels=c('O','X'))
summary(sb$refund_f)


str(sb$str_nm)
sb$str_nm_f <- factor(sb$str_nm,levels=c('무역점','본점','신촌점','천호점'),
                      labels=c('무역점','본점','신촌점','천호점'))
summary(sb$str_nm_f)

ref_str_freq<-table(sb$refund_f,sb$str_nm_f)
ref_str_freq

str_ref_freq <- table(sb$str_nm_f,sb$refund_f)
str_ref_freq

str_ref_freq_sum <- addmargins(str_ref_freq)
str_ref_freq_sum

str_ref_prop <- prop.table(str_ref_freq,1)
str_ref_prop

addmargins(round(str_ref_prop,3),2)

str_ref_result <- round(str_ref_prop,3)*100
str_ref_result

addmargins(str_ref_result,2)

par(mfrow=c(2,2))
barplot(str_ref_freq,main='환불 건수 당 지점 분포율 비교',
        xlab='지점',ylab='환불 건수',col=c("lightblue",'pink','lightgreen','yellow'),beside=TRUE,legend=rownames(str_ref_freq))

barplot(str_ref_freq,main='환불 건수 당 지점 분포율 비교',
        xlab='지점',ylab='환불 건수', col=c("lightblue",'pink','lightgreen','yellow'),legend=rownames(str_ref_freq))

barplot(ref_str_freq,main='지점 별 환불 건수 비교',
        xlab='지점',ylab='환불 건수',col=c("lightblue",'pink'),beside=TRUE,legend=rownames(ref_str_freq))

barplot(ref_str_freq,main='지점 별 환불 건수 비교',
        xlab='지점',ylab='환불 건수', col=c("lightblue",'pink'),legend=rownames(ref_str_freq))

