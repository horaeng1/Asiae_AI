dt <- read.csv(file = 'XXX.csv',
                   header = TRUE, sep = ',',
                   stringsAsFactors = FALSE,
                   strip.white = TRUE,
                   na.strings = c('.','?','NA'))

dt

ctg_names <-c('custid','gender','sales_date','str_nm','goodcd','brd_nm',
              'corner_nm','pc_nm','part_nm','team_nm',
              'buyer_nm','import_flg','inst_fee', 'refund')

ctg_names

ctg<-dt[ctg_names]
head(ctg)

summary(ctg)
str(ctg$str_nm)

#팩터형으로 변환
ctg$gender_f <- factor(ctg$gender, levels = c(0,1),
                       labels=c('M','F'))

ctg$refund_f <- factor(ctg$refund,
                       levels = c(1,0),
                       labels = c('정상매출','환불'))

ctg$refund

summary(ctg$refund_f)                  
ctg$refund_f

#년월 추출
ctg$date <- format(as.Date(ctg$sales_date), "%Y-%m")
ctg$date

(MYlist <- sort(unique(ctg$date)))

ctg$date_f <- factor(ctg$date,
                     levels=c(MYlist),
                     labels=c(MYlist))

summary(ctg$refund_f)

summary(ctg$date_f)

date_ref_freq<-table(ctg$refund_f, ctg$date_f)
date_ref_freq

ref_date_freq<-table(ctg$date_f, ctg$refund_f)
ref_date_freq

par(mfrow=c(2,2))

barplot(date_ref_freq,
        main="연월별 환불 건수 분포비교",
        xlab="연월(year-month)", ylab="환불건수",
        col=c("lightblue","pink"), legend=rownames(date_ref_freq))

barplot(date_ref_freq,
        main="연월별 환불 건수 분포비교",
        xlab="연월(year-month)", ylab="환불건수", beside=TRUE,
        col=c("lightblue","pink"), legend=rownames(date_ref_freq))

barplot(ref_date_freq,
        main="환불여부에 따른\n 연월별 분포비교",
        xlab="환불여부", ylab="연월별 건수",
        col=rainbow(12), legend=rownames(ref_date_freq))

barplot(ref_date_freq,
        main="환불여부에 따른\n 연월별 분포비교",
        xlab="환불여부", ylab="연월별 건수", beside=TRUE,
        col=rainbow(12), legend=rownames(ref_date_freq))
