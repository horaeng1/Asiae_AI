re<-read.csv(file='sibal.csv',
             header = TRUE, sep = ',',
             stringsAsFactors = FALSE,
             strip.white = TRUE,
             na.strings = c(',','?','NA'))
View(re)

# 데이터명 변경하기
dis$part_nm[dis$part_nm=="아동,스포츠"]<-"아동"

# csv 파일 저장하기
write.csv(ctg,"C:/Users/ASIAE_22/Documents/R/project/sibal.csv")

# 남여
re$gender_f<-factor(re$gender,
       levels = c(0,1),
       labels = c("남","여"))
summary(re$gender_f)

re$Month_f <- format(as.Date(re$sales_date), "%Y-%m")

t<-unique(re$Month_f)
View(t)
        
re$month<-factor(re$Month_f,levels = c(sort(t)),labels = c(sort(t)))
summary(re$month)                    

# 남/여 월별 환불건수
table(re$gender_f,re$month)
table(re$gender_f,re$month,useNA = 'ifany')

gd_ag_freq<-table(re$gender_f,re$month)
gd_ag_freq

table(re$month,re$gender_f)

# 교차빈도분석 부분합 계산하기
addmargins(gd_ag_freq)
addmargins(gd_ag_freq,1)
addmargins(gd_ag_freq,2)

gd_ag_freq_sum<-addmargins(gd_ag_freq,2)
gd_ag_freq_sum

# 각 월별에서 성별 분포비율 비교
prop.table(gd_ag_freq,1)

# 각 성별에서 월별 분포비율 비교
prop.table(gd_ag_freq,2)

gd_ag_prop<-prop.table(gd_ag_freq,1)
gd_ag_prop

addmargins(round(gd_ag_prop,3),2)

gd_ag_result<-round(gd_ag_prop,3)*100
gd_ag_result

addmargins(gd_ag_result,2)

par(mfrow=c(2,2))
barplot(gd_ag_freq,
        main="백화점 월별 따른 성별 분포비교",
        xlab="연령대(age)",ylab="월별",
        col=c("lightblue","pink"), legend=rownames(gd_ag_freq))

