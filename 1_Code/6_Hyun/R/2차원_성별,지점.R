#-----------------------그래프 만들기 


df_x <-read.csv(file = 'brand_data.csv',na.strings = 'NA')

str(df_x$gender)
df_x$gender_f <- factor(df_x$gender, levels=c(0,1), labels=c('M','F'))
summary(df_x$gender_f)

str(df_x$gender_f)

str(df_x$str_nm)
df_x$str_nm_f <- factor(df_x$str_nm, levels=c('본점','무역점','신촌점','천호점'),
                        labels=c('본점','무역점','신촌점','천호점'))
summary(df_x$str_nm_f)

table(df_x$gender_f,df_x$str_nm_f)

gd_str_freq <- table(df_x$gender_f,df_x$str_nm_f)
gd_str_freq

str_gd_freq <- table(df_x$str_nm_f, df_x$gender_f)
str_gd_freq

addmargins(gd_str_freq)
addmargins(str_gd_freq)

str_gd_prop<- prop.table(str_gd_freq,1)
prop.table(str_gd_freq,2)
prop.table(str_gd_freq)


prop.table(gd_str_freq,1)
prop.table(gd_str_freq,2)
prop.table(gd_str_freq)

addmargins(round(str_gd_prop,3),2)

str_gd_result<-round(str_gd_prop,3)*100
str_gd_result

par(mfrow=c(2,2))
barplot(str_gd_freq, main="성별에 따른 지점 별 환불 빈도수 비교",
        xlab='성별',ylab='지점',col=c(rainbow(4)),legend=rownames(str_gd_freq))

barplot(str_gd_freq, main="성별에 따른 지점 별 환불 빈도수 비교",
        xlab='성별',ylab='지점',beside=TRUE,col=c(rainbow(4)),legend=rownames(str_gd_freq))

barplot(gd_str_freq, main="지점에 따른 성별 환불 빈도수 비교",
        xlab='지점',ylab='성별',col=c(rainbow(4)),legend=colnames(str_gd_freq))

barplot(gd_str_freq, main="지점에 따른 성별 환불 빈도수 비교",
        xlab='지점',ylab='성별',besdie=TRUE, col=c(rainbow(4)),legend=colnames(str_gd_freq))


#---------------------성별, 할인율


str(df_x$tot_amt)

df_x$dis<-round((abs(df_x$dis_amt)/abs(df_x$tot_amt)*100),0)

psych::describe(df_x$dis)
View(df_x)

aggregate(formula=dis~gender_f,data=df_x,FUN = mean,na.rm=TRUE)
aggregate(dis~gender_f,data=df_x,sd,na.rm=TRUE)

install.packages('purrr')
library(purrr)
library(dplyr)
df_x%>% group_by(gender_f)%>%
  dplyr::summarize(Avg=mean(dis),SD=sd(dis))%>%
  arrange(desc(Avg))


par(mfrow=c(1,2))
install.packages("sm")
library(sm)
sm.density.compare(x=df_x$dis,group = df_x$gender_f,xlab="할인율",ylab="밀도",
                   col=c(2,3),lty=c(2,3))
title(main='성별에 따른 할인율 분포비교')
legend(x=100000,y=0.01,legend=levels(df_x$gender_f),col=c(2,3,4),lty=c(2,3,4),bty='n')

boxplot(dis~gender_f, data=df_x,main='성별에 따른 할인율 분포비교',xlab='성별',
        ylab='할인율',col=c(2,3),varwidth=T,notch=T)




