
sb<-read.csv(file='brand_data.csv'
             header = TRUE, sep = ',',
             stringsAsFactors = FALSE,
             strip.white = TRUE,
             na.strings = c(',','?','NA'))
library('psych')
library('Hmisc')
library(skimr)
install.packages("ggplot2")
library(ggplot2)



sb$gender_f <- factor(sb$gender,levels=c(0,1), labels=c('M','F'))
str(sb$gender_f)
sb$str_nm_f<-factor(sb$str_nm, levels=c('무역점','본점','신촌점','천호점'),
                     labels=c('무역점','본점','신촌점','천호점'))

sb$Month_Yr <- format(as.Date(sb$sales_date), "%m")
sb$sales_date_f <- factor(sb$Month_Yr, levels=c('01','02','03','04','05','06','07','08','09','10','11','12'), labels=c(1,2,3,4,5,6,7,8,9,10,11,12))




#sb$date<-as.numeric(sb$Month_Yr)

View(sb)
  sb


psych::describe(sb$date)

aggregate(gender_f~date+str_nm_f,sb,sum,trim=0.05,na.rm=TRUE)




bb<-aggregate(gender_f~str_nm_f+date,sb,mean,trim=0.05,na.rm=TRUE)
View(bb)


p1 <- ggplot(sb,aes(gender_f,sales_date_f,color=str_nm_f, shape=str_nm_f))+
  geom_boxplot()+labs(title="월별,지점별, 남녀별 세분화 비교분석")
p1

p2 <- ggplot(sb,aes(str_nm_f,sales_date_f))+geom_boxplot()+facet_wrap(~gender_f)+
  labs(title="월별,지점별, 남녀별 세분화 비교분석")
p2

p3 <- ggplot(sb,aes(sales_date_f,str_nm_f,color=str_nm_f,shape=str_nm_f))+
  geom_boxplot()+labs(title='월별,지점별, 남녀별 세분화 비교분석')
p3

p4 <- ggplot(sb,aes(gender_f,sales_date_f))+geom_boxplot()+facet_wrap(~str_nm_f)+
  labs(title="월별,지점별, 남녀별 세분화 비교분석")
p4

library(gridExtra)
  grid.arrange(p1,p2,p3,p4, nrow=2,ncol=2)



  
  

