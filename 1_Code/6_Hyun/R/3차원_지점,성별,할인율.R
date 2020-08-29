
df_x <-read.csv(file = 'brand_data.csv',na.strings = 'NA')
install.packages('purrr')
library(purrr)
library(dplyr)
install.packages("sm")
library(sm)


str(df_x$gender)
df_x$gender_f <- factor(df_x$gender, levels=c(0,1), labels=c('M','F'))
summary(df_x$gender_f)
df_x$str_nm_f<-factor(df_x$str_nm, levels=c('무역점','본점','신촌점','천호점'),
                    labels=c('무역점','본점','신촌점','천호점'))

df_x$str_nm_f
#---------------------성별, 할인율그래프 만들기


str(df_x$tot_amt)

#할인율 계산
df_x$dis<-round((abs(df_x$dis_amt)/abs(df_x$tot_amt)*100),0)

#할인여부 컬럼 만들기
df_x$dis_ox[df_x$dis==0]<-0
df_x$dis_ox[df_x$dis!=0]<-1

df_x$dis_oxx <- factor(df_x$dis_ox, levels=c(0,1), labels=c('X','O'))

View(df_x)

aggregate(formula=dis_ox~gender_f,data=df_x,FUN = mean,na.rm=TRUE)
aggregate(dis~gender_f+str_nm_f,data=df_x,mean,na.rm=TRUE)



df_x%>% group_by(gender_f)%>%
  dplyr::summarize(Avg=mean(dis),SD=sd(dis))%>%
  arrange(desc(Avg))
install.packages("ggplot2")
library(ggplot2)

aggregate(dis~gender+str_nm_f,df_x,mean,na.rm=TRUE,trim=0.05)




p1 <- ggplot(df_x,aes(gender_f,dis,color=str_nm_f, shape=str_nm_f))+
  geom_boxplot()+labs(title="성별 할인율에 따른 환불 유무를 지점 별로 세분화해 비교분석")
p1

p2 <- ggplot(df_x,aes(str_nm_f,dis))+geom_boxplot()+facet_wrap(~gender_f)+
  labs(title="성별 할인율에 따른 환불 유무를 지점 별로 세분화해 비교분석")
p2

p3 <- ggplot(df_x,aes(dis,str_nm_f,color=gender_f,shape=gender_f))+
  geom_boxplot()+labs(title='성별 할인율에 따른 환불 유무를 지점 별로 세분화해 비교분석')
p3

p4 <- ggplot(df_x,aes(gender_f,dis))+geom_boxplot()+facet_wrap(~str_nm_f)+
  labs(title="성별 할인율에 따른 환불 유무를 지점 별로 세분화해 비교분석")
p4

library(gridExtra)
grid.arrange(p1,p2,p3,p4, nrow=2,ncol=2)


#-------------할인여부,성별,지점 별 그래프


p1 <- ggplot(df_x,aes(gender_f,dis_oxx,color=str_nm_f, shape=str_nm_f))+
  geom_boxplot()+labs(title="성별 할인여부에 따른 환불 유무를 지점 별로 세분화해 비교분석")
p1

p2 <- ggplot(df_x,aes(str_nm_f,dis_oxx))+geom_boxplot()+facet_wrap(~gender_f)+
  labs(title="성별 할인여부에 따른 환불 유무를 지점 별로 세분화해 비교분석")
p2

p3 <- ggplot(df_x,aes(dis_oxx,str_nm_f,color=gender_f,shape=gender_f))+
  geom_boxplot()+labs(title='성별 할인율에 따른 환불 유무를 지점 별로 세분화해 비교분석')
p3

p4 <- ggplot(df_x,aes(gender_f,dis_oxx))+geom_boxplot()+facet_wrap(~str_nm_f)+
  labs(title="성별 할인율에 따른 환불 유무를 지점 별로 세분화해 비교분석")
p4

library(gridExtra)
grid.arrange(p1,p2,p3,p4, nrow=2,ncol=2)
