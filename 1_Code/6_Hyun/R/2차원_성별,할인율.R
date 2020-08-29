train1 <- read.csv('Y_train.csv', sep = ',', header = T, stringsAsFactors = FALSE)
train2 <- read.csv('X_train.csv', sep = ',', header = T, stringsAsFactors = FALSE)

# join을 위한 패키지 설치
install.packages('dplyr')
library(dplyr)

# custid 기준으로 Y_train + X_train full_join하기
df_x <- full_join(train1,train2,by = 'custid')


# 'part_nm'컬럼 값 오타수정 및 통일화
df_x$part_nm <- recode (df_x$part_nm,
                      '케주얼,구두,아동' = '캐주얼',
                      '골프/유니캐쥬얼' = '스포츠캐주얼',
                      '스포츠캐쥬얼' = '스포츠캐주얼', 
                      '영라이브' = '영플라자',
                      '영어덜트캐쥬얼' = '영플라자',
                      '영캐릭터' = '영플라자',
                      '남성정장스포츠' = '남성의류',
                      '여성캐주얼' = '여성의류',
                      '여성정장' = '여성의류',
                      '여성캐쥬얼' = '여성의류',
                      '여성의류파트' = '여성의류',
                      '아동,스포츠' = '아동',
                      '아동문화' = '아동',
                      '로얄부틱' = '명품잡화',
                      '로얄부띠끄' = '명품잡화',
                      '잡화파트' = '패션잡화',
                      '잡화' = '패션잡화',
                      '생식품파트' = '생식품',
                      '가정용품파트' = '가정용품',
                      '공산품파트' = '공산품')                         


#---------------------성별, 할인율그래프 만들기

View(df_x)

df_x$refund<-ifelse(df_x$tot_amt < 0, 1, 0)
str(df_x$tot_amt)

#할인율 계산
df_x$dis<-round((abs(df_x$dis_amt)/abs(df_x$tot_amt)*100),0)


psych::describe(df_x$dis_ox)
View(df_x)

aggregate(formula=dis~refund,data=df_x,FUN = mean,na.rm=TRUE)
aggregate(dis~refund,data=df_x,sd,na.rm=TRUE)



#





install.packages('purrr')
library(purrr)
library(dplyr)
df_x%>% group_by(refund)%>%
  dplyr::summarize(Avg=mean(dis),SD=sd(dis))%>%
  arrange(desc(Avg))


par(mfrow=c(2,2))
install.packages("sm")
library(sm)
sm.density.compare(x=df_x$dis,group = df_x$refund,xlab="할인율",ylab="환불",
                   col=c('blue','red'),lty=c(2,3))
title(main='할인율에 따른 환불 빈도수 분포비교')
legend(x=100000,y=0.01,legend=levels(df_x$refund),col=c(2:7),lty=c(2:7),bty='n')

boxplot(dis~refund, data=df_x,main='할인율에 따른 환불 빈도 분포비교',xlab='환불',
        ylab='할인율',col=c('lightblue','pink'),varwidth=T,notch=T)


#할인여부에 따른 성별 환불 

#할인여부 컬럼 만들기
df_x$dis_ox[df_x$dis==0]<-0
df_x$dis_ox[df_x$dis!=0]<-1

df_x$dis_oxx <- factor(df_x$dis_ox, levels=c(0,1), labels = c('할인X','할인O'))
View(df_x)


table(df_x$gender_f,df_x$dis_oxx)


gd_dis_freq <- table(df_x$gender_f,df_x$dis_oxx)
gd_dis_freq

dis_gd_freq <- table(df_x$dis_oxx, df_x$gender_f)
dis_gd_freq

par(mfrow=c(2,2))
barplot(gd_dis_freq, main="할인 여부에 따른 남녀 환불 빈도수 비교",
        xlab='할인여부',ylab='환불빈도수',col=c('lightblue','pink'),legend=rownames(gd_dis_freq))

barplot(gd_dis_freq, main="할인 여부에 따른 남녀 환불 빈도수 비교교",
        xlab='할인여부',ylab='환불빈도수',beside=TRUE,col=c('lightblue','pink'),legend=rownames(gd_dis_freq))

barplot(dis_gd_freq, main="할인 여부에 따른 남녀 환불 빈도수 비교",
        xlab='성별',ylab='환불빈도수',col=c('lightgreen','orange'),legend=rownames(dis_gd_freq))


barplot(dis_gd_freq, main="할인 여부에 따른 남녀 환불 빈도수 비교",
        xlab='성별',ylab='환불빈도수',beside=TRUE, col=c('lightgreen','orange'),legend=rownames(dis_gd_freq))



