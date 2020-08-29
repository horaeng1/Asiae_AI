# tidyr 패키지 설치
#install.packages('tidyr')
library('tidyr')

# dataframe에 csv파일들 넣기
train1 <- read.csv('Y_train.csv', sep = ',', header = T, stringsAsFactors = FALSE)
train2 <- read.csv('X_train.csv', sep = ',', header = T, stringsAsFactors = FALSE)

summary(train2)

View(train2)
# join을 위한 패키지 설치
#install.packages('dplyr')
library(dplyr)

# custid 기준으로 Y_train + X_train full_join하기
sb <- full_join(train1,train2,by = 'custid')

ctg<-sb[c('custid','gender','sales_time','goodcd','import_flg','tot_amt','dis_amt','net_amt','inst_mon','inst_fee')]
round(cor(ctg),3)
temp<-cor(ctg)
round(temp,3)

#범주형변수 특성파악
ctg_name <- c('custid','gender','str_nm','brd_nm','part_nm','import_flg','inst_fee')
ctg1<-sb[ctg_name]
head(ctg1)
str(ctg1)
summary(ctg1)

all_names <- names(sb)
ctg_names <- c('custid','gender','str_nm','brd_nm','part_nm','import_flg','inst_fee')
cnt_names <- setdiff(all_names,ctg_names)

str(ctg_names)
ctg <- c('str_nm','custid')

plot(sb$inst_fee,sb$custid)
cor(sb$inst_fee,sb$custid, method='spearman')

str(cnt_names)

cor(sb$tot_amt,sb$dis_amt, method = 'spearman')
plot(sb$tot_amt,sb$dis_amt)

cor(ctg_names,cnt_names, method='spearman')

cor(sb$custid,sb$gender,sb$import_flg,sb$inst_fee,sb$sales_date,sb$sales_time,
    sb$tot_amt,sb$dis_amt,sb$net_amt,sb$inst_mon,method='spearman')

str(all_names)

ctg_name_1 <- c('sales_date','sales_time','tot_amt','dis_amt','net_amt','inst_mon')
ctg2 <- sb[ctg_name_1]
head(ctg2)
str(ctg2)
summary(ctg2)

tot<-subset(sb, tot_amt>0)
# 1:가정용품, 2:공산품, 3:남성의류, 4:명품잡화, 
# 5:생식품, 6:스포츠캐주얼, 7:아동, 8:여성의류, 
# 9:영플라자, 10:캐주얼, 11:패션잡화
# 12: 상품개발영업1과, 13: 인터넷백화점


# part_nm_num: Part_nm 숫자화한 변수


#tot$part_nm_num[tot$part_nm=='가정용품']<-1
#tot$part_nm_num[tot$part_nm=='공산품']<-2
#tot$part_nm_num[tot$part_nm=='남성의류']<-3
#tot$part_nm_num[tot$part_nm=='명품잡화']<-4
#tot$part_nm_num[tot$part_nm=='생식품']<-5
#tot$part_nm_num[tot$part_nm=='스포츠캐주얼']<-6
#tot$part_nm_num[tot$part_nm=='아동']<-7
#tot$part_nm_num[tot$part_nm=='여성의류']<-8
#tot$part_nm_num[tot$part_nm=='영플라자']<-9
#tot$part_nm_num[tot$part_nm=='캐주얼']<-10
#tot$part_nm_num[tot$part_nm=='패션잡화']<-11
#tot$part_nm_num[tot$part_nm=='상품개발영업1과']<-12
#tot$part_nm_num[tot$part_nm=='인터넷백화점']<-13

# part_nm 변수 팩터형으로 변환
#tot$part_nm_f<-factor(tot$part_nm_num,levels = c(seq(13)),
#                     labels = c('가정용품','공산품','남성의류','명품잡화', '생식품','스포츠캐주얼','아동','여성의류','영플라자', '캐주얼', '패션잡화','상품개발영업1과','인터넷백화점'))

# part_nm 변수 팩터형으로 변환
#tot$part_nm_f<-factor(tot$part_nm,levels = c(unique(tot$part_nm)),
#                     labels = c(seq(31)))


tot$part_nm_f<-factor(tot$part_nm,levels = c("명품잡화", "잡화파트", "남성의류", "골프/유니캐쥬얼", "로얄부띠끄",
                                             "케주얼,구두,아동", "여성캐주얼", "여성의류파트", "가정용품", 
                                             "가정용품파트", "남성정장스포츠", "아동", "잡화", "영어덜트캐쥬얼",
                                             "영라이브", "공산품", "스포츠캐쥬얼", "여성정장", "아동문화", 
                                             "공산품파트", "생식품파트", "영플라자", "생식품", "패션잡화",
                                             "아동,스포츠", "로얄부틱", "여성캐쥬얼", "스포츠캐주얼", "영캐릭터", 
                                             "상품개발영업1과", "인터넷백화점"),
                      labels = c(1:31))
View(tot$part_nm_f)

View(tot)

#View(sb)

# part_nm 기술통계량
Hmisc::describe(tot$part_nm_f)

# tot_amt 기술통계량
psych::describe(tot$tot_amt) 

# stats::aggregate() 함수이용 요약집계
aggregate(formula = tot_amt ~ part_nm, data = tot, FUN = mean, na.rm = TRUE)
aggregate(tot_amt ~ part_nm_f, tot, mean, na.rm=TRUE, trim=0.05)
aggregate(tot_amt ~ part_nm_f, tot, sd, na.rm=TRUE)

# purrr::파이프연산자 %>%, dplyr::데이터가공함수 이용 요약집계

#install.packages('purrr')
library(purrr)
library(dplyr)

tot %>% group_by(part_nm_f) %>% dplyr::summarise(Avg=mean(tot_amt),SD=sd(tot_amt)) %>% arrange(desc(Avg))

par(mfrow=c(1,1))

# sm::sm.density.compare()를 사용한 밀도그래프

#install.packages("sm")
library(sm)
View(tot)
str(tot)
tot$tot_amt_num <- as.numeric(tot$tot_amt)
tot$part_nm_f

tmp <- tot[tot$part_nm_f==1 | tot$part_nm_f==2 | tot$part_nm_f==3 | tot$part_nm_f==4 | tot$part_nm_f==5|
           tot$part_nm_f==6 | tot$part_nm_f==7 | tot$part_nm_f==8 | tot$part_nm_f==9 | tot$part_nm_f==10|
           tot$part_nm_f==11 | tot$part_nm_f==12 | tot$part_nm_f==13 | tot$part_nm_f==14 | tot$part_nm_f==15|
           tot$part_nm_f==16 | tot$part_nm_f==17 | tot$part_nm_f==18 | tot$part_nm_f==19 | tot$part_nm_f==20|
           tot$part_nm_f==21 | tot$part_nm_f==22 | tot$part_nm_f==23 | tot$part_nm_f==24 | tot$part_nm_f==25|
           tot$part_nm_f==26 | tot$part_nm_f==27 | tot$part_nm_f==28 | tot$part_nm_f==29,]

tmp

sm.density.compare(x=tmp$tot_amt_num, group=tmp$part_nm_f,xlab="매출액(tot_amt)", ylab="밀도",
                   col=c(2:30), lty=c(2:30))
#d <- density(tot$tot_amt) 
#plot(d)
#polygon(d, col="pink", border="blue")

title(main="제품군에 따른 매출액 분포비교")

legend("topright",levels(tot$part_nm_f), fill = 2+(0:29))

# boxplot()를 사용한 그래프
boxplot(tot_amt ~ part_nm_f, data = tot,
        main="제품군에 따른 매출액 분포비교",
        xlab="제품군(part_nm)", ylab="매출액(매출액(tot_amt)",
        col=c(2,32),varwidth=T, notch=T)

par(mfrow=c(1,1))

library(ggplot2)

# 멀티프레임에 배치할 개별 그래프 준비
# 단순 플로팅 그래프
par(mfrow=c(2,2))

p1 <- ggplot(tot,aes(part_nm_f, tot_amt))+
  geom_point(color = "black", shape=20, size=2)
p1

# 모든 데이터를 플로팅하는 형태로 분포모양을 나타냄
p2 <- ggplot(tot, aes(part_nm_f, tot_amt))+
  geom_jitter(color="black", shape=8, size=0.8)
p2

# 박스플롯 형태로 분포모양을 나타냄
p3 <- ggplot(tot, aes(part_nm_f, tot_amt))+
  geom_boxplot(fill="lightblue",
               outlier.color="orange", outlier.shape=17,
               outlier.size=2, notch = TRUE)
p3

# 바이올린 플롯형태로 분포모양을 나타냄
p4 <- ggplot(tot, aes(part_nm_f,tot_amt))+
  geom_violin(fill="lightpink")
p4

# gridExtra::grid.arrange() 함수 이용
# ggplot2용 멀티프레임 생성
library(gridExtra)
grid.arrange(p1,p2,p3,p4,nrow=2, ncol=2)


# 여름: 05,06,07
# 가을: 08,09,10
# 겨울: 11,12,01
# 봄: 02,03,04

tot$sales_date_num <- format(as.Date(tot$sales_date), "%m")
date<-unique(as.numeric(sort(tot$sales_date_num)))
str(date)
View(date)

View(tot)
str(tot$sales_date_num)


tot$season[tot$sales_date_num %in% c("02","03","04")]<-"봄"
tot$season[tot$sales_date_num %in% c("05","06","07")]<-"여름"
tot$season[tot$sales_date_num %in% c("08","09","10")]<-"가을"
tot$season[tot$sales_date_num %in% c("11","12","01")]<-"겨울"

tot$season_f<-factor(tot$season,levels = c(unique(tot$season)),
                        labels = c('봄','여름','가을','겨울'))
str(tot$season)

aggregate(tot_amt ~ part_nm_f + season_f, tot,
          mean, na.rm=TRUE,trim=0.05)

aggregate(tot_amt ~ season_f + part_nm_f, tot,
          mean, na.rm=TRUE, trim=0.05)

p1 <- ggplot(tot, aes(part_nm_f,tot_amt,color=season_f,shape=season_f))+
  geom_boxplot()+
  labs(title = "제품군에 따른 매출에 분포를 시즌별로 세분화해 비교분석")
p1

p2 <- ggplot(tot, aes(part_nm_f,tot_amt))+
  geom_boxplot()+facet_wrap(~season_f)+
  labs(title="제품군에 따른 매출액 분포를 시즌별로 분할그래프로 비교분석")
p2

p3 <- ggplot(tot, aes(season_f,tot_amt,color=part_nm_f, shape=part_nm_f))+
  geom_boxplot()+
  labs(title = "시즌에 따른 매출액 분포를 제품군별로 세분화해 비교분석")
p3

p4 <- ggplot(tot, aes(season_f,tot_amt))+geom_boxplot()+
  facet_wrap(~ part_nm_f)+
  labs(title = "시즌에 따른 매출액 분포를 제품군별로 분할그래프로 비교문석")
p4

# gridExtra::grid.arrange() 함수이용
# ggplot2용 멀티프레임 생성
library(gridExtra)
grid.arrange(p1,p2,p3,p4, nrow=2,ncol=2)

# NA관련 함수이용 결측치 포함유무 확인
anyNA(tot)
is.na(tot)
sapply(tot,anyNA)

install.packages('DescTools')
library(DescTools)


boxplot(tot[c('tot_amt')])

tot$tot_amt_2 <- Winsorize(tot$tot_amt)

boxplot(tot[c('tot_amt_2')])

boxplot.stats(tot$tot_amt_2)

tot$tot_amt_2 <-ifelse(tot$tot_amt_2 > 2000000, yes=2000000,
                         no=tot$tot_amt_2)

boxplot(tot_amt_2 ~ part_nm_f, data = tot,
        main="제품군에 따른 매출액 분포비교",
        xlab="제품군(part_nm)", ylab="매출액(매출액(tot_amt)",
        col=c('lightgrey'),varwidth=T, notch=T)

p1 <- ggplot(tot, aes(part_nm_f,tot_amt_2,color=season_f,shape=season_f))+
  geom_boxplot()+
  labs(title = "제품군에 따른 매출에 분포를 시즌별로 세분화해 비교분석")
p1

p2 <- ggplot(tot, aes(part_nm_f,tot_amt_2))+
  geom_boxplot()+facet_wrap(~season_f)+
  labs(title="제품군에 따른 매출액 분포를 시즌별로 분할그래프로 비교분석")
p2

p3 <- ggplot(tot, aes(season_f,tot_amt_2,color=part_nm_f, shape=part_nm_f))+
  geom_boxplot()+
  labs(title = "시즌에 따른 매출액 분포를 제품군별로 세분화해 비교분석")
p3

p4 <- ggplot(tot, aes(season_f,tot_amt_2))+geom_boxplot()+
  facet_wrap(~ part_nm_f)+
  labs(title = "시즌에 따른 매출액 분포를 제품군별로 분할그래프로 비교문석")
p4

grid.arrange(p1,p2,p3,p4, nrow=2,ncol=2)

View(tot)

# 환불 한 남녀의 비율
tot$gender_f<-factor(tot$gender,levels = c(0,1),
                    labels = c('M','F'))

# gender 기술통계량
Hmisc::describe(tot$gender_f)

# tot_amt 기술통계량
psych::describe(tot$tot_amt) 

# stats::aggregate() 함수이용 요약집계
aggregate(formula = tot_amt ~ gender, data = tot, FUN = mean, na.rm = TRUE)
aggregate(tot_amt ~ gender_f, tot, mean, na.rm=TRUE, trim=0.05)
aggregate(tot_amt ~ gender_f, tot, sd, na.rm=TRUE)

tot %>% group_by(gender_f) %>% dplyr::summarise(Avg=mean(tot_amt),SD=sd(tot_amt)) %>% arrange(desc(Avg))

par(mfrow=c(1,1))

install.packages("sm")
library(sm)
sm.density.compare(x=tot$tot_amt, group = tot$gender_f,
                   xlab="매출액(tot_amt)", ylab="밀도",
                   col=c(2,3), lty=c(2,3))

title(main="성별에 따른 매출액 분포비교교")

legend(x=100000, y=0.01, legend = levels(tot$gender_f),
       col=c(2,3),lty=c(2,3),bty="n")

legend("topright",levels(tot$gender_f), fill = 2+(0:nlevels(tot$gender_f)))

# boxplot()를 사용한 그래프
boxplot(tot_amt ~ gender_f, data = tot,
        main="성별에 따른 매출액 분포비교",
        xlab="성별(gender)", ylab="매출액(tot_amt)",
        col=c(2,3),varwidth=T, notch=T)

par(mfrow=c(1,1))

library(ggplot2)

# 멀티프레임에 배치할 개별 그래프 준비
# 단순 플로팅 그래프
par(mfrow=c(2,2))

p1 <- ggplot(tot,aes(gender_f, tot_amt))+
  geom_point(color = "black", shape=20, size=2)
p1

# 모든 데이터를 플로팅하는 형태로 분포모양을 나타냄
p2 <- ggplot(tot, aes(gender_f, tot_amt))+
  geom_jitter(color="black", shape=8, size=0.8)
p2

# 박스플롯 형태로 분포모양을 나타냄
p3 <- ggplot(tot, aes(gender_f, tot_amt))+
  geom_boxplot(fill="lightblue",
               outlier.color="orange", outlier.shape=17,
               outlier.size=2, notch = TRUE)
p3

# 바이올린 플롯형태로 분포모양을 나타냄
p4 <- ggplot(tot, aes(gender_f,tot_amt))+
  geom_violin(fill="lightpink")
p4

# gridExtra::grid.arrange() 함수 이용
# ggplot2용 멀티프레임 생성
library(gridExtra)
grid.arrange(p1,p2,p3,p4,nrow=2, ncol=2)

p1 <- ggplot(tot, aes(gender_f,tot_amt_2,color=season_f,shape=season_f))+
  geom_boxplot()+
  labs(title = "성별에 따른 매출액에 분포를 시즌별로 세분화해 비교분석")
p1

p2 <- ggplot(tot, aes(gender_f,tot_amt_2))+
  geom_boxplot()+facet_wrap(~season_f)+
  labs(title="성별에 따른 매출액 분포를 시즌별로 분할그래프로 비교분석")
p2

p3 <- ggplot(tot, aes(season_f,tot_amt_2,color=gender_f, shape=gender_f))+
  geom_boxplot()+
  labs(title = "시즌에 따른 매출액 분포를 성별로 세분화해 비교분석")
p3

p4 <- ggplot(tot, aes(season_f,tot_amt_2))+geom_boxplot()+
  facet_wrap(~ gender_f)+
  labs(title = "시즌에 따른 매출액 분포를 성별로 분할그래프로 비교문석")
p4

grid.arrange(p1,p2,p3,p4, nrow=2,ncol=2)

# 1: 무역점, 2: 본점, 3: 천호점, 4: 신촌점

tot$str_nm_num[tot$str_nm=='무역점']<-1
tot$str_nm_num[tot$str_nm=='본점']<-2
tot$str_nm_num[tot$str_nm=='천호점']<-3
tot$str_nm_num[tot$str_nm=='신촌점']<-4

# 지점변수(str_nm)팩터화 (1: 무역점, 2: 본점, 3: 천호점, 4: 신촌점)
tot$str_nm_f<-factor(tot$str_nm,levels = c(seq(4)),
                     labels = c('무역점','본점','천호점','신촌점'))

#class(tot$str_nm)
(tot$str_nm_f <- as.factor(tot$str_nm))
#str(tot$str_nm_f)
View(tot)

# 브랜드 카운트



brd_g<- tot%>% group_by(tot$brd_nm) %>% summarise(n = n())
brd_g

brd_g <- data.frame(brd_g)
class(brd_g)

View(tot)

a<- tot%>%group_by(tot$brd_nm) %>% summarise(n=unique("brd_nm"))
View(a)

a<- unique(c(tot$str_nm_f=="무역점"), c("brd_nm"))
a

tot$str_nm_f=="무역점"


str(brd_g)

tot$tot.brd_nm <- tot$brd_nm

View(tot)



# 기술통계분석 패키지 동시설치
install.packages(c('psych', 'Hmisc', 'skimr'))
library(psych)
library(Hmisc)
library(skimr)

# 할인율 컬럼 만들기(dis_rate=dis_amt/tot_amt*100)
refund$dis_rate <- round(refund$dis_amt/refund$tot_amt*100,0)
#View(refund)
#unique(refund$dis_rate)

#--------------------------------------------------------------


# 매출액(net_amt)변수 기본특성 파악
str(abs(sb$net_amt))

# 매출액 변수 간단기술통계
library(psych)
psych::describe(abs(sb$net_amt))

#할인율 계산
sb$dis_rate<-round((abs(sb$dis_amt)/abs(sb$tot_amt)*100),0)

# 할인율(dis_rate)변수 기본특성 파악
str(sb$dis_rate)

# 할인율(dis_rate)변수 간단기술통계
psych::describe(sb$dis_rate)

# 할인율(dis_rate)에 따른 매출액 변수 간 관련성 파악
# 공분산(covariance) 분석

var(sb$dis_rate, abs(sb$net_amt))

# 상관성(correlation)분석
cor(sb$dis_rate,abs(sb$net_amt),method = 'spearman')
cor(sb$dis_rate,abs(sb$net_amt), method = 'pearson')

# 기본 plot()함수 이용: 직선과 곡선 최적합화 선 추가
a <- plot(abs(sb$net_amt) ~ sb$dis_rate, data=sb, pch=19,
     main = '할인율에 따른 구입금액',
     xlab = '할인율(dis_rate)',
     ylab = '매출액(net_amt)')

a
# 최적의 추세직선 추가
abline(lm(abs(abs(sb$net_amt)) ~ sb$dis_rate, data = sb),
       col = 'red', lwd = 2, lty = 1)

# 최적의 추세곡선 추가
lines(lowess(abs(abs(sb$net_amt)) ~ sb$dis_rate),
      col = 'blue', lwd = 2, lty = 2)
