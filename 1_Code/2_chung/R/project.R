install.packages("plyr")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("doBy")
install.packages("psych")
install.packages("Hmisc")
install.packages("skimr")
library(plyr)
library(dplyr)
library(MASS)
library(ggplot2)
library(doBy)
Sys.setlocale("LC_ALL","korean")# 한글 OS 아닐시 꼭 써야함

x_data <- read.csv("X_train.csv",header = TRUE, sep = ',',
                   stringsAsFactors = FALSE,encoding = "CP949"
)
y_data <- read.csv("y_train.csv",header = TRUE, sep = ',',stringsAsFactors = FALSE,encoding = "CP949")

data <- merge(x = y_data, y = x_data, by = 'custid')



ctg_names <- c('custid','gender','sales_date','sales_time','str_nm','goodcd','brd_nm','corner_nm','pc_nm','part_nm','team_nm','buyer_nm','import_flg','tot_amt','dis_amt','net_amt','inst_mon','inst_fee')

ctg_names
(summary(data))

ctg<-data[ctg_names]
ctg

head(ctg)
str(ctg)
summary(ctg)

##########성별전체내용
ctg$gender

library(psych)
psych::describe(ctg$gender)



#Binomial scale
library(Hmisc)
Hmisc::describe(ctg$gender)
library(skimr)
skim(ctg$gender)



#### Binomial Factor variable(sex)

ctg$gender_0 <- factor(ctg$gender, levels= c(0,1), labels = c(" uk0","uk1"))

head(ctg)
str(ctg)

#성별변수 요약집계
gender_freq <- table(ctg$gender)
gender_freq

gender_freq_0 <- table(ctg$gender_0)
gender_freq_0

#Ratio Analysis
gender_prop <- prop.table(gender_freq)
gender_prop
gender_prop_0 <- prop.table(gender_freq_0)
gender_prop_0

#percentage
gender_perc <- round(gender_prop,3)*100
gender_perc
gender_perc_0 <- round(gender_prop_0,3)*100
gender_perc_0

#####Binomial Visualization
par(mfrow = c(2,2))

barplot(gender_freq_0,
        main="고객 성별 인원수:Simple Bar Plot",
        xlab='성별',ylab='명수수')

barplot(gender_freq_0,
        main="고객 성별 인원수:Horizontal Bar Plot",
        xlab='성별',ylab='명수', horiz =TRUE)
barplot(gender_freq_0,
        main="고객 성별 인원수:Simple Bar Plot",
        xlab='성별',ylab='명수', densit = c(20,30),
        legend = rownames(gender_freq_0))
barplot(gender_freq_0,
        main="고객 성별 인원수:Simple Bar Plot",
        xlab='성별',ylab='명수',horiz=TRUE,
        col=c('lightblue','pink'), beside = TRUE,
        legend=rownames(gender_freq_0))







sex0<-subset(data, gender ==0)
sex1<-subset(data, gender == 1)








subset(data, custid==0)

a <- data[1:18]
b <- list(a[1:18])
b
str(a)
head(a)
class(a)

custid0 <- subset(data, custid == 0)
subset(custid0) #custid 0번 필터링 데이터
sum(custid0$amount) #custid 0번 총 구매 금액
sum(custid0$amount - custid0$discount) #custid 0번 실제 구매금액
count(custid0$store) #custid 0번 지점별 방문 횟수
count(custid0$brand) #custid 0번 브랜드별 방문 횟수



head(data)
tail(data)


x<- subset(data,)

