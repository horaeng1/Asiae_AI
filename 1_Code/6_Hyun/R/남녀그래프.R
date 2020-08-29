x <- (read.csv("X_test(1).csv"))
x$store
head(x$store,5)
x$brand
head(x$brand)
length(x$brand)

NROW(x$brand)
summary(x$brand)
library(Hmisc)
install.packages("Hmisc")
install.packages("skimr")

library(Hmisc)
Hmisc::describe(x$brand)



d <- read.csv('XY_train.csv', encoding = 'utf-8')
d

ctg_names <- c('custid','gender','sales_date','sales_time','str_nm','goodcd','brd_nm','corner_nm','pc_nm','part_nm','team_nm','buyer_nm','import_flg','tot_amt','dis_amt','net_amt','inst_mon','inst_fee')

ctg_names

ctg <- d[ctg_names]

View(d)

head(d)
str(d)

summary(ctg)

ctg$gender
length(ctg$gender)
NROW(ctg$gender)
str(ctg$gender)
summary(ctg$gender)

install.packages('psych')
library(psych)
psych::describe(ctg$gender)

library(Hmisc)
Hmisc::describe(ctg$gender)

library(skimr)
skim(ctg$gender)

ctg$gender_f <- factor(ctg$gender,levels=c(0,1),labels=c('M','F'))

View(ctg)
head(ctg)                       
str(ctg)

gender_freq <- table(ctg$gender)
gender_freq
gender_f_freq <- table(ctg$gender_f)
gender_f_freq


gender_prop <- prop.table(gender_freq)
gender_prop
(gender_f_prop <-prop.table(gender_f_freq))

round(gender_prop,3)
round(gender_f_prop,3)

gender_pect <- round(gender_prop,3)*100
gender_pect
gender_f_pect <- round(gender_f_prop,3)*100
gender_f_pect

par(mfrow=c(2,2))
barplot(gender_f_prop,main = "남녀",xlab="성별",ylab="명수")
barplot(gender_f_prop,main = "남녀",xlab="성별",ylab="명수", horiz=TRUE)
barplot(gender_f_freq,main = "남녀",xlab="성별",ylab="명수")
barplot(gender_f_freq,main = "남녀",xlab="성별",ylab="명수",horiz=TRUE)
