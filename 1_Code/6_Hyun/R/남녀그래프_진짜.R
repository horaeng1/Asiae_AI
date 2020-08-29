install.packages('plyr')
library(plyr)

a <- ddply(ctg, .(custid),summarise,min=min(gender))
a
View(a)

ctg_names1 <- c('custid','gender')

View(ctg_names1)

ctg1 <-a[ctg_names1]

write.csv(a, file='a.csv', row.names = FALSE)

cg <- read.csv('a.csv',)
View(a)

ctg_names1 <- c('custid','gender')
ctg1 <- a[ctg_names1]

names(a) <- c('custid', 'gender')
View(a)
write.csv(a, file='a.csv', row.names = FALSE)

read.csv('a.csv')

summary(a)

a$gender
library('psych')
psych::describe(a$gender)
library('Hmisc')
Hmisc::describe(a$gender)
library(skimr)
skim(a$gender)

a$gender_f <- factor(a$gender,levels=c(0,1),labels = c('M','F'))
head(a)

gender_freq <- table(a$gender_f)
gender_f_freq <- table(a$gender_f)
gender_prop <- prop.table(gender_freq)
gender_f_prop <- prop.table(gender_f_freq)
gender_f_prop

round(gender_prop,3)
round(gender_f_prop,3)

gender_pect <- round(gender_prop,3)*100
gender_pect
gender_f_pect <- round(gender_f_prop,3)*100
gender_f_pect

par(mfrow=c(2,2))
barplot(gender_f_freq,main='남녀',xlab='성별',ylab='명수')
barplot(gender_f_freq,main='남녀',xlab='성별',ylab='명수',
        horiz=TRUE,col=c('lightblue','pink'),beside=TRUE,legend=rownames(gender_f_freq))

