str(data$brd_nm)
str(data_m$brd_nm)
str(data_f$brd_nm)
data_brd <- list(data$brd_nm)
install.packages(c('dplyr','psych','Hmisc','skimr'))
install.packages('fBasics')
library(dplyr)
library(psych)
library(Hmisc)
library(skimr)
library(fBasics)

Sys.setlocale("LC_ALL","korean")#os°¡ ÇÑ±ÛÀÌ ¾Æ´Ò½Ã¿¡ ²À ½á¾ßÇÔ

x_data <- read.csv("C:/Users/seokm/OneDrive/Documents/project_data/X_train.csv",header = TRUE, sep = ',', stringsAsFactors = FALSE,encoding = "CP949")
y_data <- read.csv("C:/Users/seokm/OneDrive/Documents/project_data/y_train.csv",header = TRUE, sep = ',',stringsAsFactors = FALSE,encoding = "CP949")

data <- merge(x = y_data, y = x_data, by = 'custid')

unique(data$brd_nm)
#1882
unique(data$goodcd)
#997+9427
unique(data$corner_nm)
#309
unique(data$pc_nm)
#77
unique(data$team_nm)
#4

###ºñÀ²Çü Ã´µµ º¯¼öÄÃ·³ Æ¯¼ºÆÄ¾Ç

#¸ÅÃâ ±â¼ú Åë°è·®
summary(data$tot_amt)
summary(data$dis_amt)
summary(data$net_amt)

library(Hmisc)
Hmisc::describe(data$tot_amt)
Hmisc::describe(data$dis_amt)
Hmisc::describe(data$net_amt)

#»ê¼úÆò±Õ
mean(data$tot_amt)
mean(data$dis_amt)
mean(data$net_amt)

#Áß¾Ó°ª
median(data$tot_amt)
median(data$dis_amt)
median(data$net_amt)

ctg_names <- c('custid','gender','sales_date','sales_time','str_nm','goodcd','brd_nm','corner_nm','pc_nm','part_nm','team_nm','buyer_nm','import_flg','tot_amt','dis_amt','net_amt','inst_mon','inst_fee')
ctg_names
ctg<-data[ctg_names]
ctg


#ÃÖºó°ª
tot_amt_freq <-table(ctg$tot_amt)
tot_amt_freq
sort(tot_amt_freq, decreasing = TRUE)
mean(tot_amt_freq)#11.76758
names(which.max(tot_amt_freq))#20000

dis_amt_freq <- table(ctg$dis_amt)
dis_amt_freq
sort(dis_amt_freq, decreasing = TRUE)
mean(dis_amt_freq)#124.6876
names(which.max(dis_amt_freq))#0


net_amt_freq <- table(ctg$net_amt)
net_amt_freq
sort(net_amt_freq, decreasing = TRUE)
mean(net_amt_freq)#10.85364
names(which.max(net_amt_freq))#20000



###ºñÀ²Çü Ã´µµ º¯¼öÄÃ·³ ¿ä¾àÁı°è

#ºĞ»ê(variance)
var(ctg$tot_amt)
var(ctg$dis_amt)
var(ctg$net_amt)

#Ç¥ÁØÆíÂ÷(std)
sd(ctg$tot_amt)
sd(ctg$dis_amt)
sd(ctg$net_amt)

#¹üÀ§ (range)
range(ctg$tot_amt)
range(ctg$dis_amt)
range(ctg$net_amt)

#ÃÖ´ë&ÃÖ¼Ò (max&min)
max(ctg$tot_amt)
max(ctg$dis_amt)
max(ctg$net_amt)

min(ctg$tot_amt)
min(ctg$dis_amt)
min(ctg$net_amt)

#¿Öµµ (skewness)
skewness(ctg$tot_amt)
skewness(ctg$dis_amt)
skewness(ctg$net_amt)

#Ã·µµ(kurtosis)
kurtosis(ctg$tot_amt)
kurtosis(ctg$dis_amt)
kurtosis(ctg$net_amt)


###ºñÀ²ÇüÃ´µµ º¯¼öÄÃ·³ ½Ã°¢È­

#simple plot
par("mar")

par(mar=c(1,1,1,1))

par(mfrow = c(2,2))

plot(ctg$tot_amt, type ='p',pch=21, bg='blue')
plot(ctg$dis_amt, type ='p',pch=21, bg='red')
plot(ctg$net_amt, type ='p',pch=21, bg='magenta')

#È÷½ºÅä±×·¥°ú È®·ü ¹Ğµµ °î¼±
par(mfrow=c(2,2))
hist(ctg$tot_amt,main='hist(), Frequency ?˜µ?…˜')
hist(ctg$tot_amt, probability=TRUE, main='hist(), Porbability ?˜µ?…˜')
plot(density(ctg$tot_amt),main='density() ?™•ë¥ ë?€?„ ?˜µ?…˜')
hist(ctg$tot_amt,probability=TRUE, main = 'hist() ?ˆ?Š¤?† ê·¸ë¨ê³? density() ?™•ë¥ ë?€?„ ?•¨?ˆ˜ ?†µ?•©')
lines(density(ctg$tot_amt))

#¹Ú½ºÇÃ·Ô
par(mfrow=c(1,1))
boxplot(ctg$tot_amt,main='ë°•ìŠ¤?”Œë¡?', ylab ='ì§€ì¶œê²½ë¹?')








