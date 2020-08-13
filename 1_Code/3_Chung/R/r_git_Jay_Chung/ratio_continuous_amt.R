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
#¤¤
unique(data$pc_nm)
#77
unique(data$team_nm)
#4

###ë¹„ìœ¨?˜• ì²™ë„ ë³€?ˆ˜ì»¬ëŸ¼ ?š”?•½ì§‘ê³„

#?‚°?ˆ ?‰ê·?
mean(data$tot_amt)
mean(data$dis_amt)
mean(data$net_amt)

#ì¤‘ê°„ê°?
median(data$tot_amt)
median(data$dis_amt)
median(data$net_amt)

ctg_names <- c('custid','gender','sales_date','sales_time','str_nm','goodcd','brd_nm','corner_nm','pc_nm','part_nm','team_nm','buyer_nm','import_flg','tot_amt','dis_amt','net_amt','inst_mon','inst_fee')
ctg_names
ctg<-data[ctg_names]
ctg


#ìµœë¹ˆê°?
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



###ë¹„ìœ¨?˜•ì²™ë„ ë³€?ˆ˜ì»¬ëŸ¼ ?š”?•½ì§‘ê³„

#ë¶„ì‚°(variance)
var(ctg$tot_amt)
var(ctg$dis_amt)
var(ctg$net_amt)

#?‘œì¤€?¸ì°?(std)
sd(ctg$tot_amt)
sd(ctg$dis_amt)
sd(ctg$net_amt)

#ë²”ìœ„ (range)
range(ctg$tot_amt)
range(ctg$dis_amt)
range(ctg$net_amt)

#ìµœë?€&ìµœì†Œ (max&min)
max(ctg$tot_amt)
max(ctg$dis_amt)
max(ctg$net_amt)

min(ctg$tot_amt)
min(ctg$dis_amt)
min(ctg$net_amt)

#?™œ?„ (skewness)
skewness(ctg$tot_amt)
skewness(ctg$dis_amt)
skewness(ctg$net_amt)

#ì²¨ë„(kurtosis)
kurtosis(ctg$tot_amt)
kurtosis(ctg$dis_amt)
kurtosis(ctg$net_amt)


###ë¹„ìœ¨?˜• ì²™ë„ ë³€?ˆ˜ì»¬ëŸ¼ ?‹œê°í™”

#simple plot
par("mar")

par(mar=c(1,1,1,1))

par(mfrow = c(2,2))

plot(ctg$tot_amt, type ='p',pch=21, bg='blue')
plot(ctg$dis_amt, type ='p',pch=21, bg='red')
plot(ctg$net_amt, type ='p',pch=21, bg='magenta')

#?ˆ?Š¤?† ê·¸ë¨ê³? ?™•ë¥? ë°€?„ê³¡ì„ 
par(mfrow=c(2,2))
hist(ctg$tot_amt,main='hist(), Frequency ?˜µ?…˜')
hist(ctg$tot_amt, probability=TRUE, main='hist(), Porbability ?˜µ?…˜')
plot(density(ctg$tot_amt),main='density() ?™•ë¥ ë?€?„ ?˜µ?…˜')
hist(ctg$tot_amt,probability=TRUE, main = 'hist() ?ˆ?Š¤?† ê·¸ë¨ê³? density() ?™•ë¥ ë?€?„ ?•¨?ˆ˜ ?†µ?•©')
lines(density(ctg$tot_amt))

#ë°•ìŠ¤?”Œë¡?
par(mfrow=c(1,1))
boxplot(ctg$tot_amt,main='ë°•ìŠ¤?”Œë¡?', ylab ='ì§€ì¶œê²½ë¹?')








