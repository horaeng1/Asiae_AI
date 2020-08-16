#기본
library(dplyr)
library(psych)
library(Hmisc)
library(skimr)
library(fBasics)
library(ggplot2)

Sys.setlocale("LC_ALL","korean")#os가 한글이 아닐시에 꼭 써야함


x_data <- read.csv("C:/Users/seokm/OneDrive/Documents/project_data/X_train.csv",header = TRUE, sep = ',', stringsAsFactors = FALSE,encoding = "CP949")
y_data <- read.csv("C:/Users/seokm/OneDrive/Documents/project_data/y_train.csv",header = TRUE, sep = ',',stringsAsFactors = FALSE,encoding = "CP949")
data <- merge(x = y_data, y = x_data, by = 'custid')
#---------dc_rate--------------------------------\
#--------------------------------
dc_rate <- round((data_inst$dis_amt/ data_inst$tot_amt)*100, 0)
dc_rate
unique(dc_rate)

from <- list(0, c(1:5), c(6:65))
to <- list(0, 5, 10)
library(doBy)
dc_rate <- recodeVar(dc_rate , from , to)
dc_rate_f <- factor(dc_rate, levels = c(0,5,10), labels = c('0%','5%','10%'))




data_inst$dc_rate <- dc_rate
data_inst$dc_rate_f <- dc_rate_f
dc_rate 

summary(data_inst$dc_rate)
#part_nm_f
unique(data$part_nm)
part_nm_f <- factor(data$part_nm, levels = unique(data$part_nm), labels = c(0:30))
part_nm_f
part_nm_f <- factor(data$part_nm_f, levels = c(0:30), labels = unique(data$part_nm))
part_nm_f
data$part_nm_f <- as.factor(data_inst$part_nm)
str(part_nm_f)


#-------------------------------------------------------------
# inst_tot / 무이자 할부 = 1/ 유이자 할부 = 2/ 일시불  = 3

str(data)
data_tmp <- data

#data_tmp <- merge(data_tmp , notfee , by = c("inst_fee", "inst_mon"))



tmp <- as.data.frame(table(data_tmp$inst_mon, data_tmp$inst_fee))
tmp
names(tmp) <- c('inst_mon', 'inst_fee', 'inst_tot')
tmp
#할부요인

tmp$inst_tot <- c(3, 1, 1, 1, 1, 1 ,1, 1 ,1 ,1 ,1, 1, 3, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2 )

tmp

data_tmp <- merge(data_tmp, tmp, by = c('inst_mon', 'inst_fee'))

data_inst <- data_tmp
data_inst
str(data_inst)
data_inst$inst_tot_f <- as.factor(data_inst$inst_tot)
data_inst$inst_tot_f <- factor(data_inst$inst_tot_f, levels= c(1:3), labels = c('무이자할부', '유이자할부','일시불'))
#part_nm /dc_rate / count

dc_rate <- round((data$dis_amt/ data$tot_amt)*100, 0)
dc_rate
data$dc_rate <- dc_rate
data$dc_rate_f <- dc_rate_f
dc_rate 
unique(dc_rate)
tmp <- table(data$dc_rate_f, data$part_nm_f)
str(data$dc_rate_f)
str(data$part_nm_f)
tmp_prop <-prop.table(tmp,2)
tmp_prop <- round(tmp_prop,4)*100
tmp_prop <-as.data.frame(tmp_prop)
names(tmp_prop) <- c('할인율','파트', '건수')
tmp_prop

ggplot(as.data.frame(tmp_prop), aes(x=파트, y=건수, fill=할인율)) +
  ggtitle("할인율에 따른 파트별 판매건수 비교")+
  geom_bar(stat="identity")+
  geom_text(aes(y=건수, label = paste(건수,"%")),position = position_stack(vjust = 0.5), color = "black", size=3)+
  theme(axis.text.x = element_text(angle=90, hjust = 1, vjust=0, color="black", size=15),
        plot.title = element_text(family="serif", face = "bold", hjust= 0.5, size=20))

#part_nm /inst_tot / count
tmp <- table(data_inst$inst_tot_f, data_inst$part_nm)
tmp_prop <-prop.table(tmp,2)
tmp_prop <- round(tmp_prop,4)*100
tmp_prop <-as.data.frame(tmp_prop)
names(tmp_prop) <- c('할부요인','파트', '건수')
tmp_prop

ggplot(as.data.frame(tmp_prop), aes(x=파트, y=건수, fill=할부요인)) +
  ggtitle("할부요인에 따른 파트별 판매건수 비교")+
  geom_bar(stat="identity")+
  geom_text(aes(y=건수, label = paste(건수,"%")),position = position_stack(vjust = 0.5), color = "black", size=3)+
  theme(axis.text.x = element_text(angle=90, hjust = 1, vjust=0, color="black", size=15),
        plot.title = element_text(family="serif", face = "bold", hjust= 0.5, size=20))
#part_nm /dc_rate / net_amt

dc_rate <- recodeVar(dc_rate , from , to)
dc_rate_f <- factor(dc_rate, levels = c(0,5,10), labels = c('0%','5%','10%'))

tmp <- aggregate(net_amt ~ part_nm_f + dc_rate_f, data, sum, drop = FALSE)
tmp[is.na(tmp)]<-0
tmp

temp <-matrix(as.numeric(tmp$net_amt), ncol = length(unique(tmp$part_nm_f)), byrow=TRUE)
view(temp)
colnames(temp) <-levels(tmp$part_nm_f)
rownames(temp)<-levels(tmp$dc_rate_f)
temp <- as.table(temp)
temp
tmp_prop <- prop.table(temp,2)
tmp_prop <-round(tmp_prop ,4)*100
tmp_prop <-as.data.frame(tmp_prop)
tmp_prop
names(tmp_prop) <-c('할인율','파트','금액')

ggplot(as.data.frame(tmp_prop), aes(x=파트, y=금액, fill=할인율)) +
  ggtitle("할인율에 따른 파트별 판매금액 비교")+
  geom_bar(stat="identity")+
  geom_text(aes(y=금액, label = paste(금액,"%")),position = position_stack(vjust = 0.5), color = "black", size=3)+
  theme(axis.text.x = element_text(angle=90, hjust = 1, vjust=0, color="black", size=15),
        plot.title = element_text(family="serif", face = "bold", hjust= 0.5, size=20))

