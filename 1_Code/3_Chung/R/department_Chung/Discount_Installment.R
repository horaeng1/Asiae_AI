library(dplyr)
library(psych)
library(Hmisc)
library(skimr)
library(fBasics)

Sys.setlocale("LC_ALL","korean")#os가 한글이 아닐시에 꼭 써야함


x_data <- read.csv("C:/Users/seokm/OneDrive/Documents/project_data/X_train.csv",header = TRUE, sep = ',', stringsAsFactors = FALSE,encoding = "CP949")
y_data <- read.csv("C:/Users/seokm/OneDrive/Documents/project_data/y_train.csv",header = TRUE, sep = ',',stringsAsFactors = FALSE,encoding = "CP949")
data <- merge(x = y_data, y = x_data, by = 'custid')

#~~~
from <- list(c(1:20))
to <- list(1)
from
from <- append(list(c(1*20:40)), from)
append(2, to)
to

for (i in c(2:10)){
  tmp <- c(((i*20)+1):((i+1)*20))
  from <- append(list(tmp), from)
  to <- append(i, to)
}
from
to

str(data)
dc_rate <- round((data$dis_amt/ data$tot_amt)*100, 0)
dc_rate
unique(dc_rate)

from <- list(0, c(1:5), c(6:65))
to <- list(0, 5, 10)
library(doBy)
dc_rate <- recodeVar(dc_rate , from , to)
dc_rate_f <- factor(dc_rate, levels = c(0,5,10), labels = c('0%','5%','10%'))




data$dc_rate <- dc_rate
data$dc_rate_f <- dc_rate_f
dc_rate 

summary(data$dc_rate)


str(data_main)

data_main <- data[data$str_nm=='본점',]
data_main


#buyer_nm_f
buyer_nm_f<- factor(data_main$buyer_nm, levels =unique(data_main$buyer_nm), labels = c(0:28))
buyer_nm_f<-factor(data_main$buyer_nm_f, levels =c(0:28), labels = unique(data_main$buyer_nm))  
buyer_nm_f

#part_nm_f
unique(data$part_nm)
part_nm_f <- factor(data$part_nm, levels = unique(data$part_nm), labels = c(0:30))
part_nm_f
part_nm_f <- factor(data$part_nm_f, levels = c(0:30), labels = unique(data$part_nm))
part_nm_f
#--------------------------------------------------
#part_nm/dc_rate/count

tmp <- table(data$dc_rate, data$part_nm)
tmp
tmp_prop <- prop.table(tmp, 2)
tmp_prop <- round(tmp_prop,4)*100
tmp_prop
par(mfrow=c(1,1))
library(ggplot2)
tmp_prop <- as.data.frame(tmp_prop)
names(tmp_prop)<-c('할인율','파트','건수')

tmp_prop

ggplot(as.data.frame(tmp_prop), aes(x = 파트, ,y = 건수, fill = 할인율))+
  geom_bar(stat = "identity")+
  geom_text(aes(y=건수, label = paste(건수)), color= 'black', size = 3)+
  theme(axis.text.x=element_text(angle= 90, hjust=1, vjust=0, color='black',size=10))

par(mfrow=c(1,1))
ggplot(as.data.frame(tmp_prop),aes(x = 파트, ,y = 건수, group= 할인율))+
  geom_col(aes(fill=할인율))+
  geom_text(aes(label=건수),position = position_stack(vjust=0.5, reverse=FALSE))+
  theme(axis.text.x=element_text(angle= 90, hjust=1, vjust=0, color='black',size=10))

#part_nm/dc_rate/tot_amt

tmp <- table(data$dc_rate, data$part_nm)
tmp
tmp_prop <- prop.table(tmp, 2)
tmp_prop <- round(tmp_prop,4)*100
tmp_prop
par(mfrow=c(1,1))
library(ggplot2)
tmp_prop <- as.data.frame(tmp_prop)
names(tmp_prop)<-c('할인율','파트','건수')

tmp_prop

par(mfrow=c(1,1))
ggplot(as.data.frame(tmp_prop),aes(x = 파트, ,y = 건수, group= 할인율))+
  geom_col(aes(fill=할인율))+
  geom_text(aes(label=건수),position = position_stack(vjust=0.5, reverse=FALSE))+
  theme(axis.text.x=element_text(angle= 90, hjust=1, vjust=0, color='black',size=10))

# buyer_nm / dc_rate / count
tmp <- table(data_main$dc_rate, data_main$buyer_nm)
tmp
tmp_prop <- prop.table(tmp, 2)
tmp_prop <- round(tmp_prop, 4) *100
tmp_prop
par(mfrow=c(1,1))
barplot(tmp, main =  "카테고리별 판매건수 할인율로 비교",
        xlab = "카테고리", ylab = '건수', las = 2,
        col = c("lightblue", "pink", "yellow"))
legend("topright", legend = c('0%','5%','10%'), fill =c("lightblue", "pink", "yellow") , cex = 0.3)

barplot(tmp_prop, main =  "카테고리별 판매건수 할인율로 비교",
        xlab = "카테고리", ylab = '건수', las = 2,
        col = c("lightblue", "pink", "yellow"))
legend("topright", legend = c('0%','5%','10%'), fill =c("lightblue", "pink", "yellow") , cex = 0.3)
#--------------------------------------------------
# corner_nm / dc_rate / count
tmp <- table(data_main$dc_rate, data_main$corner_nm_f)
tmp
tmp_prop <- prop.table(tmp, 2)
tmp_prop <- round(tmp_prop, 4) *100
tmp_prop
par(mfrow=c(2,1))

barplot(tmp, main =  "코너별 판매건수 할인율로 비교",
        xlab = "코너", ylab = '건수', las = 2,
        col = c("lightblue", "pink", "yellow"))
legend("topright", legend = c('0%','5%','10%'), fill =c("lightblue", "pink", "yellow") , cex = 0.3)

barplot(tmp_prop, main =  "코너별 판매건수 할인율로 비교",
        xlab = "코너", ylab = '건수', las = 2,
        col = c("lightblue", "pink", "yellow"))
legend("topright", legend = c('0%','5%','10%'), fill =c("lightblue", "pink", "yellow") , cex = 0.3)

#--------------------------------------------------
# pt_nm / dc_rate / count
tmp <- table(data_main$dc_rate, data_main$part_nm_f)
tmp
tmp_prop <- prop.table(tmp, 2)
tmp_prop <- round(tmp_prop, 4) *100
tmp_prop
par(mfrow=c(2,1))
barplot(tmp, main =  "파트별 판매건수 할인율로 비교",
        xlab = "파트", ylab = '건수', las = 2,
        col = c("lightblue", "pink", "yellow"))
legend("topright", legend = c('0%','5%','10%'), fill =c("lightblue", "pink", "yellow") , cex = 0.3)

barplot(tmp_prop, main =  "파트별 판매건수 할인율로 비교",
        xlab = "파트", ylab = '건수', las = 2,
        col = c("lightblue", "pink", "yellow"))
legend("topright", legend = c('0%','5%','10%'), fill =c("lightblue", "pink", "yellow") , cex = 0.3)

#--------------------------------------------------
#--------------------------------------------------
# tot_amt
# buyer_nm / dc_rate / tot_amt
par(mfrow=c(1,2))
p1 <- ggplot(data_main, aes(buyer_nm_f, tot_amt, color = dc_rate_f, shape = dc_rate_f))+
  geom_boxplot()+
  labs(title = "할인율에 따른 지출금액 분포를 카테고리별로 세분화해 비교분석")
p1 <- p1  + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
p2 <- ggplot(data_main, aes(buyer_nm_f, tot_amt))+
  geom_boxplot() + facet_wrap(~ dc_rate_f)+
  labs(title = "할인율에 따른 지출금액 분포를 카테고리별 분할그래프로 비교분석")
p2 <- p2  + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
# ggplot(data_main, aes(dc_rate_f, tot_amt, color = buyer_nm_f, shape = buyer_nm_f))+
# geom_boxplot()+
# labs(title = "카테고리에 따른 지출금액 분포를 할인율별로 세분화해 비교분석")
library(gridExtra)
grid.arrange(p1, p2, nrow = 2, ncol = 1)

#--------------------------------------------------
# part_nm / dc_rate / tot_amt
p1 <- ggplot(data_main, aes(part_nm_f, tot_amt, color = dc_rate_f, shape = dc_rate_f))+
  geom_boxplot()+
  labs(title = "할인율에 따른 지출금액 분포를 파트별로 세분화해 비교분석")
p1 <- p1  + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
p2 <- ggplot(data_main, aes(part_nm_f, tot_amt))+
  geom_boxplot() + facet_wrap(~ dc_rate_f)+
  labs(title = "할인율에 따른 지출금액 분포를 파트별 분할그래프로 비교분석")
p2 <- p2  + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
grid.arrange(p1, p2, nrow = 2, ncol = 1)

#--------------------------------------------------
# corner_nm / dc_rate / tot_amt
p1 <- ggplot(data_main, aes(corner_nm_f, tot_amt, color = dc_rate_f, shape = dc_rate_f))+
  geom_boxplot()+
  labs(title = "할인율에 따른 지출금액 분포를 코너별로 세분화해 비교분석")
p1 <- p1  + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
p2 <- ggplot(data_main, aes(corner_nm_f, tot_amt))+
  geom_boxplot() + facet_wrap(~ dc_rate_f)+
  labs(title = "할인율에 따른 지출금액 분포를 코너별 분할그래프로 비교분석")
p2 <- p2  + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
grid.arrange(p1, p2, nrow = 2, ncol = 1)

#--------------------------------------------------
#--------------------------------------------------
# dis_amt
# buyer_nm / dc_rate / dis_amt
p1 <- ggplot(data_main, aes(buyer_nm_f, dis_amt, color = dc_rate_f, shape = dc_rate_f))+
  geom_boxplot()+
  labs(title = "할인율에 따른 할인금액 분포를 코너별로 세분화해 비교분석")
p1 <- p1  + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

p2 <- ggplot(data_main, aes(buyer_nm_f, dis_amt))+
  geom_boxplot() + facet_wrap(~ dc_rate_f)+
  labs(title = "할인율에 따른 할인금액 분포를 코너별 분할그래프로 비교분석")
p2 <- p2  + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
grid.arrange(p1, p2, nrow = 2, ncol = 1)

#--------------------------------------------------
# corner_nm / dc_rate / dis_amt
p1 <- ggplot(data_main, aes(corner_nm_f, dis_amt, color = dc_rate_f, shape = dc_rate_f))+
  geom_boxplot()+
  labs(title = "할인율에 따른 할인금액 분포를 코너별로 세분화해 비교분석")
p1 <- p1  + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

p2 <- ggplot(data_main, aes(corner_nm_f, dis_amt))+
  geom_boxplot() + facet_wrap(~ dc_rate_f)+
  labs(title = "할인율에 따른 할인금액 분포를 코너별 분할그래프로 비교분석")
p2 <- p2  + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

grid.arrange(p1, p2, nrow = 2, ncol = 1)

#--------------------------------------------------
# part_nm / dc_rate / dis_amt
p1 <- ggplot(data_main, aes(part_nm_f, dis_amt, color = dc_rate_f, shape = dc_rate_f))+
  geom_boxplot()+
  labs(title = "할인율에 따른 할인금액 분포를 코너별로 세분화해 비교분석")
p1 <- p1  + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

p2 <- ggplot(data_main, aes(part_nm_f, dis_amt))+
  geom_boxplot() + facet_wrap(~ dc_rate_f)+
  labs(title = "할인율에 따른 할인금액 분포를 코너별 분할그래프로 비교분석")
p2 <- p2  + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

grid.arrange(p1, p2, nrow = 2, ncol = 1)

#--------------------------------------------------
# corner_nm_f 본점기준으로 재변환
length(unique(data_main$corner_nm))
str(data)
tmp <- factor(data_main$corner_nm, levels = unique(data_main$corner_nm), labels = c(0:131))
tmp <- factor(tmp, levels = c(0:131), labels = unique(data_main$corner_nm))
data_main$corner_nm_f <- tmp

#--------------------------------------------------
# 환불이력이 있는 custid로 필터링

data_main_dis <- data_main[data_main$tot_amt <0,]
length(unique(data_main_dis$custid))
u_custid <- unique(data_main_dis$custid)
u_custid

tmp <- data_main[data_main$custid %in% u_custid,]
tmp

#-------------------------------------------------------------
# 할인율에 따른 구매금액 분포 월별 그래프
# sales_date_month / tot_amt / dc_rate
str(data_main)
p1 <- ggplot(data_main, aes(sales_date_month, tot_amt, color = dc_rate_f, shape = dc_rate_f))+
  geom_boxplot()+
  labs(title = "할인율에 따른 구매금액 분포를 월별로 세분화해 비교분석")
p1 <- p1  + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

p2 <- ggplot(data_main, aes(sales_date_month, tot_amt))+
  geom_boxplot() + facet_wrap(~ dc_rate_f)+
  labs(title = "할인율에 따른 구매금액 분포를 월별로 세분화해 비교분석")
p2 <- p2  + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
grid.arrange(p1, p2, nrow = 2, ncol = 1)

#-------------------------------------------------------------
# 할인율에 따른 판매건수 분포 월별 그래프
# sales_date_month / tot_amt / dc_rate
par(mfrow=c(1,1))
plot(tot_amt ~ sales_date_month_f, data = data_main, pch=19)
str(data_main$sales_date_month_f)
data_main$sales_date_month_f <- factor(data_main$sales_date_month_f , levels = c("2000-05", "2000-06", "2000-07", "2000-08", "2000-09", "2000-10", "2000-11", "2000-12", "2001-01", "2001-02", "2001-03","2001-04"), labels= c(0:11))
data_main$sales_date_month_f <- factor(data_main$sales_date_month_f , levels = c(0:11), labels= c("2000-05", "2000-06", "2000-07", "2000-08", "2000-09", "2000-10", "2000-11", "2000-12", "2001-01", "2001-02", "2001-03","2001-04"))

tmp <- table(data_main$dc_rate, data_main$sales_date_month_f)
tmp
tmp_prop <- prop.table(tmp, 2)
par(mfrow=c(2,1))
barplot(tmp, main =  "월별 판매건수 할인율로 비교",
        xlab = "년월", ylab = '건수', las = 2,
        col = c("lightblue", "pink", "yellow"))


barplot(tmp_prop, main =  "월별 판매건수 할인율로 비교",
        xlab = "년월", ylab = '건수', las = 2,
        col = c("lightblue", "pink", "yellow"))

#-------------------------------------------------------------
# 할인율에 따른 구매금액 분포 월별 그래프
tmp <- aggregate(tot_amt ~ sales_date_month_f + dc_rate_f, data_main, sum, na.rm=TRUE)

temp <- matrix(as.numeric(tmp$tot_amt), ncol=12,  byrow=TRUE)
colnames(temp) <- levels(tmp$sales_date_month_f)
rownames(temp) <- levels(tmp$dc_rate_f)
temp <- as.table(temp)
temp
temp_prop <- prop.table(temp, 2)

barplot(temp, main =  "월별 판매금액 할인율로 비교",
        xlab = "년월", ylab = '금액', las = 2,
        col = c("lightblue", "pink", "yellow"))
legend("topright", legend = c('0%','5%','10%'), fill =c("lightblue", "pink", "yellow") , cex = 0.3)

barplot(temp, main =  "월별 판매금액 비율 할인율로 비교",
        xlab = "년월", ylab = '금액', las = 2,
        col = c("lightblue", "pink", "yellow"))
legend("topright", legend = c('0%','5%','10%'), fill =c("lightblue", "pink", "yellow") , cex = 0.3)


#-------------------------------------------------------------
# 할인율에 따른 할인금액 분포 월별 그래프
tmp <- aggregate(dis_amt ~ sales_date_month_f + dc_rate_f, data_main, sum, na.rm=TRUE)
tmp
temp <- matrix(as.numeric(tmp$dis_amt), ncol=12,  byrow=TRUE)
colnames(temp) <- levels(tmp$sales_date_month_f)
rownames(temp) <- levels(tmp$dc_rate_f)
temp <- as.table(temp)
temp
temp_prop <- prop.table(temp, 2)

barplot(temp, main =  "월별 할인금액 할인율로 비교",
        xlab = "년월", ylab = '금액', las = 2,
        col = c("lightblue", "pink", "yellow"))
legend("topright", legend = c('0%','5%','10%'), fill =c("lightblue", "pink", "yellow") , cex = 0.3)

barplot(temp, main =  "월별 할인금액 비율 할인율로 비교",
        xlab = "년월", ylab = '금액', las = 2,
        col = c("lightblue", "pink", "yellow"))
legend("topright", legend = c('0%','5%','10%'), fill =c("lightblue", "pink", "yellow") , cex = 0.3)



#-------------------------------------------------------------
#-------------------------------------------------------------
#-------------------------------------------------------------
# buyer_nm / dc_rate / count
tmp <- table(data$dc_rate, data$buyer_nm_f)
tmp
tmp_prop <- prop.table(tmp, 2)
tmp_prop <- round(tmp_prop, 4) *100
tmp_prop
par(mfrow=c(1,1))
barplot(tmp, main =  "할인율에 따른 카테고리별 판매건수 비교",
        xlab = "카테고리", ylab = '건수', las = 2,
        col = c("lightblue", "pink", "yellow"))
legend("topright", legend = c('0%','5%','10%'), fill =c("lightblue", "pink", "yellow") , cex = 0.3)

barplot(tmp_prop, main =  "할인율에 따른 카테고리별 판매건수 비율 비교",
        xlab = "카테고리", ylab = '비율', las = 2,
        col = c("lightblue", "pink", "yellow"))
legend("topright", legend = c('0%','5%','10%'), fill =c("lightblue", "pink", "yellow") , cex = 0.3)

# 돌려보자

barplot(tmp, main =  "할인율에 따른 카테고리별 판매건수 비교", horiz=TRUE, 
        xlab = "카테고리", ylab = '건수', las = 2,
        col = c("lightblue", "pink", "yellow"))
legend("topright", legend = c('0%','5%','10%'), fill =c("lightblue", "pink", "yellow") , cex = 0.3)


#-------------------------------------------------------------
# buyer_nm / dc_rate / net_amt
tmp <- aggregate(net_amt ~ buyer_nm_f + dc_rate_f, data, sum, drop=FALSE)
tmp[is.na(tmp)] <- 0
tmp

temp <- matrix(as.numeric(tmp$net_amt), ncol=length(unique(tmp$buyer_nm_f)),  byrow=TRUE)
colnames(temp) <- levels(tmp$buyer_nm_f)
rownames(temp) <- levels(tmp$dc_rate_f)
temp <- as.table(temp)
temp
temp_prop <- prop.table(temp, 2)

barplot(temp, main =  "할인율에 따른 카테고리별 판매금액 비교",
        xlab = "카테고리", ylab = '금액', las = 2,
        col = c("lightblue", "pink", "yellow"))
legend("topright", legend = c('0%','5%','10%'), fill =c("lightblue", "pink", "yellow") , cex = 0.3)

barplot(temp_prop, main =  "할인율에 따른 카테고리별 판매금액 비교",
        xlab = "카테고리", ylab = '금액', las = 2,
        col = c("lightblue", "pink", "yellow"))
legend("topright", legend = c('0%','5%','10%'), fill =c("lightblue", "pink", "yellow") , cex = 0.3)

#-------------------------------------------------------------
#-------------------------------------------------------------
#-------------------------------------------------------------
# sales_date_month / dc_rate / count
data$sales_date_month_f <- factor(data$sales_date_month_f , levels = c("2000-05", "2000-06", "2000-07", "2000-08", "2000-09", "2000-10", "2000-11", "2000-12", "2001-01", "2001-02", "2001-03","2001-04"), labels= c(0:11))
data$sales_date_month_f <- factor(data$sales_date_month_f , levels = c(0:11), labels= c("2000-05", "2000-06", "2000-07", "2000-08", "2000-09", "2000-10", "2000-11", "2000-12", "2001-01", "2001-02", "2001-03","2001-04"))

tmp <- table(data$dc_rate, data$sales_date_month_f)
tmp
tmp_prop <- prop.table(tmp, 2)
tmp_prop <- round(tmp_prop, 4) *100
tmp_prop
par(mfrow=c(2,1))
barplot(tmp, main =  "할인율에 따른 월별 판매건수 비교",
        xlab = "년월", ylab = '건수', las = 2,
        col = c("lightblue", "pink", "yellow"))
legend("topright", legend = c('0%','5%','10%'), fill =c("lightblue", "pink", "yellow") , cex = 0.3)

barplot(tmp_prop, main =  "할인율에 따른 월별 판매건수 비율 비교",
        xlab = "년월", ylab = '비율', las = 2,
        col = c("lightblue", "pink", "yellow"))
legend("topright", legend = c('0%','5%','10%'), fill =c("lightblue", "pink", "yellow") , cex = 0.3)
#-------------------------------------------------------------
# salse_date_month / dc_rate / net_amt
tmp <- aggregate(net_amt ~ sales_date_month_f + dc_rate_f, data, sum, drop=FALSE)
tmp[is.na(tmp)] <- 0
tmp

temp <- matrix(as.numeric(tmp$net_amt), ncol=length(unique(tmp$sales_date_month_f)),  byrow=TRUE)
colnames(temp) <- levels(tmp$sales_date_month_f)
rownames(temp) <- levels(tmp$dc_rate_f)
temp <- as.table(temp)
temp
temp_prop <- prop.table(temp, 2)

barplot(temp, main =  "할인율에 따른 월별 판매금액 비교",
        xlab = "년월", ylab = '금액', las = 2,
        col = c("lightblue", "pink", "yellow"))
legend("topright", legend = c('0%','5%','10%'), fill =c("lightblue", "pink", "yellow") , cex = 0.3)

barplot(temp_prop, main =  "할인율에 따른 월별 판매금액 비교",
        xlab = "년월", ylab = '금액', las = 2,
        col = c("lightblue", "pink", "yellow"))
legend("topright", legend = c('0%','5%','10%'), fill =c("lightblue", "pink", "yellow") , cex = 0.3)

#-------------------------------------------------------------
# str_nm / dc_rate / count
data$str_nm_f <- factor(data$str_nm , levels = c("천호점", "신촌점", "무역점", "본점"), labels= c(0:3))
data$str_nm_f <- factor(data$str_nm_f , levels = c(0:3), labels= c("천호점", "신촌점", "무역점", "본점"))

tmp <- table(data$dc_rate, data$str_nm_f)
tmp
tmp_prop <- prop.table(tmp, 2)
tmp_prop <- round(tmp_prop, 4) *100
tmp_prop
par(mfrow=c(2,1))
barplot(tmp, main =  "할인율에 따른 지점별 판매건수 비교", horiz=TRUE,
        xlab = "건수", ylab = '지점', las = 2,
        col = c("lightblue", "pink", "yellow"))
legend("topright", legend = c('0%','5%','10%'), fill =c("lightblue", "pink", "yellow") , cex = 0.3)

barplot(tmp_prop, main =  "할인율에 따른 지점별 판매건수 비율 비교", horiz=TRUE,
        xlab = "건수", ylab = '지점', las = 2,
        col = c("lightblue", "pink", "yellow"))
legend("topright", legend = c('0%','5%','10%'), fill =c("lightblue", "pink", "yellow") , cex = 0.3)

#-------------------------------------------------------------
# str_na / dc_rate / net_amt
tmp <- aggregate(net_amt ~ str_nm_f+ dc_rate_f, data, sum, drop=FALSE)
tmp[is.na(tmp)] <- 0
tmp

temp <- matrix(as.numeric(tmp$net_amt), ncol=length(unique(tmp$str_nm_f)),  byrow=TRUE)
colnames(temp) <- levels(tmp$str_nm_f)
rownames(temp) <- levels(tmp$dc_rate_f)
temp <- as.table(temp)
temp
temp_prop <- prop.table(temp, 2)

barplot(temp, main =  "할인율에 따른 지점별 판매금액 비교", horiz=TRUE,
        xlab = "금액", ylab = '지점', las = 2,
        col = c("lightblue", "pink", "yellow"))
legend("topright", legend = c('0%','5%','10%'), fill =c("lightblue", "pink", "yellow") , cex = 0.3)

barplot(temp_prop, main =  "할인율에 따른 지점별 판매금액 비교", horiz=TRUE,
        xlab = "금액", ylab = '지점', las = 2,
        col = c("lightblue", "pink", "yellow"))
legend("topright", legend = c('0%','5%','10%'), fill =c("lightblue", "pink", "yellow") , cex = 0.3)

#-------------------------------------------------------------
#-------------------------------------------------------------
#-------------------------------------------------------------
# inst_mon 팩터형 추가
length(unique(data$inst_mon))
tmp <- factor(data$inst_mon, levels=c(1:12), labels = c(1:12))
data$inst_mon_f <- tmp
str(data)

# inst_fee 팩터형 추가
length(unique(data$inst_fee))
tmp <- factor(data$inst_fee, levels=c(0,1), labels = c("무","유"))
data$inst_fee_f <- tmp
str(data)
data$inst_fee_f

#-------------------------------------------------------------
#-------------------------------------------------------------
#-------------------------------------------------------------
# sales_date_month / (inst_mon, inst_fee) / count
tmp <- table(data$inst_mon_f, data$sales_date_month_f)
tmp_prop <- prop.table(tmp, 2)
tmp_prop <- round(tmp_prop, 4) *100
tmp_prop
par(mfrow=c(1,1))
barplot(tmp, main =  "할부 요인에 따른 월별 판매건수", horiz=TRUE,
        xlab = "건수", ylab = '년월', las = 2,
        col = c("lightblue", "gray", "pink",'red','black','yellow','hotpink','green','orange','navy', 'black', 'red'))
#legend("topright", legend = c('0%','5%','10%'), fill =c("lightblue", "pink", "red",'magenta','cyan','yellow','hotpink','green','orange','navy', 'black', 'white') , cex = 0.3)
barplot(tmp_prop, main =  "할부 요인에 따른 월별 판매건수 비율", horiz=TRUE,
        xlab = "건수", ylab = '년월', las = 2,
        col = c("lightblue", "gray", "pink",'red','black','yellow','hotpink','green','orange','navy', 'black', 'red'))


#-------------------------------------------------------------
# sales_date_month / (inst_mon, inst_fee) / net_amt
tmp <- aggregate(net_amt ~ sales_date_month_f+ inst_mon_f, data, sum, drop=FALSE)
tmp[is.na(tmp)] <- 0
tmp

temp <- matrix(as.numeric(tmp$net_amt), ncol=length(unique(tmp$sales_date_month_f)),  byrow=TRUE)
colnames(temp) <- levels(tmp$sales_date_month_f)
rownames(temp) <- levels(tmp$inst_mon_f)
temp <- as.table(temp)
temp
temp_prop <- prop.table(temp, 2)
temp_prop
barplot(temp, main =  "할부 요인에 따른 월별 판매금액 비교", horiz=TRUE,
        xlab = "금액", ylab = '년월', las =2,
        col = c("lightblue", "gray", "pink",'red','black','yellow','hotpink','green','orange','navy', 'black', 'red'))


barplot(temp_prop, main =  "할인율에 따른 지점별 판매금액 비교", horiz=TRUE,
        xlab = "금액", ylab = '년월', las = 2,
        col = c("lightblue", "gray", "pink",'red','black','yellow','hotpink','green','orange','navy', 'black', 'red'))

#-------------------------------------------------------------
# str_nm / (inst_mon, inst_fee) / count
tmp <- table(data$inst_mon_f, data$str_nm_f)
tmp_prop <- prop.table(tmp, 2)
tmp_prop <- round(tmp_prop, 4) *100
tmp_prop
par(mfrow=c(2,1))
barplot(tmp, main =  "할부 요인에 따른 지점별 판매건수", horiz=TRUE,
        xlab = "건수", ylab = '지점', las = 2,
        col = c("lightblue", "gray", "pink",'red','black','yellow','hotpink','green','orange','navy', 'black', 'red'))
#legend("topright", legend = c('0%','5%','10%'), fill =c("lightblue", "pink", "red",'magenta','cyan','yellow','hotpink','green','orange','navy', 'black', 'white') , cex = 0.3)
barplot(tmp_prop, main =  "할부 요인에 따른 지점별 판매건수 비율", horiz=TRUE,
        xlab = "건수", ylab = '지점', las = 2,
        col = c("lightblue", "gray", "pink",'red','black','yellow','hotpink','green','orange','navy', 'black', 'red'))



#-------------------------------------------------------------
# str_nm / (inst_mon, inst_fee) / net_amt
tmp <- aggregate(net_amt ~ str_nm_f+ inst_mon_f, data, sum, drop=FALSE)
tmp[is.na(tmp)] <- 0
tmp

temp <- matrix(as.numeric(tmp$net_amt), ncol=length(unique(tmp$str_nm_f)),  byrow=TRUE)
colnames(temp) <- levels(tmp$str_nm_f)
rownames(temp) <- levels(tmp$inst_mon_f)
temp <- as.table(temp)
temp
temp_prop <- prop.table(temp, 2)
temp_prop
barplot(temp, main =  "할부 요인에 따른 지점별 판매금액 비교", horiz=TRUE,
        xlab = "금액", ylab = '지점', las =2,
        col = c("lightblue", "gray", "pink",'red','black','yellow','hotpink','green','orange','navy', 'black', 'red'))


barplot(temp_prop, main =  "할인 요인에 따른 지점별 판매금액 비율 비교", horiz=TRUE,
        xlab = "금액", ylab = '지점', las = 2,
        col = c("lightblue", "gray", "pink",'red','black','yellow','hotpink','green','orange','navy', 'black', 'red'))



#-------------------------------------------------------------
# buyer_nm / (inst_mon, inst_fee) / count
tmp <- table(data$inst_mon_f, data$buyer_nm_f)
tmp_prop <- prop.table(tmp, 2)
tmp_prop <- round(tmp_prop, 4) *100
tmp_prop
par(mfrow=c(2,1))
barplot(tmp, main =  "할부 요인에 따른 카테고리별 판매건수", 
        xlab = "카테고리", ylab = '건수', las = 2,
        col = c("lightblue", "gray", "pink",'red','black','yellow','hotpink','green','orange','navy', 'black', 'red'))
#legend("topright", legend = c('0%','5%','10%'), fill =c("lightblue", "pink", "red",'magenta','cyan','yellow','hotpink','green','orange','navy', 'black', 'white') , cex = 0.3)
barplot(tmp_prop, main =  "할부 요인에 따른 카테고리별 판매건수 비율", 
        xlab = "카테고리", ylab = '건수', las = 2,
        col = c("lightblue", "gray", "pink",'red','black','yellow','hotpink','green','orange','navy', 'black', 'red'))



#-------------------------------------------------------------
# buyer_nm / (inst_mon, inst_fee) / net_amt
tmp <- aggregate(net_amt ~ buyer_nm_f+ inst_mon_f, data, sum, drop=FALSE)
tmp[is.na(tmp)] <- 0
tmp

temp <- matrix(as.numeric(tmp$net_amt), ncol=length(unique(tmp$buyer_nm_f)),  byrow=TRUE)
colnames(temp) <- levels(tmp$buyer_nm_f)
rownames(temp) <- levels(tmp$inst_mon_f)
temp <- as.table(temp)
temp
temp_prop <- prop.table(temp, 2)
temp_prop
barplot(temp, main =  "할부 요인에 따른 카테고리별 판매금액 비교", 
        xlab = "카테고리", ylab = '금액', las =2,
        col = c("lightblue", "gray", "pink",'red','black','yellow','hotpink','green','orange','navy', 'black', 'red'))


barplot(temp_prop, main =  "할인 요인에 따른 카테고리별 판매금액 비율 비교", 
        xlab = "카테고리", ylab = '금액', las = 2,
        col = c("lightblue", "gray", "pink",'red','black','yellow','hotpink','green','orange','navy', 'black', 'red'))








#-------------------------------------------

data_x <- data[data$inst_fee==0,]
data_o <- data[data$inst_fee==1,]

inst_x <- table(data_x$inst_mon, data_x$str_nm_f)
inst_o <- table(data_o$inst_mon, data_o$str_nm_f)

from <- list(1,2,3,c(4:12))
to <- list(1,2,3,4)

tmp <- recodeVar(data_x$inst_mon, src = from, tgt = to)
data_x$inst_mon <- tmp
inst_x <- table(data_x$inst_mon, data_x$str_nm_f)
inst_x

#-------------------------------------------

tmp_x_ctr <- tmp_x[,1] %>% scale(center = TRUE)
tmp_x_ctr
summary(tmp_x_ctr)
psych::describe(tmp_x_ctr)

tmp_x_snd <- tmp_x_ctr %>%
  scale(center = TRUE, scale = TRUE)
tmp_x_snd
summary(tmp_x_snd)
psych::describe

tmp_x_ctr[,1]
boxplot(tmp_x_ctr[1,])

summary(tmp_x[,1])
#-----------
par(mfrow=c(2,1))
opar <- par(no.readonly = TRUE)
x2 <- .6
half <- x2/17
space_ <- 1
par(fig = c(0, x2, 0, 1))      
barplot(tmp_x,col=c("lightblue","blue"),axisnames=FALSE,space=space_)
par(fig = c(half, x2+half, 0, 1), new = TRUE)        
barplot(tmp_o,col=c("salmon","red"), axes=FALSE,space=space_)


#-------------------------------------------------------------


