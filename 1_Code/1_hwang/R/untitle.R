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

data_main <- data[data$str_nm_f=='본점',]
#--------------------------------------------------
# buyer_nm / dc_rate / count
tmp <- table(data_main$dc_rate, data_main$buyer_nm_f)
tmp
tmp_prop <- prop.table(tmp, 2)
tmp_prop <- round(tmp_prop, 4) *100
tmp_prop
par(mfrow=c(2,1))
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


str(data)
#-------------------------------------------------------------
# barplot 그리기 ---------------------------------------------
#-------------------------------------------------------------
#-------------------------------------------------------------
#-------------------------------------------------------------
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
# inst_tot / 무이자 할부 = 1/ 유이자 할부 = 2/ 일시불  = 3

str(data)
data_tmp <- data
fee <- data[(data$inst_fee == 1) & (data$inst_mon > 1),]
notfee <- data[(data$inst_fee ==0) & (data$inst_mon) >1,]
pay <- data[data$inst_mon <= 1,]
notfee$inst_tot <- 1
fee$inst_tot <- 2
pay$inst_tot <- 3

#notfee <- subset(notfee, select=c("inst_fee", "inst_mon", "inst_tot"))
tmp <- rbind(notfee, fee)
tmp <- rbind(tmp, pay)
#data_tmp <- merge(data_tmp , notfee , by = c("inst_fee", "inst_mon"))
li <- colnames(data_tmp)
class(li)

li <- li[-36]
li
data_tmp <- merge(data_tmp , tmp , by = li)
str(data_tmp)

tmp <- as.data.frame(table(data_tmp$inst_mon, data_tmp$inst_fee))
tmp
names(tmp) <- c('inst_mon', 'inst_fee', 'inst_tot')
tmp
tmp$inst_tot <- c(3, 1, 1, 1, 1, 1 ,1, 1 ,1 ,1 ,1, 1, 3, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2 )
tmp

data_tmp <- merge(data_tmp, tmp, by = c('inst_mon', 'inst_fee'))
View(data_tmp)
data_inst <- data_tmp
str(data_inst)

#-------------------------------------------------------------
#--------------그래프안에 글자 넣기-------------------------
#---------------ggplot으로 그래프 그리기----------------------------
#-------------------------------------------------------------
# buyer_nm / dc_rate / count
tmp <- table(data_inst$dc_rate, data_inst$buyer_nm_f)
tmp_prop <- prop.table(tmp, 2)
tmp_prop <- round(tmp_prop, 4) *100
#tmp <- as.data.frame(tmp)
#names(tmp) <- c('할인율', '카테고리', '건수')
tmp_prop <- as.data.frame(tmp_prop)
#tmp_prop$test <- c(1:105)
#tmp_prop$test[tmp_prop$test==1] <- NA
names(tmp_prop) <- c('할인율', '카테고리', '건수')
tmp_prop

ggplot(as.data.frame(tmp_prop), aes(x=카테고리, y=건수, fill=할인율)) +
  ggtitle("할인율에 따른 카테고리별 판매건수 비교")+
  geom_bar(stat="identity")+
  geom_text(aes(y=건수, label = paste(건수,"%")),position = position_stack(vjust = 0.5), color = "black", size=3)+
  theme(axis.text.x = element_text(angle=90, hjust = 1, vjust=0, color="black", size=12),
        plot.title = element_text(family="serif", face = "bold", hjust= 0.5, size=20))

#-------------------------------------------------------------
# buyer_nm / dc_rate / net_amt
tmp <- aggregate(net_amt ~ buyer_nm_f + dc_rate_f, data, sum, drop=FALSE)
tmp[is.na(tmp)] <- 0
tmp

temp <- matrix(as.numeric(tmp$net_amt), ncol=length(unique(tmp$buyer_nm_f)),  byrow=TRUE)
colnames(temp) <- levels(tmp$buyer_nm_f)
rownames(temp) <- levels(tmp$dc_rate_f)
temp <- as.table(temp)
tmp_prop <- prop.table(temp, 2)
tmp_prop <- round(tmp_prop, 4)*100
tmp_prop <- as.data.frame(tmp_prop)
tmp_prop
names(tmp_prop) <- c('할인율', '카테고리', '금액')

ggplot(as.data.frame(tmp_prop), aes(x=카테고리, y=금액, fill=할인율)) +
  ggtitle("할인율에 따른 카테고리별 판매금액 비교")+
  geom_bar(stat="identity")+
  geom_text(aes(y=금액, label = paste(금액,"%")),position = position_stack(vjust = 0.5), color = "black", size=3)+
  theme(axis.text.x = element_text(angle=90, hjust = 1, vjust=0, color="black", size=12),
        plot.title = element_text(family="serif", face = "bold", hjust= 0.5, size=20))

#-------------------------------------------------------------
#-------------------------------------------------------------
# inst_tot 팩터형 추가
# inst_tot / 무이자 할부 = 1/ 유이자 할부 = 2/ 일시불  = 3
data_inst$inst_tot_f <- factor(data_inst$inst_tot, levels = c(1:3), labels = c("무이자 할부", "유이자 할부", "일시불"))
#-------------------------------------------------------------
# buyer_nm / inst_tot / count
tmp <- table(data_inst$inst_tot_f, data_inst$buyer_nm_f)
tmp_prop <- prop.table(tmp, 2)
tmp_prop <- round(tmp_prop, 4) *100

tmp_prop <- as.data.frame(tmp_prop)

names(tmp_prop) <- c('할부요인', '카테고리', '건수')
tmp_prop

ggplot(as.data.frame(tmp_prop), aes(x=카테고리, y=건수, fill=할부요인)) +
  ggtitle("할부요인에 따른 카테고리별 판매건수 비교")+
  geom_bar(stat="identity")+
  geom_text(aes(y=건수, label = paste(건수,"%")),position = position_stack(vjust = 0.5), color = "black", size=3)+
  theme(axis.text.x = element_text(angle=90, hjust = 1, vjust=0, color="black", size=12),
        plot.title = element_text(family="serif", face = "bold", hjust= 0.5, size=20))

#-------------------------------------------------------------
# buyer_nm / inst_tot / net_amt
tmp <- aggregate(net_amt ~ buyer_nm_f + inst_tot_f, data_inst, sum, drop=FALSE)
tmp[is.na(tmp)] <- 0
tmp

temp <- matrix(as.numeric(tmp$net_amt), ncol=length(unique(tmp$buyer_nm_f)),  byrow=TRUE)
colnames(temp) <- levels(tmp$buyer_nm_f)
rownames(temp) <- levels(tmp$inst_tot_f)
temp <- as.table(temp)
tmp_prop <- prop.table(temp, 2)
tmp_prop <- round(tmp_prop, 4)*100
tmp_prop <- as.data.frame(tmp_prop)
tmp_prop
names(tmp_prop) <- c('할부요인', '카테고리', '금액')

ggplot(as.data.frame(tmp_prop), aes(x=카테고리, y=금액, fill=할부요인)) +
  ggtitle("할부요인에 따른 카테고리별 판매금액 비교")+
  geom_bar(stat="identity")+
  geom_text(aes(y=금액, label = paste(금액,"%")),position = position_stack(vjust = 0.5), color = "black", size=3)+
  theme(axis.text.x = element_text(angle=90, hjust = 1, vjust=0, color="black", size=12),
        plot.title = element_text(family="serif", face = "bold", hjust= 0.5, size=20))

#-------------------------------------------------------------
# brd_nm / inst_tot / count
tmp <- table(data_inst$inst_tot_f, data_inst$brd_nm_f)
tmp_prop <- prop.table(tmp, 2)
tmp_prop <- round(tmp_prop, 4) *100

tmp_prop <- as.data.frame(tmp_prop)

names(tmp_prop) <- c('할부요인', '브랜드', '건수')
tmp_prop

ggplot(as.data.frame(tmp_prop), aes(x=브랜드, y=건수, fill=할부요인)) +
  ggtitle("할부요인에 따른 브랜드별 판매건수 비교")+
  geom_bar(stat="identity")+
  
  theme(axis.text.x = element_text(angle=90, hjust = 1, vjust=0, color="black", size=12),
        plot.title = element_text(family="serif", face = "bold", hjust= 0.5, size=20))

#-------------------------------------------------------------
# brd_nm / inst_tot / net_amt
tmp <- aggregate(net_amt ~ brd_nm_f + inst_tot_f, data_inst, sum, drop=FALSE)
tmp[is.na(tmp)] <- 0
tmp

temp <- matrix(as.numeric(tmp$net_amt), ncol=length(unique(tmp$brd_nm_f)),  byrow=TRUE)
colnames(temp) <- levels(tmp$brd_nm_f)
rownames(temp) <- levels(tmp$inst_tot_f)
temp <- as.table(temp)
tmp_prop <- prop.table(temp, 2)
tmp_prop <- round(tmp_prop, 4)*100
tmp_prop <- as.data.frame(tmp_prop)
tmp_prop
names(tmp_prop) <- c('할부요인', '브랜드', '금액')

ggplot(as.data.frame(tmp_prop), aes(x=브랜드, y=금액, fill=할부요인)) +
  ggtitle("할부요인에 따른 브랜드별 판매금액 비교")+
  geom_bar(stat="identity")+
  theme(axis.text.x = element_text(angle=90, hjust = 1, vjust=0, color="black", size=12),
        plot.title = element_text(family="serif", face = "bold", hjust= 0.5, size=20))

#-------------------------------------------------------------
# corner_nm / inst_tot / count
str(data_inst)
tmp <- table(data_inst$inst_tot_f, data_inst$corner_nm_f)
tmp_prop <- prop.table(tmp, 2)
tmp_prop <- round(tmp_prop, 4) *100

tmp_prop <- as.data.frame(tmp_prop)

names(tmp_prop) <- c('할부요인', '코너', '건수')
tmp_prop

ggplot(as.data.frame(tmp_prop), aes(x=코너, y=건수, fill=할부요인)) +
  ggtitle("할부요인에 따른 코너별 판매건수 비교")+
  geom_bar(stat="identity")+
  theme(axis.text.x = element_text(angle=90, hjust = 1, vjust=0, color="black", size=4),
        plot.title = element_text(family="serif", face = "bold", hjust= 0.5, size=20))

#-------------------------------------------------------------
# corner_nm / inst_tot / net_amt
tmp <- aggregate(net_amt ~ corner_nm_f + inst_tot_f, data_inst, sum, drop=FALSE)
tmp[is.na(tmp)] <- 0
tmp

temp <- matrix(as.numeric(tmp$net_amt), ncol=length(unique(tmp$corner_nm_f)),  byrow=TRUE)
colnames(temp) <- levels(tmp$corner_nm_f)
rownames(temp) <- levels(tmp$inst_tot_f)
temp
temp <- as.table(temp)
tmp_prop <- prop.table(temp, 2)
tmp_prop <- round(tmp_prop, 4)*100
tmp_prop <- as.data.frame(tmp_prop)
tmp_prop
names(tmp_prop) <- c('할부요인', '코너', '금액')

ggplot(as.data.frame(tmp_prop), aes(x=코너, y=금액, fill=할부요인)) +
  ggtitle("할부요인에 따른 코너별 판매금액 비교")+
  geom_bar(stat="identity")+
  theme(axis.text.x = element_text(angle=90, hjust = 1, vjust=0, color="black", size=3),
        plot.title = element_text(family="serif", face = "bold", hjust= 0.5, size=20))

p1 <- ggplot(tmp_prop, aes(할부요인, 금액))+
  geom_point(color = "red", shape = 20, size=2)
p2 <- ggplot(tmp_prop, aes(할부요인, 금액))+
  geom_jitter(color = "blue", shape = 8, size = 0.8)
p3 <- ggplot(tmp_prop, aes(할부요인, 금액))+
  geom_boxplot(fill = "lightblue",
               outlier.color = "orange", outlier.shape = 17,
               outlier.size = 2, notch = TRUE)
p4 <- ggplot(tmp_prop, aes(할부요인, 금액))+
  geom_violin(fill = "lightpink")



grid.arrange(p1, p2, p3, p4, nrow=2, ncol=2)

#-------------------------------------------------------------
# str_nm / inst_tot / count
tmp <- table(data_inst$inst_tot_f, data_inst$str_nm_f)
tmp_prop <- prop.table(tmp, 2)
tmp_prop <- round(tmp_prop, 4) *100

tmp_prop <- as.data.frame(tmp_prop)

names(tmp_prop) <- c('할부요인', '지점', '건수')
tmp_prop

ggplot(as.data.frame(tmp_prop), aes(x=지점, y=건수, fill=할부요인)) +
  ggtitle("할부요인에 따른 지점별 판매건수 비교")+
  geom_bar(stat="identity")+
  geom_text(aes(y=건수, label = paste(건수,"%")),position = position_stack(vjust = 0.5), color = "black", size=6)+
  theme(axis.text.x = element_text(angle=0, hjust = 1, vjust=0, color="black", size=15),
        plot.title = element_text(family="serif", face = "bold", hjust= 0.5, size=20))

#-------------------------------------------------------------
# str_nm / inst_tot / net_amt
tmp <- aggregate(net_amt ~ str_nm_f + inst_tot_f, data_inst, sum, drop=FALSE)
tmp[is.na(tmp)] <- 0
tmp

temp <- matrix(as.numeric(tmp$net_amt), ncol=length(unique(tmp$str_nm_f)),  byrow=TRUE)
colnames(temp) <- levels(tmp$str_nm_f)
rownames(temp) <- levels(tmp$inst_tot_f)
temp <- as.table(temp)
tmp_prop <- prop.table(temp, 2)
tmp_prop <- round(tmp_prop, 4)*100
tmp_prop <- as.data.frame(tmp_prop)
tmp_prop
names(tmp_prop) <- c('할부요인', '지점', '금액')

ggplot(as.data.frame(tmp_prop), aes(x=지점, y=금액, fill=할부요인)) +
  ggtitle("할부요인에 따른 지점별 판매금액 비교")+
  geom_bar(stat="identity")+
  geom_text(aes(y=금액, label = paste(금액,"%")),position = position_stack(vjust = 0.5), color = "black", size=6)+
  theme(axis.text.x = element_text(angle=0, hjust = 1, vjust=0, color="black", size=15),
        plot.title = element_text(family="serif", face = "bold", hjust= 0.5, size=20))

#-------------------------------------------------------------
# team_nm / inst_tot / count
tmp <- table(data_inst$inst_tot_f, data_inst$team_nm_f)
tmp_prop <- prop.table(tmp, 2)
tmp_prop <- round(tmp_prop, 4) *100

tmp_prop <- as.data.frame(tmp_prop)

names(tmp_prop) <- c('할부요인', '팀', '건수')
tmp_prop

ggplot(as.data.frame(tmp_prop), aes(x=팀, y=건수, fill=할부요인)) +
  ggtitle("할부요인에 따른 팀별 판매건수 비교")+
  geom_bar(stat="identity")+
  geom_text(aes(y=건수, label = paste(건수,"%")),position = position_stack(vjust = 0.5), color = "black", size=6)+
  theme(axis.text.x = element_text(angle=0,  color="black", size=15),
        plot.title = element_text(family="serif", face = "bold", hjust= 0.5, size=20))

#-------------------------------------------------------------
# team_nm / inst_tot / net_amt
tmp <- aggregate(net_amt ~ team_nm_f + inst_tot_f, data_inst, sum, drop=FALSE)
tmp[is.na(tmp)] <- 0
tmp

temp <- matrix(as.numeric(tmp$net_amt), ncol=length(unique(tmp$team_nm_f)),  byrow=TRUE)
colnames(temp) <- levels(tmp$team_nm_f)
rownames(temp) <- levels(tmp$inst_tot_f)
temp <- as.table(temp)
tmp_prop <- prop.table(temp, 2)
tmp_prop <- round(tmp_prop, 4)*100
tmp_prop <- as.data.frame(tmp_prop)
tmp_prop
names(tmp_prop) <- c('할부요인', '팀', '금액')

ggplot(as.data.frame(tmp_prop), aes(x=팀, y=금액, fill=할부요인)) +
  ggtitle("할부요인에 따른 팀별 판매금액 비교")+
  geom_bar(stat="identity")+
  geom_text(aes(y=금액, label = paste(금액,"%")),position = position_stack(vjust = 0.5), color = "black", size=6)+
  theme(axis.text.x = element_text(angle=0,  color="black", size=15),
        plot.title = element_text(family="serif", face = "bold", hjust= 0.5, size=20))



#-------------------------------------------------------------
#-------------------------------------------------------------
tmp <- aggregate(net_amt ~ dc_rate_f, data_inst, sum, drop=FALSE)
tmp[is.na(tmp)] <- 0
tmp


#-------------------------------------------------------------
#          내 할당량  => 월 / 요일 / 일 / 개별판매액
#-------------------------------------------------------------

# 요일 데이터 만들기
tmp <- as.POSIXlt(data_inst$sales_date)
tmp
str(tmp)
data_inst$sales_date_wday <- tmp$wday
data_inst$sales_date_wday_f <- factor(data_inst$sales_date_wday, levels=c(0:6), labels =c('일', '월', '화', '수', '목' ,'금', '토') )
str(data_inst)
# -----------------------------------------------------------------------------
# sales_date_month / inst_tot / count
tmp <- table(data_inst$inst_tot_f, data_inst$sales_date_month_f)
tmp_prop <- prop.table(tmp, 2)
tmp_prop <- round(tmp_prop, 4) *100

tmp_prop <- as.data.frame(tmp_prop)

names(tmp_prop) <- c('할부요인', '년월', '건수')
tmp_prop

ggplot(as.data.frame(tmp_prop), aes(x=년월, y=건수, fill=할부요인)) +
  ggtitle("할부요인에 따른 월별 판매건수 비교")+
  geom_bar(stat="identity")+
  geom_text(aes(y=건수, label = paste(건수,"%")),position = position_stack(vjust = 0.5), color = "black", size=5)+
  theme(axis.text.x = element_text(angle=0,  vjust=0, color="black", size=12),
        plot.title = element_text(family="serif", face = "bold", hjust= 0.5, size=20))

#-------------------------------------------------------------
# sales_date_month / inst_tot / net_amt
tmp <- aggregate(net_amt ~ sales_date_month_f + inst_tot_f, data_inst, sum, drop=FALSE)
tmp[is.na(tmp)] <- 0
tmp

temp <- matrix(as.numeric(tmp$net_amt), ncol=length(unique(tmp$sales_date_month_f)),  byrow=TRUE)
colnames(temp) <- levels(tmp$sales_date_month_f)
rownames(temp) <- levels(tmp$inst_tot_f)
temp <- as.table(temp)
tmp_prop <- prop.table(temp, 2)
tmp_prop <- round(tmp_prop, 4)*100
tmp_prop <- as.data.frame(tmp_prop)
tmp_prop
names(tmp_prop) <- c('할부요인', '년월', '금액')

ggplot(as.data.frame(tmp_prop), aes(x=년월, y=금액, fill=할부요인)) +
  ggtitle("할부요인에 따른 월별 판매금액 비교")+
  geom_bar(stat="identity")+
  geom_text(aes(y=금액, label = paste(금액,"%")),position = position_stack(vjust = 0.5), color = "black", size=5)+
  theme(axis.text.x = element_text(angle=0, vjust=0, color="black", size=12),
        plot.title = element_text(family="serif", face = "bold", hjust= 0.5, size=20))


# -----------------------------------------------------------------------------
# sales_date_month / dc_rate / count
tmp <- table(data_inst$dc_rate_f, data_inst$sales_date_month_f)
tmp_prop <- prop.table(tmp, 2)
tmp_prop <- round(tmp_prop, 4) *100

tmp_prop <- as.data.frame(tmp_prop)

names(tmp_prop) <- c('할인율', '년월', '건수')
tmp_prop

ggplot(as.data.frame(tmp_prop), aes(x=년월, y=건수, fill=할인율)) +
  ggtitle("할인율에 따른 월별 판매건수 비교")+
  geom_bar(stat="identity")+
  geom_text(aes(y=건수, label = paste(건수,"%")),position = position_stack(vjust = 0.5), color = "black", size=5)+
  theme(axis.text.x = element_text(angle=0,  vjust=0, color="black", size=12),
        plot.title = element_text(family="serif", face = "bold", hjust= 0.5, size=20))

#-------------------------------------------------------------
# sales_date_month / dc_rate / net_amt
tmp <- aggregate(net_amt ~ sales_date_month_f + dc_rate_f, data_inst, sum, drop=FALSE)
tmp[is.na(tmp)] <- 0
tmp

temp <- matrix(as.numeric(tmp$net_amt), ncol=length(unique(tmp$sales_date_month_f)),  byrow=TRUE)
colnames(temp) <- levels(tmp$sales_date_month_f)
rownames(temp) <- levels(tmp$dc_rate_f)
temp <- as.table(temp)
tmp_prop <- prop.table(temp, 2)
tmp_prop <- round(tmp_prop, 4)*100
tmp_prop <- as.data.frame(tmp_prop)
tmp_prop
names(tmp_prop) <- c('할인율', '년월', '금액')

ggplot(as.data.frame(tmp_prop), aes(x=년월, y=금액, fill=할인율)) +
  ggtitle("할인율에 따른 월별 판매금액 비교")+
  geom_bar(stat="identity")+
  geom_text(aes(y=금액, label = paste(금액,"%")),position = position_stack(vjust = 0.5), color = "black", size=5)+
  theme(axis.text.x = element_text(angle=0, vjust=0, color="black", size=12),
        plot.title = element_text(family="serif", face = "bold", hjust= 0.5, size=20))


# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# 요일별
# sales_date_wday / inst_tot / count
tmp <- table(data_inst$inst_tot_f, data_inst$sales_date_wday_f)
tmp_prop <- prop.table(tmp, 2)
tmp_prop <- round(tmp_prop, 4) *100

tmp_prop <- as.data.frame(tmp_prop)

names(tmp_prop) <- c('할부요인', '요일', '건수')
tmp_prop

ggplot(as.data.frame(tmp_prop), aes(x=요일, y=건수, fill=할부요인)) +
  ggtitle("할부요인에 따른 요일별 판매건수 비교")+
  geom_bar(stat="identity")+
  geom_text(aes(y=건수, label = paste(건수,"%")),position = position_stack(vjust = 0.5), color = "black", size=5)+
  theme(axis.text.x = element_text(angle=0,  vjust=0, color="black", size=12),
        plot.title = element_text(family="serif", face = "bold", hjust= 0.5, size=20))

#-------------------------------------------------------------
# sales_date_wday / inst_tot / net_amt
tmp <- aggregate(net_amt ~ sales_date_wday_f + inst_tot_f, data_inst, sum, drop=FALSE)
tmp[is.na(tmp)] <- 0
tmp

temp <- matrix(as.numeric(tmp$net_amt), ncol=length(unique(tmp$sales_date_wday_f)),  byrow=TRUE)
colnames(temp) <- levels(tmp$sales_date_wday_f)
rownames(temp) <- levels(tmp$inst_tot_f)
temp <- as.table(temp)
tmp_prop <- prop.table(temp, 2)
tmp_prop <- round(tmp_prop, 4)*100
tmp_prop <- as.data.frame(tmp_prop)
tmp_prop
names(tmp_prop) <- c('할부요인', '요일', '금액')

ggplot(as.data.frame(tmp_prop), aes(x=요일, y=금액, fill=할부요인)) +
  ggtitle("할부요인에 따른 요일별 판매금액 비교")+
  geom_bar(stat="identity")+
  geom_text(aes(y=금액, label = paste(금액,"%")),position = position_stack(vjust = 0.5), color = "black", size=5)+
  theme(axis.text.x = element_text(angle=0, vjust=0, color="black", size=12),
        plot.title = element_text(family="serif", face = "bold", hjust= 0.5, size=20))


# -----------------------------------------------------------------------------
# sales_date_wday / dc_rate / count
tmp <- table(data_inst$dc_rate_f, data_inst$sales_date_wday_f)
tmp_prop <- prop.table(tmp, 2)
tmp_prop <- round(tmp_prop, 4) *100

tmp_prop <- as.data.frame(tmp_prop)

names(tmp_prop) <- c('할인율', '요일', '건수')
tmp_prop

ggplot(as.data.frame(tmp_prop), aes(x=요일, y=건수, fill=할인율)) +
  ggtitle("할인율에 따른 요일별 판매건수 비교")+
  geom_bar(stat="identity")+
  geom_text(aes(y=건수, label = paste(건수,"%")),position = position_stack(vjust = 0.5), color = "black", size=5)+
  theme(axis.text.x = element_text(angle=0,  vjust=0, color="black", size=12),
        plot.title = element_text(family="serif", face = "bold", hjust= 0.5, size=20))

#-------------------------------------------------------------
# sales_date_wday / dc_rate / net_amt
tmp <- aggregate(net_amt ~ sales_date_wday_f + dc_rate_f, data_inst, sum, drop=FALSE)
tmp[is.na(tmp)] <- 0
tmp

temp <- matrix(as.numeric(tmp$net_amt), ncol=length(unique(tmp$sales_date_wday_f)),  byrow=TRUE)
colnames(temp) <- levels(tmp$sales_date_wday_f)
rownames(temp) <- levels(tmp$dc_rate_f)
temp <- as.table(temp)
tmp_prop <- prop.table(temp, 2)
tmp_prop <- round(tmp_prop, 4)*100
tmp_prop <- as.data.frame(tmp_prop)
tmp_prop
names(tmp_prop) <- c('할인율', '요일', '금액')

ggplot(as.data.frame(tmp_prop), aes(x=요일, y=금액, fill=할인율)) +
  ggtitle("할인율에 따른 요일별 판매금액 비교")+
  geom_bar(stat="identity")+
  geom_text(aes(y=금액, label = paste(금액,"%")),position = position_stack(vjust = 0.5), color = "black", size=5)+
  theme(axis.text.x = element_text(angle=0, vjust=0, color="black", size=12),
        plot.title = element_text(family="serif", face = "bold", hjust= 0.5, size=20))


# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# sales_date_day 팩터형 추가
data_inst$sales_date_day_f <- factor(data_inst$sales_date_day, levels=c(1:31), labels = c(1:31))
# -----------------------------------------------------------------------------
# 일자별
# sales_date_day / inst_tot / count
tmp <- table(data_inst$inst_tot_f, data_inst$sales_date_day)
tmp_prop <- prop.table(tmp, 2)
tmp_prop <- round(tmp_prop, 4) *100

tmp_prop <- as.data.frame(tmp_prop)

names(tmp_prop) <- c('할부요인', '일', '건수')
tmp_prop

ggplot(as.data.frame(tmp_prop), aes(x=일, y=건수, fill=할부요인)) +
  ggtitle("할부요인에 따른 일자별 판매건수 비교")+
  geom_bar(stat="identity")+
  geom_text(aes(y=건수, label = paste(건수,"%")),position = position_stack(vjust = 0.5), color = "black", size=3)+
  theme(axis.text.x = element_text(angle=0,  vjust=0, color="black", size=12),
        plot.title = element_text(family="serif", face = "bold", hjust= 0.5, size=20))

#-------------------------------------------------------------
# sales_date_day / inst_tot / net_amt
tmp <- aggregate(net_amt ~ sales_date_day_f + inst_tot_f, data_inst, sum, drop=FALSE)
tmp[is.na(tmp)] <- 0
tmp

temp <- matrix(as.numeric(tmp$net_amt), ncol=length(unique(tmp$sales_date_day_f)),  byrow=TRUE)
colnames(temp) <- levels(tmp$sales_date_day_f)
rownames(temp) <- levels(tmp$inst_tot_f)
temp <- as.table(temp)
tmp_prop <- prop.table(temp, 2)
tmp_prop <- round(tmp_prop, 4)*100
tmp_prop <- as.data.frame(tmp_prop)
tmp_prop
names(tmp_prop) <- c('할부요인', '일', '금액')

ggplot(as.data.frame(tmp_prop), aes(x=일, y=금액, fill=할부요인)) +
  ggtitle("할부요인에 따른 일자별 판매금액 비교")+
  geom_bar(stat="identity")+
  geom_text(aes(y=금액, label = paste(금액,"%")),position = position_stack(vjust = 0.5), color = "black", size=3)+
  theme(axis.text.x = element_text(angle=0, vjust=0, color="black", size=12),
        plot.title = element_text(family="serif", face = "bold", hjust= 0.5, size=20))


# -----------------------------------------------------------------------------
# sales_date_day / dc_rate / count
tmp <- table(data_inst$dc_rate_f, data_inst$sales_date_day)
tmp_prop <- prop.table(tmp, 2)
tmp_prop <- round(tmp_prop, 4) *100

tmp_prop <- as.data.frame(tmp_prop)

names(tmp_prop) <- c('할인율', '일', '건수')
tmp_prop

ggplot(as.data.frame(tmp_prop), aes(x=일, y=건수, fill=할인율)) +
  ggtitle("할인율에 따른 일자별 판매건수 비교")+
  geom_bar(stat="identity")+
  geom_text(aes(y=건수, label = paste(건수,"%")),position = position_stack(vjust = 0.5), color = "black", size=3)+
  theme(axis.text.x = element_text(angle=0,  vjust=0, color="black", size=12),
        plot.title = element_text(family="serif", face = "bold", hjust= 0.5, size=20))

#-------------------------------------------------------------
# sales_date_day / dc_rate / net_amt
tmp <- aggregate(net_amt ~ sales_date_day_f + dc_rate_f, data_inst, sum, drop=FALSE)
tmp[is.na(tmp)] <- 0
tmp

temp <- matrix(as.numeric(tmp$net_amt), ncol=length(unique(tmp$sales_date_day_f)),  byrow=TRUE)
temp
colnames(temp) <- levels(tmp$sales_date_day_f)
rownames(temp) <- levels(tmp$dc_rate_f)
temp
temp <- as.table(temp)
tmp_prop <- prop.table(temp, 2)
tmp_prop <- round(tmp_prop, 4)*100
tmp_prop <- as.data.frame(tmp_prop)
tmp_prop
names(tmp_prop) <- c('할인율', '일', '금액')

ggplot(as.data.frame(tmp_prop), aes(x=일, y=금액, fill=할인율)) +
  ggtitle("할인율에 따른 일자별 판매금액 비교")+
  geom_bar(stat="identity")+
  geom_text(aes(y=금액, label = paste(금액,"%")),position = position_stack(vjust = 0.5), color = "black", size=3)+
  theme(axis.text.x = element_text(angle=0, vjust=0, color="black", size=12),
        plot.title = element_text(family="serif", face = "bold", hjust= 0.5, size=20))


#-------------------------------------------------------------
# 개별 판매액 ??
# tot_amt범주화
tot_amt_n <- cut(data_inst$tot_amt, breaks = c(-Inf, (seq(-1000000, 1000000, 5000)), Inf ))
tot_amt_n
tot_amt_nf <- as.factor(tot_amt_n)
tot_amt_nf
unique(tot_amt_n)
tot_amt_nf <- factor(tot_amt_n, levels= unique(tot_amt_n), labels = c(1:402))
tot_amt_nf


psych::describe(tot_amt_nf)
Hmisc::describe(tot_amt_nf)
skim(tot_amt_nf)
tot_t <- table(tot_amt_nf)
tot_t <- as.data.frame(tot_t)
tot_t
summary(tot_t)

barplot(tot_t,
        main = '금액별 거래 횟수 분포 비교',
        xlab = '금액', ylab='거래 횟수')

#-------------------------------------------------------------
#-------------------------------------------------------------
# 시간 30분 단위로 나누기
str(data_inst)
# ---------------------------------------------------------
# sales_time_hour 
# sales_tune 에서 거래 시간별로 데이터 가공
date_time_time <- transform(data$sales_time,
                            sales_time_hour1 = sprintf("%04d", data$sales_time))
date_time_time$sales_time_hour1

date_time_time <- date_time_time$sales_time_hour1
date_time_time

date_time_time <- substr(date_time_time, 0, 2)
unique(date_time_time)
date_time_time
date_time_time <- as.integer(date_time_time)
data$sales_time_hour <- date_time_time
str(data)


summary(data_inst$net_amt)

#-------------------------------------------------------------
#-------------------------------------------------------------
data_pos <- data_inst[data_inst$net_amt>=0,]
summary(data_pos$net_amt)
#---------------------------------------------------------------
#---------------------------------------------------------------
#---------------------------------------------------------------
#---------------------------------------------------------------
# refund 상품 제외하고 뽑기

#-------------------------------------------------------------
#          내 할당량  => 월 / 요일 / 일 / 개별판매액
#-------------------------------------------------------------
#---------------------------------------------------------------------------
# sales_date_month / inst_tot / count
tmp <- table(data_pos$inst_tot_f, data_pos$sales_date_month_f)
tmp
tmp_prop <- prop.table(tmp, 2)
tmp_prop <- round(tmp_prop, 4) *100

tmp_prop <- as.data.frame(tmp_prop)


names(tmp_prop) <- c('할부요인', '년월', '건수')
tmp_prop

ggplot(as.data.frame(tmp_prop), aes(x=년월, y=건수, fill=할부요인)) +
  ggtitle("할부요인에 따른 월별 판매건수 비교")+
  geom_bar(stat="identity")+
  geom_text(aes(y=건수, label = paste(건수,"%")),position = position_stack(vjust = 0.5), color = "black", size=5)+
  theme(axis.text.x = element_text(angle=0,  vjust=0, color="black", size=12),
        plot.title = element_text(family="serif", face = "bold", hjust= 0.5, size=20))
tmp <- as.data.frame(tmp)
tmp
names(tmp) <- c('할부요인', '년월', '건수')

ggplot(as.data.frame(tmp), aes(x=년월, y=건수, fill=할부요인)) +
  ggtitle("할부요인에 따른 월별 판매건수 비교")+
  geom_bar(stat="identity")+
  geom_text(aes(y=건수, label = paste(건수)),position = position_stack(vjust = 0.5), color = "black", size=5)+
  theme(axis.text.x = element_text(angle=0,  vjust=0, color="black", size=12),
        plot.title = element_text(family="serif", face = "bold", hjust= 0.5, size=20))
#-------------------------------------------------------------
# sales_date_month / inst_tot / tot_amt
tmp <- aggregate(tot_amt ~ sales_date_month_f + inst_tot_f, data_pos, sum, drop=FALSE)
tmp[is.na(tmp)] <- 0
tmp

temp <- matrix(as.numeric(tmp$tot_amt), ncol=length(unique(tmp$sales_date_month_f)),  byrow=TRUE)
colnames(temp) <- levels(tmp$sales_date_month_f)
rownames(temp) <- levels(tmp$inst_tot_f)
temp <- as.table(temp)
tmp_prop <- prop.table(temp, 2)
tmp_prop <- round(tmp_prop, 4)*100
tmp_prop <- as.data.frame(tmp_prop)
tmp_prop
names(tmp_prop) <- c('할부요인', '년월', '금액')

ggplot(as.data.frame(tmp_prop), aes(x=년월, y=금액, fill=할부요인)) +
  ggtitle("할부요인에 따른 월별 판매금액 비교")+
  geom_bar(stat="identity")+
  geom_text(aes(y=금액, label = paste(금액,"%")),position = position_stack(vjust = 0.5), color = "black", size=5)+
  theme(axis.text.x = element_text(angle=0, vjust=0, color="black", size=12),
        plot.title = element_text(family="serif", face = "bold", hjust= 0.5, size=20))

tmp <- as.data.frame(tmp)
tmp
names(tmp) <- c('년월', '할부요인', '금액')
ggplot(as.data.frame(tmp), aes(x=년월, y=금액, fill=할부요인)) +
  ggtitle("할부요인에 따른 월별 판매금액 비교")+
  geom_bar(stat="identity")+
  geom_text(aes(y=금액, label = paste(금액)),position = position_stack(vjust = 0.5), color = "black", size=5)+
  theme(axis.text.x = element_text(angle=0, vjust=0, color="black", size=12),
        plot.title = element_text(family="serif", face = "bold", hjust= 0.5, size=20))

# -----------------------------------------------------------------------------
# sales_date_month / dc_rate / count
tmp <- table(data_pos$dc_rate_f, data_pos$sales_date_month_f)
tmp_prop <- prop.table(tmp, 2)
tmp_prop <- round(tmp_prop, 4) *100

tmp_prop <- as.data.frame(tmp_prop)

names(tmp_prop) <- c('할인율', '년월', '건수')
tmp_prop

ggplot(as.data.frame(tmp_prop), aes(x=년월, y=건수, fill=할인율)) +
  ggtitle("할인율에 따른 월별 판매건수 비교")+
  geom_bar(stat="identity")+
  geom_text(aes(y=건수, label = paste(건수,"%")),position = position_stack(vjust = 0.5), color = "black", size=5)+
  theme(axis.text.x = element_text(angle=0,  vjust=0, color="black", size=12),
        plot.title = element_text(family="serif", face = "bold", hjust= 0.5, size=20))
tmp <- as.data.frame(tmp)
tmp
names(tmp) <- c('할인율', '년월', '건수')
ggplot(as.data.frame(tmp), aes(x=년월, y=건수, fill=할인율)) +
  ggtitle("할인율에 따른 월별 판매건수 비교")+
  geom_bar(stat="identity")+
  geom_text(aes(y=건수, label = paste(건수)),position = position_stack(vjust = 0.5), color = "black", size=5)+
  theme(axis.text.x = element_text(angle=0,  vjust=0, color="black", size=12),
        plot.title = element_text(family="serif", face = "bold", hjust= 0.5, size=20))

tmp_prop <- tmp_prop[tmp_prop$할인율=='0%' ,]
tmp_prop <- tmp_prop[,-1]
tmp_prop
tmp_prop <- aggregate(건수 ~ 년월 , data=tmp_prop, sum, drop=FALSE)

plot(건수 ~ 년월, data=tmp_prop)
lines(lowess(tmp_prop$건수 ~ tmp_prop$년월), col="black")
par(new=TRUE)
par(mfrow=c(1,1))
#-------------------------------------------------------------
# sales_date_month / dc_rate / tot_amt
tmp <- aggregate(tot_amt ~ sales_date_month_f + dc_rate_f, data_pos, sum, drop=FALSE)
tmp[is.na(tmp)] <- 0
tmp

temp <- matrix(as.numeric(tmp$tot_amt), ncol=length(unique(tmp$sales_date_month_f)),  byrow=TRUE)
colnames(temp) <- levels(tmp$sales_date_month_f)
rownames(temp) <- levels(tmp$dc_rate_f)
temp <- as.table(temp)
tmp_prop <- prop.table(temp, 2)
tmp_prop <- round(tmp_prop, 4)*100
tmp_prop <- as.data.frame(tmp_prop)
tmp_prop
names(tmp_prop) <- c('할인율', '년월', '금액')

ggplot(as.data.frame(tmp_prop), aes(x=년월, y=금액, fill=할인율)) +
  ggtitle("할인율에 따른 월별 판매금액 비교")+
  geom_bar(stat="identity")+
  geom_text(aes(y=금액, label = paste(금액,"%")),position = position_stack(vjust = 0.5), color = "black", size=5)+
  theme(axis.text.x = element_text(angle=0, vjust=0, color="black", size=12),
        plot.title = element_text(family="serif", face = "bold", hjust= 0.5, size=20))

tmp <- as.data.frame(tmp)
tmp
names(tmp) <- c('년월', '할인율', '금액')
ggplot(as.data.frame(tmp), aes(x=년월, y=금액, fill=할인율)) +
  ggtitle("할인율에 따른 월별 판매금액 비교")+
  geom_bar(stat="identity")+
  geom_text(aes(y=금액, label = paste(금액)),position = position_stack(vjust = 0.5), color = "black", size=5)+
  theme(axis.text.x = element_text(angle=0, vjust=0, color="black", size=12),
        plot.title = element_text(family="serif", face = "bold", hjust= 0.5, size=20))

# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# 요일별
# sales_date_wday / inst_tot / count
tmp <- table(data_pos$inst_tot_f, data_pos$sales_date_wday_f)
tmp_prop <- prop.table(tmp, 2)
tmp_prop <- round(tmp_prop, 4) *100

tmp_prop <- as.data.frame(tmp_prop)

names(tmp_prop) <- c('할부요인', '요일', '건수')
tmp_prop

ggplot(as.data.frame(tmp_prop), aes(x=요일, y=건수, fill=할부요인)) +
  ggtitle("할부요인에 따른 요일별 판매건수 비교")+
  geom_bar(stat="identity")+
  geom_text(aes(y=건수, label = paste(건수,"%")),position = position_stack(vjust = 0.5), color = "black", size=5)+
  theme(axis.text.x = element_text(angle=0,  vjust=0, color="black", size=12),
        plot.title = element_text(family="serif", face = "bold", hjust= 0.5, size=20))

tmp <- as.data.frame(tmp)
tmp
names(tmp) <- c('할부요인', '요일', '건수')
ggplot(as.data.frame(tmp), aes(x=요일, y=건수, fill=할부요인)) +
  ggtitle("할부요인에 따른 요일별 판매건수 비교")+
  geom_bar(stat="identity")+
  geom_text(aes(y=건수, label = paste(건수)),position = position_stack(vjust = 0.5), color = "black", size=5)+
  theme(axis.text.x = element_text(angle=0,  vjust=0, color="black", size=12),
        plot.title = element_text(family="serif", face = "bold", hjust= 0.5, size=20))

#-------------------------------------------------------------
# sales_date_wday / inst_tot / tot_amt
tmp <- aggregate(tot_amt ~ sales_date_wday_f + inst_tot_f, data_pos, sum, drop=FALSE)
tmp[is.na(tmp)] <- 0
tmp

temp <- matrix(as.numeric(tmp$tot_amt), ncol=length(unique(tmp$sales_date_wday_f)),  byrow=TRUE)
colnames(temp) <- levels(tmp$sales_date_wday_f)
rownames(temp) <- levels(tmp$inst_tot_f)
temp <- as.table(temp)
tmp_prop <- prop.table(temp, 2)
tmp_prop <- round(tmp_prop, 4)*100
tmp_prop <- as.data.frame(tmp_prop)
tmp_prop
names(tmp_prop) <- c('할부요인', '요일', '금액')

ggplot(as.data.frame(tmp_prop), aes(x=요일, y=금액, fill=할부요인)) +
  ggtitle("할부요인에 따른 요일별 판매금액 비교")+
  geom_bar(stat="identity")+
  geom_text(aes(y=금액, label = paste(금액,"%")),position = position_stack(vjust = 0.5), color = "black", size=5)+
  theme(axis.text.x = element_text(angle=0, vjust=0, color="black", size=12),
        plot.title = element_text(family="serif", face = "bold", hjust= 0.5, size=20))

tmp <- as.data.frame(tmp)
tmp
names(tmp) <- c('요일', '할부요인', '금액')

ggplot(as.data.frame(tmp), aes(x=요일, y=금액, fill=할부요인)) +
  ggtitle("할부요인에 따른 요일별 판매금액 비교")+
  geom_bar(stat="identity")+
  geom_text(aes(y=금액, label = paste(금액)),position = position_stack(vjust = 0.5), color = "black", size=5)+
  theme(axis.text.x = element_text(angle=0, vjust=0, color="black", size=12),
        plot.title = element_text(family="serif", face = "bold", hjust= 0.5, size=20))

# -----------------------------------------------------------------------------
# sales_date_wday / dc_rate / count
tmp <- table(data_pos$dc_rate_f, data_pos$sales_date_wday_f)
tmp_prop <- prop.table(tmp, 2)
tmp_prop <- round(tmp_prop, 4) *100

tmp_prop <- as.data.frame(tmp_prop)

names(tmp_prop) <- c('할인율', '요일', '건수')
tmp_prop

ggplot(as.data.frame(tmp_prop), aes(x=요일, y=건수, fill=할인율)) +
  ggtitle("할인율에 따른 요일별 판매건수 비교")+
  geom_bar(stat="identity")+
  geom_text(aes(y=건수, label = paste(건수,"%")),position = position_stack(vjust = 0.5), color = "black", size=5)+
  theme(axis.text.x = element_text(angle=0,  vjust=0, color="black", size=12),
        plot.title = element_text(family="serif", face = "bold", hjust= 0.5, size=20))

tmp <- as.data.frame(tmp)
tmp
names(tmp) <- c('할인율', '요일', '건수')

ggplot(as.data.frame(tmp), aes(x=요일, y=건수, fill=할인율)) +
  ggtitle("할인율에 따른 요일별 판매건수 비교")+
  geom_bar(stat="identity")+
  geom_text(aes(y=건수, label = paste(건수)),position = position_stack(vjust = 0.5), color = "black", size=5)+
  theme(axis.text.x = element_text(angle=0,  vjust=0, color="black", size=12),
        plot.title = element_text(family="serif", face = "bold", hjust= 0.5, size=20))

#-------------------------------------------------------------
# sales_date_wday / dc_rate / tot_amt
tmp <- aggregate(tot_amt ~ sales_date_wday_f + dc_rate_f, data_pos, sum, drop=FALSE)
tmp[is.na(tmp)] <- 0
tmp

temp <- matrix(as.numeric(tmp$tot_amt), ncol=length(unique(tmp$sales_date_wday_f)),  byrow=TRUE)
colnames(temp) <- levels(tmp$sales_date_wday_f)
rownames(temp) <- levels(tmp$dc_rate_f)
temp <- as.table(temp)
tmp_prop <- prop.table(temp, 2)
tmp_prop <- round(tmp_prop, 4)*100
tmp_prop <- as.data.frame(tmp_prop)
tmp_prop
names(tmp_prop) <- c('할인율', '요일', '금액')

ggplot(as.data.frame(tmp_prop), aes(x=요일, y=금액, fill=할인율)) +
  ggtitle("할인율에 따른 요일별 판매금액 비교")+
  geom_bar(stat="identity")+
  geom_text(aes(y=금액, label = paste(금액,"%")),position = position_stack(vjust = 0.5), color = "black", size=5)+
  theme(axis.text.x = element_text(angle=0, vjust=0, color="black", size=12),
        plot.title = element_text(family="serif", face = "bold", hjust= 0.5, size=20))

tmp <- as.data.frame(tmp)
tmp
names(tmp) <- c('요일', '할인율', '금액')
ggplot(as.data.frame(tmp), aes(x=요일, y=금액, fill=할인율)) +
  ggtitle("할인율에 따른 요일별 판매금액 비교")+
  geom_bar(stat="identity")+
  geom_text(aes(y=금액, label = paste(금액)),position = position_stack(vjust = 0.5), color = "black", size=5)+
  theme(axis.text.x = element_text(angle=0, vjust=0, color="black", size=12),
        plot.title = element_text(family="serif", face = "bold", hjust= 0.5, size=20))


# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
#-----------------------------------------------------------------------------
# 일자별
# sales_date_day / inst_tot / count
tmp <- table(data_pos$inst_tot_f, data_pos$sales_date_day)
tmp_prop <- prop.table(tmp, 2)
tmp_prop <- round(tmp_prop, 4) *100

tmp_prop <- as.data.frame(tmp_prop)

names(tmp_prop) <- c('할부요인', '일', '건수')
tmp_prop

ggplot(as.data.frame(tmp_prop), aes(x=일, y=건수, fill=할부요인)) +
  ggtitle("할부요인에 따른 일자별 판매건수 비교")+
  geom_bar(stat="identity")+
  geom_text(aes(y=건수, label = paste(건수,"%")),position = position_stack(vjust = 0.5), color = "black", size=3)+
  theme(axis.text.x = element_text(angle=0,  vjust=0, color="black", size=12),
        plot.title = element_text(family="serif", face = "bold", hjust= 0.5, size=20))

tmp_prop <- tmp_prop[tmp_prop$할부요인=='일시불' ,]
tmp_prop <- tmp_prop[,-1]
tmp_prop
tmp_prop <- aggregate(건수 ~ 일 , data=tmp_prop, sum, drop=FALSE)


plot(건수 ~ 일, data=tmp_prop)
#abline(lm(건수 ~ 일, data=tmp_prop))
lines(lowess(tmp_prop$건수 ~ tmp_prop$일))


tmp <- as.data.frame(tmp)
tmp
names(tmp) <- c('할부요인', '일', '건수')



ggplot(as.data.frame(tmp), aes(x=일, y=건수, fill=할부요인)) +
  ggtitle("할부요인에 따른 일자별 판매건수 비교")+
  geom_bar(stat="identity")+
  geom_text(aes(y=건수, label = paste(건수)),position = position_stack(vjust = 0.5), color = "black", size=3)+
  theme(axis.text.x = element_text(angle=0,  vjust=0, color="black", size=12),
        plot.title = element_text(family="serif", face = "bold", hjust= 0.5, size=20))


#-------------------------------------------------------------
# sales_date_day / inst_tot / tot_amt
tmp <- aggregate(tot_amt ~ sales_date_day_f + inst_tot_f, data_pos, sum, drop=FALSE)
tmp[is.na(tmp)] <- 0
tmp

temp <- matrix(as.numeric(tmp$tot_amt), ncol=length(unique(tmp$sales_date_day_f)),  byrow=TRUE)
colnames(temp) <- levels(tmp$sales_date_day_f)
rownames(temp) <- levels(tmp$inst_tot_f)
temp <- as.table(temp)
tmp_prop <- prop.table(temp, 2)
tmp_prop <- round(tmp_prop, 4)*100
tmp_prop <- as.data.frame(tmp_prop)
tmp_prop
names(tmp_prop) <- c('할부요인', '일', '금액')

ggplot(as.data.frame(tmp_prop), aes(x=일, y=금액, fill=할부요인)) +
  ggtitle("할부요인에 따른 일자별 판매금액 비교")+
  geom_bar(stat="identity")+
  geom_text(aes(y=금액, label = paste(금액,"%")),position = position_stack(vjust = 0.5), color = "black", size=3)+
  theme(axis.text.x = element_text(angle=0, vjust=0, color="black", size=12),
        plot.title = element_text(family="serif", face = "bold", hjust= 0.5, size=20))

tmp <- as.data.frame(tmp)
tmp
names(tmp) <- c('일', '할부요인', '금액')
ggplot(as.data.frame(tmp), aes(x=일, y=금액, fill=할부요인)) +
  ggtitle("할부요인에 따른 일자별 판매금액 비교")+
  geom_bar(stat="identity")+
  geom_text(aes(y=금액, label = paste(금액)),position = position_stack(vjust = 0.5), color = "black", size=3)+
  theme(axis.text.x = element_text(angle=0, vjust=0, color="black", size=12),
        plot.title = element_text(family="serif", face = "bold", hjust= 0.5, size=20))

# -----------------------------------------------------------------------------
# sales_date_day / dc_rate / count
tmp <- table(data_pos$dc_rate_f, data_pos$sales_date_day)
tmp_prop <- prop.table(tmp, 2)
tmp_prop <- round(tmp_prop, 4) *100

tmp_prop <- as.data.frame(tmp_prop)

names(tmp_prop) <- c('할인율', '일', '건수')
tmp_prop

ggplot(as.data.frame(tmp_prop), aes(x=일, y=건수, fill=할인율)) +
  ggtitle("할인율에 따른 일자별 판매건수 비교")+
  geom_bar(stat="identity")+
  geom_text(aes(y=건수, label = paste(건수,"%")),position = position_stack(vjust = 0.5), color = "black", size=3)+
  theme(axis.text.x = element_text(angle=0,  vjust=0, color="black", size=12),
        plot.title = element_text(family="serif", face = "bold", hjust= 0.5, size=20))

tmp <- as.data.frame(tmp)
tmp
names(tmp) <- c('할인율', '일', '건수')
ggplot(as.data.frame(tmp), aes(x=일, y=건수, fill=할인율)) +
  ggtitle("할인율에 따른 일자별 판매건수 비교")+
  geom_bar(stat="identity")+
  geom_text(aes(y=건수, label = paste(건수)),position = position_stack(vjust = 0.5), color = "black", size=3)+
  theme(axis.text.x = element_text(angle=0,  vjust=0, color="black", size=12),
        plot.title = element_text(family="serif", face = "bold", hjust= 0.5, size=20))

tmp_prop <- tmp_prop[tmp_prop$할인율=='10%' ,]
tmp_prop <- tmp_prop[,-1]
tmp_prop
tmp_prop <- aggregate(건수 ~ 일 , data=tmp_prop, sum, drop=FALSE)

plot(건수 ~ 일, data=tmp_prop)
lines(lowess(tmp_prop$건수 ~ tmp_prop$일), col="blue")
par(new=TRUE)

#-------------------------------------------------------------
# sales_date_day / dc_rate / tot_amt
tmp <- aggregate(tot_amt ~ sales_date_day_f + dc_rate_f, data_pos, sum, drop=FALSE)
tmp[is.na(tmp)] <- 0
tmp

temp <- matrix(as.numeric(tmp$tot_amt), ncol=length(unique(tmp$sales_date_day_f)),  byrow=TRUE)
temp
colnames(temp) <- levels(tmp$sales_date_day_f)
rownames(temp) <- levels(tmp$dc_rate_f)
temp
temp <- as.table(temp)
tmp_prop <- prop.table(temp, 2)
tmp_prop <- round(tmp_prop, 4)*100
tmp_prop <- as.data.frame(tmp_prop)
tmp_prop
names(tmp_prop) <- c('할인율', '일', '금액')

ggplot(as.data.frame(tmp_prop), aes(x=일, y=금액, fill=할인율)) +
  ggtitle("할인율에 따른 일자별 판매금액 비교")+
  geom_bar(stat="identity")+
  geom_text(aes(y=금액, label = paste(금액,"%")),position = position_stack(vjust = 0.5), color = "black", size=3)+
  theme(axis.text.x = element_text(angle=0, vjust=0, color="black", size=12),
        plot.title = element_text(family="serif", face = "bold", hjust= 0.5, size=20))
tmp <- as.data.frame(tmp)
tmp
names(tmp) <- c('일', '할인율', '금액')

ggplot(as.data.frame(tmp), aes(x=일, y=금액, fill=할인율)) +
  ggtitle("할인율에 따른 일자별 판매금액 비교")+
  geom_bar(stat="identity")+
  geom_text(aes(y=금액, label = paste(금액)),position = position_stack(vjust = 0.5), color = "black", size=3)+
  theme(axis.text.x = element_text(angle=0, vjust=0, color="black", size=12),
        plot.title = element_text(family="serif", face = "bold", hjust= 0.5, size=20))
#---------------------------------------------------------------------------
# tot_amt 범주화
summary(data_pos$tot_amt)
tot_amt_ctr <- data_pos$tot_amt %>% scale(center = TRUE)
summary(tot_amt_ctr)
psych::describe(tot_amt_ctr)

tot_amt_rng <- data_pos$tot_amt %>% scale(center = FALSE)
summary(tot_amt_rng)
psych::describe(tot_amt_rng)

tot_amt_snd <- data_pos$tot_amt %>% scale(center = TRUE, scale = TRUE)
summary(tot_amt_snd)
psych::describe(tot_amt_snd)


par(mfrow=c(1,2))
boxplot(data_pos$tot_amt)
boxplot(tot_amt_ctr)
boxplot(tot_amt_rng)
boxplot(tot_amt_snd)

hist(tot_amt_ctr, probability = TRUE)
lines(density(tot_amt_ctr))
hist(tot_amt_rng, probability = TRUE)
lines(density(tot_amt_rng))
hist(tot_amt_snd, probability = TRUE)
lines(density(tot_amt_snd))

library(tibble)
library(robustHD)
install.packages('robustHD')

tot_amt_win <- winsorize(data_pos$tot_amt, prob = 0.99)
summary(tot_amt_win)
summary(data_pos$tot_amt)

boxplot(tot_amt_win)
hist(tot_amt_win, probability = TRUE)
lines(density(tot_amt_win))
library(doBy)
#---------------------------------------------------------------------------
#---------------------------------------------------------------------------
from <- list(c(1:27770), c(27771:58000), c(58001:128000), c(128001:80000000))
to <- list(1,2,3,4)
data_pos$tot_amt_f <- recodeVar(data_pos$tot_amt, src = from, tgt = to)

# -----------------------------------------------------------------------------
# tot_amt / dc_rate / count
tmp <- table(data_pos$dc_rate_f, data_pos$tot_amt_f)
tmp_prop <- prop.table(tmp, 2)
tmp_prop <- round(tmp_prop, 4) *100

tmp_prop <- as.data.frame(tmp_prop)

names(tmp_prop) <- c('할인율', '가격대', '건수')
tmp_prop

ggplot(as.data.frame(tmp_prop), aes(x=가격대, y=건수, fill=할인율)) +
  ggtitle("할인율에 따른 가격대별 판매건수 비교")+
  geom_bar(stat="identity")+
  geom_text(aes(y=건수, label = paste(건수,"%")),position = position_stack(vjust = 0.5), color = "black", size=7)+
  theme(axis.text.x = element_text(angle=0,  vjust=0, color="black", size=12),
        plot.title = element_text(family="serif", face = "bold", hjust= 0.5, size=20))

tmp <- as.data.frame(tmp)
tmp
names(tmp) <- c('할인율', '가격대', '건수')
ggplot(as.data.frame(tmp), aes(x=가격대, y=건수, fill=할인율)) +
  ggtitle("할인율에 따른 일자별 판매건수 비교")+
  geom_bar(stat="identity")+
  geom_text(aes(y=건수, label = paste(건수)),position = position_stack(vjust = 0.5), color = "black", size=7)+
  theme(axis.text.x = element_text(angle=0,  vjust=0, color="black", size=12),
        plot.title = element_text(family="serif", face = "bold", hjust= 0.5, size=20))

tmp_prop <- tmp_prop[tmp_prop$할인율=='0%' ,]
tmp_prop <- tmp_prop[,-1]
tmp_prop
tmp_prop <- aggregate(건수 ~ 가격대 , data=tmp_prop, sum, drop=FALSE)

plot(건수 ~ 가격대, data=tmp_prop)
lines(lowess(tmp_prop$건수 ~ tmp_prop$가격대), col="black")
par(new=TRUE)

#-------------------------------------------------------------
# sales_date_day / dc_rate / tot_amt
tmp <- aggregate(tot_amt ~ tot_amt_f + dc_rate_f, data_pos, sum, drop=FALSE)
tmp[is.na(tmp)] <- 0
tmp

temp <- matrix(as.numeric(tmp$tot_amt), ncol=length(unique(tmp$tot_amt_f)),  byrow=TRUE)
temp
colnames(temp) <- c(1:4)
rownames(temp) <- levels(tmp$dc_rate_f)
temp
temp <- as.table(temp)
tmp_prop <- prop.table(temp, 2)
tmp_prop <- round(tmp_prop, 4)*100
tmp_prop <- as.data.frame(tmp_prop)
tmp_prop
names(tmp_prop) <- c('할인율', '가격대', '금액')

ggplot(as.data.frame(tmp_prop), aes(x=가격대, y=금액, fill=할인율)) +
  ggtitle("할인율에 따른 가격대별 판매금액 비교")+
  geom_bar(stat="identity")+
  geom_text(aes(y=금액, label = paste(금액,"%")),position = position_stack(vjust = 0.5), color = "black", size=7)+
  theme(axis.text.x = element_text(angle=0, vjust=0, color="black", size=12),
        plot.title = element_text(family="serif", face = "bold", hjust= 0.5, size=20))
tmp <- as.data.frame(tmp)
tmp
names(tmp) <- c('가격대', '할인율', '금액')

ggplot(as.data.frame(tmp), aes(x=가격대, y=금액, fill=할인율)) +
  ggtitle("할인율에 따른 일자별 판매금액 비교")+
  geom_bar(stat="identity")+
  geom_text(aes(y=금액, label = paste(금액)),position = position_stack(vjust = 0.5), color = "black", size=5)+
  theme(axis.text.x = element_text(angle=0, vjust=0, color="black", size=12),
        plot.title = element_text(family="serif", face = "bold", hjust= 0.5, size=20))

# -----------------------------------------------------------------------------
# tot_amt / inst_tot / count
tmp <- table(data_pos$inst_tot_f, data_pos$tot_amt_f)
tmp_prop <- prop.table(tmp, 2)
tmp_prop <- round(tmp_prop, 4) *100

tmp_prop <- as.data.frame(tmp_prop)

names(tmp_prop) <- c('할부요인', '가격대', '건수')
tmp_prop

ggplot(as.data.frame(tmp_prop), aes(x=가격대, y=건수, fill=할부요인)) +
  ggtitle("할부요인에 따른 가격대별 판매건수 비교")+
  geom_bar(stat="identity")+
  geom_text(aes(y=건수, label = paste(건수,"%")),position = position_stack(vjust = 0.5), color = "black", size=3)+
  theme(axis.text.x = element_text(angle=0,  vjust=0, color="black", size=12),
        plot.title = element_text(family="serif", face = "bold", hjust= 0.5, size=20))

tmp <- as.data.frame(tmp)
tmp
names(tmp) <- c('할부요인', '가격대', '건수')
ggplot(as.data.frame(tmp), aes(x=가격대, y=건수, fill=할부요인)) +
  ggtitle("할부요인에 따른 가격대별 판매건수 비교")+
  geom_bar(stat="identity")+
  geom_text(aes(y=건수, label = paste(건수)),position = position_stack(vjust = 0.5), color = "black", size=3)+
  theme(axis.text.x = element_text(angle=0,  vjust=0, color="black", size=12),
        plot.title = element_text(family="serif", face = "bold", hjust= 0.5, size=20))
#-------------------------------------------------------------
# sales_date_day / inst_tot / tot_amt
tmp <- aggregate(tot_amt ~ tot_amt_f + inst_tot_f, data_pos, sum, drop=FALSE)
tmp[is.na(tmp)] <- 0
tmp

temp <- matrix(as.numeric(tmp$tot_amt), ncol=length(unique(tmp$tot_amt_f)),  byrow=TRUE)
temp
colnames(temp) <- c(1:4)
rownames(temp) <- levels(tmp$inst_tot_f)
temp
temp <- as.table(temp)
tmp_prop <- prop.table(temp, 2)
tmp_prop <- round(tmp_prop, 4)*100
tmp_prop <- as.data.frame(tmp_prop)
tmp_prop
names(tmp_prop) <- c('할부요인', '가격대', '금액')

ggplot(as.data.frame(tmp_prop), aes(x=가격대, y=금액, fill=할부요인)) +
  ggtitle("할부요인에 따른 가격대별 판매금액 비교")+
  geom_bar(stat="identity")+
  geom_text(aes(y=금액, label = paste(금액,"%")),position = position_stack(vjust = 0.5), color = "black", size=3)+
  theme(axis.text.x = element_text(angle=0, vjust=0, color="black", size=12),
        plot.title = element_text(family="serif", face = "bold", hjust= 0.5, size=20))
tmp <- as.data.frame(tmp)
tmp
names(tmp) <- c('가격대', '할부요인', '금액')

ggplot(as.data.frame(tmp), aes(x=가격대, y=금액, fill=할부요인)) +
  ggtitle("할부요인에 따른 가격대별 판매금액 비교")+
  geom_bar(stat="identity")+
  geom_text(aes(y=금액, label = paste(금액)),position = position_stack(vjust = 0.5), color = "black", size=3)+
  theme(axis.text.x = element_text(angle=0, vjust=0, color="black", size=12),
        plot.title = element_text(family="serif", face = "bold", hjust= 0.5, size=20))




summary(data_pos$tot_amt[data_pos$import_flg==0])

str(data_inst)

tmp <- aggregate(tot_amt ~ tot_amt_f + inst_tot_f, data_pos, sum, drop=FALSE)
aggregate(tot_amt_win ~ team_nm,data_inst,sum,  drop=FALSE)
data_inst$tot_amt_win <- winsorize(data_inst$tot_amt, prob = 0.99)
summary(data_inst$tot_amt_win)
summary(data_inst$tot_amt)
tmp <- aggregate(tot_amt ~ custid, data_inst, sum, drop=FALSE)
tmp
summary(tmp$tot_amt)
tmp[tmp$tot_amt>3402056,]
unique(tmp$custid)

data_inst[data_inst$corner_nm=='인터넷 백화점',]
