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
# 할인율에 따른 구매금액 분포 월별 그래프
# sales_date_month / tot_amt / dc_rate
par(mfrow=c(1,1))
plot(tot_amt ~ sales_date_month_f, data = data_main, pch=19)
str(data_main$sales_date_month_f)
data_main$sales_date_month_f <- factor(data_main$sales_date_month_f , levels = c("2000-05", "2000-06", "2000-07", "2000-08", "2000-09", "2000-10", "2000-11", "2000-12", "2001-01", "2001-02", "2001-03","2001-04"), labels= c(0:11))
data_main$sales_date_month_f <- factor(data_main$sales_date_month_f , levels = c(0:11), labels= c("2000-05", "2000-06", "2000-07", "2000-08", "2000-09", "2000-10", "2000-11", "2000-12", "2001-01", "2001-02", "2001-03","2001-04"))

table(data_main$sales_date_month_f, data_main$tot_amt)
