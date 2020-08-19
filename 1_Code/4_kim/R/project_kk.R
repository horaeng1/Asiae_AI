data <- read.csv('C:/Users/ASIAE_20/Desktop/근영/실습DB/X_train.csv')
class(data)
str(data)
colnames(data)
summary(data)

###환불거래 제거
data_pos <- data[data$tot_amt>=0,]

# 할인율 추가
data_pos$dc_rate <- round(data_pos$dis_amt/data_pos$tot_amt,2)*100
unique(data_pos$dc_rate)
data_pos$dc_rate

#할인율 스케일링
library(doBy)
table(data_pos$dc_rate)

from <- list(0, c(1:5), c(6:65))
to <- list(0, 5, 10)

dc_rate <- data_pos$dc_rate
dc_rate <- recodeVar(dc_rate , from , to)
table(dc_rate)

dc_rate_f <- factor(dc_rate, levels = c(0,5,10), labels = c('0%','5%','10%'))
dc_rate_f
data_pos$dc_rate_f <- dc_rate_f
data_pos$dc_rate_f
table(dc_rate_f)


################판매시간###################

data_pos$hhm_time <- (data_pos$sales_time%/%100)*10+((data_pos$sales_time%%100)%/%30)
data_pos$hhm_time
tmp <- unique(data_pos$hhm_time)
tmp <- sort(tmp)
tmp
length(tmp)

from <- list(c(81,90,91,100),101,110,111,120,121,130,131,140,141,150,151, 
             160,161,170,171,180,181,190,191,200,201,210,211,c(220,231))
to <- list(0,101,110,111,120,121,130,131,140,141,150,151, 
           160,161,170,171,180,181,190,191,200,201,210,211,240)

tmp <- recodeVar(data_pos$hhm_time, src=from, tgt=to)
data_pos$hhm_time <- tmp
table(data_pos$hhm_time)

#판매시간 스케일링 & factor변환(라벨링)

timename <- c('영업시간이전','10:30','11:00','11:30','12:00','12:30','13:00','13:30','14:00','14:30','15:00','15:30','16:00','16:30','17:00','17:30','18:00',
              '18:30','19:00','19:30','20:00','20:30','21:00','21:30','영업시간이후')
sort(unique(data_pos$hhm_time))

data_pos$hhm_time_f <- factor(data_pos$hhm_time, levels=sort(unique(data_pos$hhm_time)), labels = timename)
str(data_pos)

#-------------------------------------------------------------
# hhm_time / dc_rate / count

#tmp <- table(data_pos$dc_rate, data_pos$str_nm_f)
tmp <- table(data_pos$dc_rate_f, data_pos$hhm_time_f)
tmp
tmp_prop <- prop.table(tmp, 2)
tmp_prop <- round(tmp_prop, 4) *100
tmp_prop
par(mfrow=c(2,1))
barplot(tmp, main =  "할인율에 따른 판매시간별 판매건수 비교", #horiz=TRUE,
        xlab = "판매시간", ylab = '건수', las = 2,
        col = c("lightblue", "pink", "yellow"))
legend("topright", legend = c('0%','5%','10%'), fill =c("lightblue", "pink", "yellow") , cex = 0.3)

barplot(tmp_prop, main =  "할인율에 따른 판매시간별 판매건수 비율 비교", #horiz=TRUE,
        xlab = "판매시간", ylab = '건수', las = 2,
        col = c("lightblue", "pink", "yellow"))
legend("topright", legend = c('0%','5%','10%'), fill =c("lightblue", "pink", "yellow") , cex = 0.3)

#-------------------------------------------------------------
# hhm_time / dc_rate / tot_amt
tmp <- aggregate(tot_amt ~ hhm_time_f+ dc_rate_f, data_pos, sum, drop=FALSE)
tmp[is.na(tmp)] <- 0
tmp

temp <- matrix(as.numeric(tmp$tot_amt), ncol=length(unique(tmp$hhm_time_f)),  byrow=TRUE)
colnames(temp) <- levels(tmp$hhm_time_f)
rownames(temp) <- levels(tmp$dc_rate_f)
temp <- as.table(temp)
temp
temp_prop <- prop.table(temp, 2)
######################
barplot(temp, main =  "할인율에 따른 판매시간별 판매금액 비교", #horiz=TRUE,
        xlab = "금액", ylab = '판매시간', las = 2,
        col = c("lightblue", "pink", "yellow"))
legend("topright", legend = c('0%','5%','10%'), fill =c("lightblue", "pink", "yellow") , cex = 0.3)

barplot(temp_prop, main =  "할인율에 따른 판매시간별 판매금액 비교", #horiz=TRUE,
        xlab = "금액", ylab = '판매시간', las = 2,
        col = c("lightblue", "pink", "yellow"))
legend("topright", legend = c('0%','5%','10%'), fill =c("lightblue", "pink", "yellow") , cex = 0.3)

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

#-----------------------------------------------------------
#--------------그래프안에 글자 넣기--------------------------------------------------------------------------------------------------
#---------------ggplot으로 그래프 그리기--------------------
#-----------------------------------------------------------
library(dplyr)
# hhm_time / dc_rate / count
str(data_pos)
tmp <- table(data_pos$dc_rate_f, data_pos$hhm_time_f)
tmp_prop <- prop.table(tmp, 2)
tmp_prop <- round(tmp_prop, 4) *100
#tmp <- as.data_pos.frame(tmp)
#names(tmp) <- c('할인율', '카테고리', '건수')
tmp_prop <- as.data_pos.frame(tmp_prop)
#tmp_prop$test <- c(1:105)
#tmp_prop$test[tmp_prop$test==1] <- NA
names(tmp_prop) <- c('할인율', '판매시간', '건수')
tmp_prop

ggplot(as.data_pos.frame(tmp_prop), aes(x=판매시간, y=건수, fill=할인율)) +
  ggtitle("할인율에 따른 판매시간별 판매건수 비교")+
  geom_bar(stat="identity")+
  geom_text(aes(y=건수, label = paste(건수,"%")),position = position_stack(vjust = 0.5), color = "black", size=2)+
  theme(axis.text.x = element_text(angle=90, hjust = 1, vjust=0, color="black", size=12),
        plot.title = element_text(family="serif", face = "bold", hjust= 0.5, size=20))

#-------------------------------------------------------------
# hhm_time / dc_rate / tot_amt
tmp <- aggregate(tot_amt ~ hhm_time + dc_rate_f, data_pos, sum, drop=FALSE)
tmp[is.na(tmp)] <- 0
tmp

temp <- matrix(as.numeric(tmp$tot_amt), ncol=length(unique(tmp$hhm_time)),  byrow=TRUE)
colnames(temp) <- levels(tmp$hhm_time)
rownames(temp) <- levels(tmp$dc_rate_f)
temp <- as.table(temp)
tmp_prop <- prop.table(temp, 2)
tmp_prop <- round(tmp_prop, 4)*100
tmp_prop <- as.data_pos.frame(tmp_prop)
tmp_prop
names(tmp_prop) <- c('할인율', '판매시간', '금액')

ggplot(as.data_pos.frame(tmp_prop), aes(x=판매시간, y=금액, fill=할인율)) +
  ggtitle("할인율에 따른 판매시간별 판매금액 비교")+
  geom_bar(stat="identity")+
  geom_text(aes(y=금액, label = paste(금액,"%")),position = position_stack(vjust = 0.5), color = "black", size=3)+
  theme(axis.text.x = element_text(angle=90, hjust = 1, vjust=0, color="black", size=12),
        plot.title = element_text(family="serif", face = "bold", hjust= 0.5, size=20))

#-------------------------------------------------------------