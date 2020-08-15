data <- read.csv('C:/Users/ASIAE_22/Downloads/X_train.csv')
class(data)
str(data)
colnames(data)
summary(data)

library(plyr)
library(dplyr)
library(MASS)
library(ggplot2)
library(corrplot)

main_data <- read.csv("C:/Users/ASIAE_22/Downloads/X_train.csv",
                      header = TRUE, sep = ',',
                      stringsAsFactors = FALSE,
                      strip.white = TRUE,
                      na.strings = c(',', '?', "NA")
)

data_main <- 

ctg_names <- c('str_nm', 'corner_nm', 'part_nm', 'buyer_nm', 'team_nm')
ctg <- main_data[ctg_names]
ctg
ctg_main <- subset(ctg, str_nm == "본점")
ctg_muyuk <- subset(ctg, str_nm == "무역점")
ctg_shinchon <- subset(ctg, str_nm == "신촌점")
ctg_chunho <- subset(ctg, str_nm == "천호점")

ctg_main # 본점: 299735, 무역점: 284226, 무역점: 249603, 천호점: 203089

ctg_team_food <- subset(ctg, team_nm == "식품팀")
ctg_team_fashion <- subset(ctg, team_nm == "의류패션팀")
ctg_team_ect <- subset(ctg, team_nm == "잡화가용팀")


team_table <- table(ctg$team_nm, ctg$str_nm)
part_table <- table(ctg$part_nm, ctg$str_nm)

team_table <- prop.table(team_table, 2)
part_table <- prop.table(part_table, 2)
part_df<-as.data.frame(part_table)

par(mfrow=c(1,2))
barplot(team_table,
        main = "지점별 팀별 총 매출",
        xlab = "지점별",
        ylab = "구매갯수",
        las = 1,
        col = mycol,
        legend = names(ctg$team_nm))

barplot(part_table,
        main = "지점별 파트 총 매출",
        xlab = "지점별",
        ylab = "구매갯수",
        las = 1,
        col = mycol,
        legend = names(ctg$part_nm))

d[data$tot_amt,]
install.packages('RColarBrewer')
library(RColorBrewer)

mycol = brewer.pal(12, "Set3")
mycol









data
str(c_data)
k <- as.POSIXlt(c_data$sales_date) 
dc_rate <- round(c_data$dis_amt/c_data$tot_amt,2)*100
stime <- c_data$sales_time%/%100
sale <- c_data$net_amt
k$year
k$mon      # 월 (0 = January)
k$mday     # 일
k$wday     # 요일
stime     # 시간 
max(k)

kk <- data.frame(year=k$year+1900, mon=k$mon+1, day=k$mday, hour=stime,
                 wday=k$wday, sale=sale, dc_rate=dc_rate)

allk$wday

allk <- as.POSIXlt(d_data$sales_date) 
alldc_rate <- round(d_data$dis_amt/d_data$tot_amt,2)*100
allstime <- d_data$sales_time%/%100
allsale <- d_data$net_amt

kkk <- data.frame(year=allk$year+1900, mon=allk$mon+1, day=allk$mday, hour=allstime,
                  wday=allk$wday, sale=allsale, dc_rate=alldc_rate)

summary(kkk)
class(kk)
str(kk)

install.packages("psych")
install.packages("Hmisc")
install.packages("skimr")
install.packages("PerformanceAnalytics")
install.packages("corrplot")

library(psych)
library(Hmisc)
library(skimr)
library(PerformanceAnalytics)
