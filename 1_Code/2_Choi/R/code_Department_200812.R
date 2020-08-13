install.packages("corrplot")
library(plyr)
library(dplyr)
library(MASS)
library(ggplot2)
library(corrplot)

main_data <- read.csv("X_train.csv",
                   header = TRUE, sep = ',',
                   stringsAsFactors = FALSE,
                   strip.white = TRUE,
                   na.strings = c(',', '?', "NA")
)

#sales_time 다항

sales_time <- as.factor(main_data$sales_time)
sales_time <- sales %/% 100
sales_ctg_names <- factor(sales_time, levels = c(1, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23),
                          labels = c("기타", '기타', '기타', '10시', '11시', '12시', '13시',
                                     '14시', '15시', '16시', '17시', '18시', '19시', '20시', '21시',
                                     '기타', '기타'))
sales_ctg_names_df <- as.data.frame(sales_ctg_names)
View(sales_ctg_names_df)

#str_nm 서열
main_data$str_nm
str_nm <- as.factor(main_data$str_nm)
head(str_nm)

#goodcd 다항
main_data$goodcd
goodcd <- as.factor(main_data$goodcd)
class(goodcd)
goodcd_df <- as.data.frame(goodcd)
goodcd_df
#inst_mon 다항
inst_month_f <- factor(main_data$inst_mon, levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11 ,12),
                       labels = c('일시불', '2개월 할부', '3개월 할부', '4개월 할부', '5개월 할부', '6개월 할부', '7개월 할부'
                                  , '8개월 할부', '9개월 할부', '10개월 할부', '11개월 할부', '12개월 할부'))
inst_month_df <- as.data.frame(inst_month_f)
View(inst_month_df)
#inst_fee 이항
inst_fee_f <- factor(main_data$inst_fee, levels = c(0, 1),
                       labels = c('무이자할부', '이자할부'))
inst_fee_df <- as.data.frame(inst_fee_f)
View(inst_fee_df)
