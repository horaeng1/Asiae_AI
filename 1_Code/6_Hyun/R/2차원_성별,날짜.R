
sb<-read.csv(file='brand_data.csv',
             header = TRUE, sep = ',',
             stringsAsFactors = FALSE,
             strip.white = TRUE,
             na.strings = c(',','?','NA'))

library('psych')
library('Hmisc')
library(skimr)
install.packages("ggplot2")
library(ggplot2)



sb$gender_f <- factor(sb$gender,levels=c(0,1), labels=c('M','F'))
str(sb$gender_f)
sb$str_nm_f<-factor(sb$str_nm, levels=c('무역점','본점','신촌점','천호점'),
                    labels=c('무역점','본점','신촌점','천호점'))

sb$Month_Yr <- format(as.Date(sb$sales_date), "%m")
sb$sales_date_f <- factor(sb$Month_Yr, levels=c('01','02','03','04','05','06','07','08','09','10','11','12'), labels=c(1,2,3,4,5,6,7,8,9,10,11,12))



table(sb$gender_f,sb$sales_date_f)



gd_sal_freq <- table(sb$gender_f,sb$sales_date_f)
gd_sal_freq

sal_gd_freq <- table(sb$sales_date_f, sb$gender_f)
sal_gd_freq

gd_sal_prop <- prop.table(gd_sal_freq,2)
gd_sal_prop

sal_gd_prop <- prop.table(sal_gd_freq,2)
sal_gd_prop

gd_sal_result <- round(gd_sal_prop,3)
gd_sal_result

sal_gd_result <- round(sal_gd_prop,3)


par(mfrow=c(2,2))
barplot(gd_sal_result, main="성별에 따른 날짜 별 환불 비율 비교",
        xlab='월',ylab='환불빈도수',col=c('lightblue','pink'),legend=rownames(gd_sal_result))

barplot(gd_sal_freq, main="성별에 따른 날짜 별 환불 건수 비교",
        xlab='월',ylab='환불빈도수',col=c('lightblue','pink'),legend=rownames(gd_sal_freq))


#---------------------------------------------------

barplot(sal_gd_result, main="성별에 따른 날짜 별 환불 빈도수 비교",
        xlab='월',ylab='환불빈도수',col=c(rainbow(12)),legend=rownames(sal_gd_result))


barplot(sal_gd_freq, main="성별에 따른 날짜 환불 빈도수 비교",
        xlab='월',ylab='환불빈도수',beside=TRUE, col=c(rainbow(12)),legend=rownames(sal_gd_freq))


