install.packages('tidyr')
library('tidyr')
install.packages('dplyr')
library(dplyr)
sb <-read.csv(file = 'XY_train.csv',na.strings = 'NA')
sb$part_nm <- recode(sb$part_nm,
                      '케주얼,구두,아동' = '캐주얼',
                      '골프/유니캐쥬얼' = '스포츠캐주얼',
                      '스포츠캐쥬얼' = '스포츠캐주얼', 
                      '영라이브' = '영플라자',
                      '영어덜트캐쥬얼' = '영플라자',
                      '영캐릭터' = '영플라자',
                      '남성정장스포츠' = '남성의류',
                      '여성캐주얼' = '여성의류',
                      '여성정장' = '여성의류',
                      '여성캐쥬얼' = '여성의류',
                      '여성의류파트' = '여성의류',
                      '아동,스포츠' = '아동',
                      '아동문화' = '아동',
                      '로얄부틱' = '명품잡화',
                      '로얄부띠끄' = '명품잡화',
                      '잡화파트' = '패션잡화',
                      '잡화' = '패션잡화',
                      '생식품파트' = '생식품',
                      '가정용품파트' = '가정용품',
                      '공산품파트' = '공산품')   


sb$refund<-ifelse(sb$tot_amt < 0, 1, 0)

summary(sb)

#
install.packages("dplyr")
library(dplyr)

View(sb)
sb$gender_f <- factor(sb$gender, levels=c(0,1), labels = c('m','f'))

table(sb$part_nm,sb$gender_f)
table(sb$part_nm,sb$refund)

pt_rf_freq <- table(sb$part_nm,sb$refund)
pt_rf_freq[,2]

#지점 별
st_rf_freq <- table(sb$str_nm,sb$refund)
st_rf_freq

pt_rf_prop <- prop.table(pt_rf_freq,1)
pt_rf_prop
pt_rf_result <- round(pt_rf_prop,3)
pt_rf_result

#월 별
sb$Month_Yr <- format(as.Date(sb$sales_date), "%m")
sb$sales_date_f <- factor(sb$Month_Yr, levels=c('01','02','03','04','05','06','07','08','09','10','11','12'), labels=c(1,2,3,4,5,6,7,8,9,10,11,12))


pt_gd_freq <- table(sb$part_nm,sb$gender_f)
dt_rf_freq <- table(sb$sales_date_f,sb$refund)
dt_rf_freq

pt_gd_prop<- prop.table(pt_gd_freq,1)
gd_pt_prop <- prop.table(gd_pt_freq,2)

pt_gd_result <- round(pt_dg_prop,3)
gd_pt_result <- round(gd_pt_prop,3)
pt_gd_result

par(mfrow=c(1,1))
barplot(gd_rf_freq[,2], main="남녀 별 환불빈도수 비교",
        xlab='성별',ylab='환불빈도수',col=c('blue','red'))


barplot(pt_rf_freq
        
        
        
        
        , main="성별에 따른 파트 별 환불 빈도수 비교",
        xlab='파트',ylab='환불빈도수',beside=TRUE, col=c('lightblue','pink'),legend=rownames(pt_rf_freq))




