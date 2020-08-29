# tidyr 패키지 설치
install.packages('tidyr')
library('tidyr')

# dataframe에 csv파일들 넣기
train1 <- read.csv('Y_train.csv', sep = ',', header = T, stringsAsFactors = FALSE)
train2 <- read.csv('X_train.csv', sep = ',', header = T, stringsAsFactors = FALSE)

# join을 위한 패키지 설치
install.packages('dplyr')
library(dplyr)

# custid 기준으로 Y_train + X_train full_join하기
sb <- full_join(train1,train2,by = 'custid')

# 'part_nm'컬럼 값 오타수정 및 통일화
sb$part_nm <- recode (sb$part_nm,
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


# 환불 한 남녀의 비율
sb$gender_f<-factor(sb$gender,levels = c(0,1),
                    labels = c('M','F'))

# 1:가정용품, 2:공산품, 3:남성의류, 4:명품잡화, 
# 5:생식품, 6:스포츠캐주얼, 7:아동, 8:여성의류, 
# 9:영플라자, 10:캐주얼, 11:패션잡화

sb$part_nm_num[sb$part_nm=='가정용품']<-1
sb$part_nm_num[sb$part_nm=='공산품']<-2
sb$part_nm_num[sb$part_nm=='남성의류']<-3
sb$part_nm_num[sb$part_nm=='명품잡화']<-4
sb$part_nm_num[sb$part_nm=='생식품']<-5
sb$part_nm_num[sb$part_nm=='스포츠캐주얼']<-6
sb$part_nm_num[sb$part_nm=='아동']<-7
sb$part_nm_num[sb$part_nm=='여성의류']<-8
sb$part_nm_num[sb$part_nm=='영플라자']<-9
sb$part_nm_num[sb$part_nm=='캐주얼']<-10
sb$part_nm_num[sb$part_nm=='패션잡화']<-11
sb$part_nm_num[sb$part_nm=='상품개발영업1과']<-12
sb$part_nm_num[sb$part_nm=='인터넷백화점']<-13

part_nm_num<-unique(as.numeric(sb$part_nm_num))
str(part_nm_num)

# part_nm 변수 팩터형으로 변환
sb$part_nm_f<-factor(sb$part_nm_num,levels = c(seq(13)),
                     labels = c('가정용품','공산품','남성의류','명품잡화', '생식품','스포츠캐주얼','아동','여성의류','영플라자', '캐주얼', '패션잡화','상품개발영업1과','인터넷백화점'))

View(sb)
# 1: 무역점, 2: 본점, 3: 천호점, 4: 신촌점


sb$str_nm_num[sb$str_nm=='무역점']<-1
sb$str_nm_num[sb$str_nm=='본점']<-2
sb$str_nm_num[sb$str_nm=='천호점']<-3
sb$str_nm_num[sb$str_nm=='신촌점']<-4
str(sb$str_nm_num)

str_nm_num<-unique(as.numeric(sb$str_nm_num))

sb$str_nm_f<-factor(sb$str_nm_num,levels = c(str_nm_num),
                        labels = c('무역점','본점','천호점','신촌점'))

# month_f : sale_date -> "%Y-%m" 형식으로 표현
sb$sales_date_num <- format(as.Date(sb$sales_date), "%Y%m")
date<-as.numeric(sort(unique(sb$sales_date_num)))
str(date)
View(date)

# 1: 200005, 2: 200006, 3: 200007
# 4: 200008, 5: 200009. 6: 200010
# 7: 200011. 8: 200012, 9: 200101
# 10: 200102, 11: 200103, 12: 200104

sb$sales_date_f<-factor(sb$sales_date_num,levels = c(date),
                           labels = c('200005','200006','200007','200008', '200009','200010',
                                      '200011','200012','200101', '200102', '200103','200104'))


sb$refund<-ifelse(sb$tot_amt < 0, 1, 0)

View(refund)
View(sb)

# 성별에 따른 part_nm 간 교차빈도분석
table(refund$gender,refund$part_nm_num)
table(refund$gender_f,refund$part_nm_f)
table(refund$gender_f,refund$part_nm_f,useNA = 'ifany')

gd_pn_freq<-table(refund$gender_f,refund$part_nm_f)
gd_pn_freq

# part_nm에 따른 성별간 교차빈도 분석
table(refund$part_nm_num,refund$gender)
table(refund$part_nm_f,refund$gender_f)
table(refund$part_nm_f,refund$gender_f,useNA = 'ifany')

pn_gd_freq<-table(refund$part_nm_f,refund$gender_f)
pn_gd_freq

# 교차빈도분석을 비율분석으로 변환
prop.table(gd_pn_freq,1)
prop.table(gd_pn_freq,2)


gd_pn_prop<-prop.table(gd_pn_freq,2)
gd_pn_prop

addmargins(round(gd_pn_prop,3),2)

gd_pn_result<-round(gd_pn_prop,3)*100
gd_pn_result

addmargins(gd_pn_result,2)

# 성별에 따른 환불시기 간 교차빈도분석
table(refund$gender,refund$sales_date_f)
table(refund$gender_f,refund$sales_date_f)
table(refund$gender_f,refund$sales_date_f,useNA = 'ifany')



gd_sd_freq<-table(refund$gender_f,refund$sales_date_f)
gd_sd_freq

#월에 따른 환불 빈도수 교차비교분석
table(sb$refund,sb$sales_date_f)
rf_sb_freq <- table(sb$refund,sb$sales_date_f)
rf_sb_freq
sb_rf_freq <- table(sb$sales_date_f,sb$refund)
sb_rf_freq

# 교차빈도분석을 비율분석으로 변환
prop.table(gd_sd_freq,1)
prop.table(gd_sd_freq,2)

gd_sd_prop<-prop.table(gd_sd_freq,2)
gd_sd_prop

addmargins(round(gd_sd_prop,3),2)

gd_sd_result<-round(gd_sd_prop,3)*100
gd_sd_result

addmargins(gd_sd_result,2)
View(refund)

# 성별에 따른 구매점포 간 교차빈도분석
table(refund$gender,refund$str_nm_f)
table(refund$gender_f,refund$str_nm_f)
table(refund$gender_f,refund$str_nm_f,useNA = 'ifany')

gd_sn_freq<-table(refund$gender_f,refund$str_nm_f)
gd_sn_freq

# 교차빈도분석을 비율분석으로 변환
prop.table(gd_sn_freq,1)
prop.table(gd_sn_freq,2)

gd_sn_prop<-prop.table(gd_sn_freq,2)
gd_sn_prop

addmargins(round(gd_sn_prop,3),2)

gd_sn_result<-round(gd_sn_prop,3)*100
gd_sn_result

addmargins(gd_sn_result,2)

# 환불점포에 따른 환불시기 간 교차빈도분석
table(refund$sales_date_f,refund$str_nm_f)
table(refund$sales_date_f,refund$str_nm_f)
table(refund$sales_date_f,refund$str_nm_f,useNA = 'ifany')

sd_sn_freq<-table(refund$sales_date_f,refund$str_nm_f)
sd_sn_freq

# 교차빈도분석을 비율분석으로 변환
prop.table(sd_sn_freq,1)
prop.table(sd_sn_freq,2)

sd_sn_prop<-prop.table(sd_sn_freq,2)
sd_sn_prop

addmargins(round(sd_sn_prop,3),2)

sd_sn_result<-round(sd_sn_prop,3)*100
sd_sn_result

addmargins(sd_sn_result,2)

View(refund)
# 환불점포에 따른 환불품목 간 교차빈도분석
table(refund$part_nm_f,refund$str_nm_f)
table(refund$part_nm_f,refund$str_nm_f)
table(refund$part_nm_f,refund$str_nm_f,useNA = 'ifany')

pn_sn_freq<-table(refund$part_nm_f,refund$str_nm_f)
pn_sn_freq

# 교차빈도분석을 비율분석으로 변환

prop.table(pn_sn_freq,1)
prop.table(pn_sn_freq,2)

pn_sn_prop<-prop.table(pn_sn_freq,2)
pn_sn_prop

addmargins(round(pn_sn_prop,3),2)

pn_sn_result<-round(pn_sn_prop,3)*100
pn_sn_result

addmargins(pn_sn_result,2)

# 환불시기에 따른 환불품목 간 교차빈도분석
table(refund$part_nm,refund$sales_date_f)
table(refund$part_nm_f,refund$sales_date_f)
table(refund$part_nm_f,refund$sales_date_f,useNA = 'ifany')

pn_sd_freq<-table(refund$part_nm_f,refund$sales_date_f)
pn_sd_freq

# 교차빈도분석을 비율분석으로 변환
prop.table(pn_sd_freq,1)
prop.table(pn_sd_freq,2)

pn_sd_prop<-prop.table(pn_sd_freq,2)
pn_sd_prop

addmargins(round(pn_sd_prop,3),2)

pn_sd_result<-round(pn_sd_prop,3)*100
pn_sd_result

addmargins(pn_sd_result,2)



par(mfrow=c(2,2))


barplot(gd_pn_prop,
        main="백화점 환불 품목에 따른 성별 분포 비교",
        xlab="환불품목", ylab="성별",
        col=c("lightblue","pink"),legend=rownames(gd_pn_prop))

barplot(gd_sd_prop,
        main="백화점 환불 시기에 따른 성별 분포 비교",
        xlab="환불시기", ylab="성별",
        col=c("lightblue","pink"),legend=rownames(gd_sd_prop))

barplot(gd_sn_prop,
        main="백화점 환불 점포에 따른 성별 분포 비교",
        xlab="환불점포", ylab="성별",
        col=c("lightblue","pink"),legend=rownames(gd_sn_prop))


barplot(sd_sn_prop,
        main="백화점 환불 점포에 따른 환불 시기 분포 비교",
        xlab="환불점포", ylab="환불시기",
        col=c(rainbow(12)),legend=rownames(sd_sn_prop))

barplot(pn_sn_prop,
        main="백화점 환불 지점에 따른 환불 품목 분포 비교",
        xlab="환불점포", ylab="환불품목",
        col=c(rainbow(11)),legend=rownames(pn_sn_prop))

barplot(pn_sd_prop,
        main="백화점 환불 시기에 따른 환불 분포 비교",
        xlab="환불시기", ylab="환불품목",
        col=c(rainbow(12)),legend=rownames(pn_sd_prop))


barplot(sb_rf_freq[,2],
        main="날짜에 따른 환불 분포 비교",
        xlab="환불시기", ylab="환불품목",
        col=c(rainbow(12)))
bar.text <- barplot(bar)
text(bar.text, bar+20, labels=bar)
par(mfrow=c(1,1))


install.packages("ggplot2")
library(ggplot2)

p1 <- ggplot(sb,aes(sales_date_f,refund,color=str_nm_f, shape=str_nm_f)+
        geom_boxplot()+labs(title="월별,지점별, 남녀별 세분화 비교분석")
p1


