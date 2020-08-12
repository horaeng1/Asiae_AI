main_data <- read.csv(file='X_train.csv',
                      header = TRUE, sep=',',
                      stringsAsFactors = FALSE,
                      strip.white = TRUE,
                      na.strings = c(',','?','NA'))

#범주형 변수컬럼명 파악
ctg_names <-c('custid','sales_date','sales_time','str_nm','goodcd','brd_nm',
              'corner_nm','pc_nm','part_nm','team_nm','buyer_nm','import_flg')

#범주형 변수컬럼 데이터셋 추출
ctg<-data[ctg_names]

#범주형 데이터셋 간단조회
head(ctg)

#내부구조 조회
str(ctg)

#기본 기술통계량 파악
summary(ctg)

#지점 간단조회
str(ctg$str_nm)

#지점 변수 팩터형으로 변환
ctg$str_nm_f <- factor(ctg$str_nm, levels = c('본점','무역점','신촌점','천호점'),
                       labels = c('본점','무역점','신촌점','천호점'))

#팩터형 뷰로 확인
View(ctg$str_nm_f)

#지점변수 간단기술통계
summary(ctg$str_nm_f)

#수입상품 변수 간단조회
str(ctg$import_flg)

#수입상품 팩터형으로 변환 0=국,1=수입
ctg$import_flg_f <- factor(ctg$import_flg,
                           levels = c(0,1),
                           labels = c(0,1))

#수입 상품 변수 간단기술통계
summary(ctg$import_flg_f)

#지점에 따른 수입상품 간 차이 교차빈도분석
(table(ctg$str_nm, ctg$import_flg))
(table(ctg$str_nm_f,ctg$import_flg_f))
(table(ctg$str_nm_f,ctg$import_flg_f, useNA = 'ifany'))

#교차분석내용 객체저장
str_import_freq <- table(ctg$str_nm_f,ctg$import_flg_f)
str_import_freq

#수입상품에 따른 지점 간 차이 교차빈도분석
(table(ctg$import_flg,ctg$str_nm ))
(table(ctg$import_flg_f,ctg$str_nm_f ))
(table(ctg$import_flg_f,ctg$str_nm_f, useNA = 'ifany' ))

#교차분석내용 객체저장
import_str_freq <- table(ctg$import_flg_f,ctg$str_nm_f )
import_str_freq

#교차빈도분석 부분합 계산
addmargins(str_import_freq)
addmargins(str_import_freq,1)
addmargins(str_import_freq,2)

str_import_freq_sum <- addmargins(str_import_freq,2)
str_import_freq_sum

#교차빈도분석을 비율분석으로 변환
(prop.table(str_import_freq,1))  # 각 지점에서 수입분포비율비교
(prop.table(str_import_freq,2))  # 각 수입에서 지점분포비율비교

(str_import_prop <- prop.table(str_import_freq,1))

addmargins(round(str_import_prop,3),2)

#교차비율분석을 백분율분석으로 변환
(str_import_result <- round(str_import_prop,3)*100)

(addmargins(str_import_result,2))

#그래프
par(mfrow=c(2,2))

barplot(str_import_freq,
        main='지점별 수입상품 분포비교',
        xlab = '수입상품', ylab= '지점',
        col = rainbow(4), legend=rownames(str_import_freq))

barplot(str_import_freq,
        main='지점별 수입상품 분포비교',
        xlab = '수입상품', ylab= '지점',
        col = rainbow(4), legend=rownames(str_import_freq),beside=TRUE)

barplot(import_str_freq,
        main='지점별 수입상품 분포비교',
        xlab = '지점', ylab= '수입상품',
        col = c("lightblue",'pink'), legend=rownames(str_import_freq))

barplot(import_str_freq,
        main='지점별 수입상품 분포비교',
        xlab = '수입상품', ylab= '지점', beside = TRUE,
        col = c("lightblue",'pink'), legend=rownames(str_import_freq))

par(mfrow=c(2,2))

plot(import_flg_f ~str_nm_f, data= ctg,
     main='지점별 수입상품 분포비교',
     xlab = '지점', ylab= '수입상품',
     col= rainbow(length(unique(ctg$import_flg_f))))

plot(str_nm_f ~import_flg_f, data= ctg,
     main='지점별 수입상품 분포비교',
     xlab = '수입상품', ylab= '지점',
     col= rainbow(length(unique(ctg$str_nm_f))))

mosaicplot(import_flg_f ~str_nm_f, data= ctg,
           main='지점별 수입상품 분포비교',
           xlab = '수입상품', ylab= '지점',
           col= rainbow(length(unique(ctg$str_nm_f))))

mosaicplot(str_nm_f ~import_flg_f, data= ctg,
           main='지점별 수입상품 분포비교',
           xlab = '지점', ylab= '수입상품',
           col= rainbow(length(unique(ctg$import_flg_f))))


str(ctg)
## custid 팩터형
unique(ctg$custid)
length(unique(ctg$custid))
ctg$custid_f <- factor(ctg$custid, levels = unique(ctg$custid),labels = c(0:29999))
ctg$custid_f <- factor(ctg$custid_f, levels = c(0:29999),labels = unique(ctg$custid))
str(ctg$custid)

##세일즈데이트 팩터형
unique(ctg$sales_date)
length(unique(ctg$sales_date))
ctg$sales_date_f <- factor(ctg$sales_date, levels = unique(ctg$sales_date),labels = c(0:340))
ctg$sales_date_f <- factor(ctg$sales_date_f, levels = c(0:340),labels = unique(ctg$sales_date))
str(ctg$sales_date_f)
View(ctg$sales_date_f)

## goodcd 팩터형
unique(ctg$goodcd)
length(unique(ctg$goodcd))
ctg$goodcd_f <- factor(ctg$goodcd, levels = unique(ctg$goodcd),labels = c(0:10426))
ctg$goodcd_f <- factor(ctg$goodcd_f, levels = c(0:10426),labels = unique(ctg$goodcd))
str(ctg$goodcd_f)


##ㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡ

head(ctg$brd_nm,10)
tail(ctg$brd_nm,15)
ctg$brd_nm[10:20]
summary(ctg)
length(ctg$brd_nm)
NROW(ctg$brd_nm)
summary(ctg$brd_nm)

##브랜드
unique(ctg$brd_nm)
length(unique(ctg$brd_nm))
ctg$brd_nm_f <- factor(ctg$brd_nm, levels= unique(ctg$brd_nm),labels = c(0:1881))
ctg$brd_nm_f <- factor(ctg$brd_nm_f, levels= c(0:1881),labels = unique(ctg$brd_nm))
ctg$brd_nm_f
head(ctg)
str(ctg)

##브랜드 빈도
brd_nm_freq <- table(ctg$brd_nm)
brd_nm_freq
brd_nm_f_freq<-table(ctg$brd_nm_f)
brd_nm_f_freq

##브랜드 빈도분석 결과를 데이터프레임형태
brd_nm_df <- as.data.frame(brd_nm_f_freq)
brd_nm_df
View(brd_nm_df)

##브랜드 비율분석
brd_nm_prop <- prop.table(brd_nm_freq)
brd_nm_prop
brd_nm_f_prop <- prop.table(brd_nm_f_freq)
brd_nm_f_prop

##브랜드 비율분석 결과를 데이터 프레임형태
brd_nm_prop_df <- as.data.frame(brd_nm_f_prop)
brd_nm_prop_df
View(brd_nm_prop_df)

##브랜드 비율분석 소수자리정리
round(brd_nm_prop,3)
round(brd_nm_f_prop,3)

##브랜드 백분율분석 
brd_nm_pect <- round(brd_nm_prop,3)*100
brd_nm_pect
brd_nm_f_pect <- round(brd_nm_f_prop,3)*100
brd_nm_f_pect

##브랜드 백분율분석 결과를 데이터 프레임형태
brd_nm_pect_df <- as.data.frame(brd_nm_f_pect)
brd_nm_pect_df
View(brd_nm_pect_df)

##브랜드 그래프
par(mfrow=c(2,2))

barplot(brd_nm_f_freq,
        main="브랜드 분포비교",
        xlab='브랜드',ylab='거래횟수',las=2)

barplot(brd_nm_f_freq,
        main="브랜드 분포비교",
        xlab='브랜드',ylab='거래횟수',las=2, horiz=TRUE)

barplot(brd_nm_f_prop,
        main="브랜드 비율 분포비교",
        xlab='브랜드',ylab='거래횟수',las=2,density=c(20,30))


barplot(brd_nm_f_prop,
        main="브랜드 비율 분포비교",
        xlab='브랜드',ylab='거래횟수',las=2, horiz=TRUE,
        density=c(20,30),col=c("lightblue","pink"),
        beside=TRUE)

##ㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡ


##코너
unique(ctg$corner_nm)
length(unique(ctg$corner_nm))
ctg$corner_nm_f <- factor(ctg$corner_nm, levels= unique(ctg$corner_nm),labels = c(0:308))
ctg$corner_nm_f <- factor(ctg$corner_nm_f, levels= c(0:308),labels = unique(ctg$corner_nm))
str(ctg$brd_nm_f)

##코너 빈도
corner_nm_freq <- table(ctg$corner_nm)
corner_nm_freq
corner_nm_f_freq<-table(ctg$corner_nm_f)
corner_nm_f_freq

##코너 빈도분석 결과를 데이터프레임형태
corner_nm_df <- as.data.frame(corner_nm_f_freq)
corner_nm_df
View(corner_nm_df)

##코너 비율분석
corner_nm_prop <- prop.table(corner_nm_freq)
corner_nm_prop
corner_nm_f_prop <- prop.table(corner_nm_f_freq)
corner_nm_f_prop

##코너 비율분석 결과를 데이터 프레임형태
corner_nm_prop_df <- as.data.frame(corner_nm_f_prop)
corner_nm_prop_df
View(corner_nm_prop_df)

##코너 비율분석 소수자리정리
round(corner_nm_prop,3)
round(corner_nm_f_prop,3)

##코너 백분율분석 
corner_nm_pect <- round(corner_nm_prop,3)*100
corner_nm_pect
corner_nm_f_pect <- round(corner_nm_f_prop,3)*100
corner_nm_f_pect

##코너 백분율분석 결과를 데이터 프레임형태
corner_nm_pect_df <- as.data.frame(corner_nm_f_pect)
corner_nm_pect_df
View(corner_nm_pect_df)

##코너 그래프
par(mfrow=c(2,2))

barplot(corner_nm_f_freq,
        main="브랜드 분포비교",
        xlab='브랜드',ylab='거래횟수',las=2)

barplot(corner_nm_f_freq,
        main="브랜드 분포비교",
        xlab='브랜드',ylab='거래횟수',las=2, horiz=TRUE)

barplot(corner_nm_f_prop,
        main="브랜드 비율 분포비교",
        xlab='브랜드',ylab='거래횟수',density=c(20,30),
        legend=(row.names(brd_nm_f_freq)),las=2)

barplot(corner_nm_f_prop,
        main="브랜드 비율 분포비교",
        xlab='브랜드',ylab='거래횟수',las=2, horiz=TRUE,
        density=c(20,30),col=c("lightblue","pink"),
        beside=TRUE, 
        legend=(row.names(brd_nm_f_freq)),)

##ㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡ

##pc
unique(ctg$pc_nm)
length(unique(ctg$pc_nm))
ctg$pc_nm_f <- factor(ctg$pc_nm, levels= unique(ctg$pc_nm),labels = c(0:77))
ctg$pc_nm_f <- factor(ctg$pc_nm_f, levels= c(0:77),labels = unique(ctg$pc_nm))
str(ctg$pc_nm_f)

##pc 빈도
pc_nm_freq <- table(ctg$pc_nm)
pc_nm_freq
pc_nm_f_freq<-table(ctg$pc_nm_f)
pc_nm_f_freq

##pc 빈도분석 결과를 데이터프레임형태
pc_nm_df <- as.data.frame(pc_nm_f_freq)
pc_nm_df
View(pc_nm_df)

##pc 비율분석
pc_nm_prop <- prop.table(pc_nm_freq)
pc_nm_prop
pc_nm_f_prop <- prop.table(pc_nm_f_freq)
pc_nm_f_prop

##pc 비율분석 결과를 데이터 프레임형태
pc_nm_prop_df <- as.data.frame(pc_nm_f_prop)
pc_nm_prop_df
View(pc_nm_prop_df)

##pc 비율분석 소수자리정리
round(pc_nm_prop,3)
round(pc_nm_f_prop,3)

##pc 백분율분석 
pc_nm_pect <- round(pc_nm_prop,3)*100
pc_nm_pect
pc_nm_f_pect <- round(pc_nm_f_prop,3)*100
pc_nm_f_pect

##pc 백분율분석 결과를 데이터 프레임형태
pc_nm_pect_df <- as.data.frame(pc_nm_f_pect)
pc_nm_pect_df
View(pc_nm_pect_df)

##pc 그래프
par(mfrow=c(2,2))

barplot(pc_nm_f_freq,
        main="PC 분포비교",
        xlab='PC',ylab='거래횟수',las=2)

barplot(pc_nm_f_freq,
        main="PC 분포비교",
        xlab='PC',ylab='거래횟수',las=2, horiz=TRUE)

barplot(pc_nm_f_prop,
        main="PC 비율 분포비교",
        xlab='PC',ylab='거래횟수',density=c(20,30),
        legend=(row.names(brd_nm_f_freq)),las=2)

barplot(pc_nm_f_prop,
        main="PC 비율 분포비교",
        xlab='PC',ylab='거래횟수',las=2, horiz=TRUE,
        density=c(20,30),col=c("lightblue","pink"),
        beside=TRUE,
        legend=(row.names(pc_nm_f_freq)))

##ㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡ

##team
unique(ctg$team_nm)
length(unique(ctg$team_nm))
ctg$team_nm_f <- factor(ctg$team_nm, levels= unique(ctg$team_nm),labels = c(0:4))
ctg$team_nm_f <- factor(ctg$team_nm_f, levels= c(0:4),labels = unique(ctg$team_nm))
str(ctg$team_nm_f)

##team 빈도
team_nm_freq <- table(ctg$team_nm)
team_nm_freq
team_nm_f_freq<-table(ctg$team_nm_f)
team_nm_f_freq

##team 빈도분석 결과를 데이터프레임형태
team_nm_df <- as.data.frame(team_nm_f_freq)
team_nm_df
View(team_nm_df)

##team 비율분석
team_nm_prop <- prop.table(team_nm_freq)
team_nm_prop
team_nm_f_prop <- prop.table(team_nm_f_freq)
team_nm_f_prop

##team 비율분석 결과를 데이터 프레임형태
team_nm_prop_df <- as.data.frame(team_nm_f_prop)
team_nm_prop_df
View(team_nm_prop_df)

##team 비율분석 소수자리정리
round(team_nm_prop,3)
round(team_nm_f_prop,3)

##team 백분율분석 
team_nm_pect <- round(team_nm_prop,3)*100
team_nm_pect
team_nm_f_pect <- round(team_nm_f_prop,3)*100
team_nm_f_pect

##team 백분율분석 결과를 데이터 프레임형태
team_nm_pect_df <- as.data.frame(team_nm_f_pect)
team_nm_pect_df
View(team_nm_pect_df)

##team 그래프
par(mfrow=c(2,2))

barplot(team_nm_f_freq,
        main="팀별 분포비교",
        xlab='팀별',ylab='거래횟수',las=2)

barplot(team_nm_f_freq,
        main="팀별 분포비교",
        xlab='팀별',ylab='거래횟수',las=2, horiz=TRUE)

barplot(team_nm_f_prop,
        main="팀별 비율 분포비교",
        xlab='팀별',ylab='거래횟수',density=c(20,30),
        legend=(row.names(brd_nm_f_freq)),las=2)

barplot(team_nm_f_prop,
        main="팀별 비율 분포비교",
        xlab='팀별',ylab='거래횟수',las=2, horiz=TRUE,
        density=c(20,30),col=c("lightblue","pink"),
        beside=TRUE,
        legend=(row.names(pc_nm_f_freq)))

##ㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡ

##buyer
unique(ctg$buyer_nm)
length(unique(ctg$buyer_nm))
ctg$buyer_nm_f <- factor(ctg$buyer_nm, levels= unique(ctg$buyer_nm),labels = c(0:34))
ctg$buyer_nm_f <- factor(ctg$buyer_nm_f, levels= c(0:34),labels = unique(ctg$buyer_nm))
str(ctg$buyer_nm_f)

##buyer 빈도
buyer_nm_freq <- table(ctg$buyer_nm)
buyer_nm_freq
buyer_nm_f_freq<-table(ctg$buyer_nm_f)
buyer_nm_f_freq

##buyer 빈도분석 결과를 데이터프레임형태
buyer_nm_df <- as.data.frame(buyer_nm_f_freq)
buyer_nm_df
View(buyer_nm_df)

##buyer 비율분석
buyer_nm_prop <- prop.table(buyer_nm_freq)
buyer_nm_prop
buyer_nm_f_prop <- prop.table(buyer_nm_f_freq)
buyer_nm_f_prop

##buyer 비율분석 결과를 데이터 프레임형태
buyer_nm_prop_df <- as.data.frame(buyer_nm_f_prop)
buyer_nm_prop_df
View(buyer_nm_prop_df)

##buyer 비율분석 소수자리정리
round(buyer_nm_prop,3)
round(buyer_nm_f_prop,3)

##buyer 백분율분석 
buyer_nm_pect <- round(buyer_nm_prop,3)*100
buyer_nm_pect
buyer_nm_f_pect <- round(buyer_nm_f_prop,3)*100
buyer_nm_f_pect

##buyer 백분율분석 결과를 데이터 프레임형태
buyer_nm_pect_df <- as.data.frame(buyer_nm_f_pect)
buyer_nm_pect_df
View(buyer_nm_pect_df)

##buyer 그래프
par(mfrow=c(2,2))

barplot(buyer_nm_f_freq,
        main="카테고리 분포비교",
        xlab='카테고리',ylab='거래횟수',las=2)

barplot(buyer_nm_f_freq,
        main="카테고리 분포비교",
        xlab='카테고리',ylab='거래횟수',las=2, horiz=TRUE)

barplot(buyer_nm_f_prop,
        main="카테고리 비율 분포비교",
        xlab='카테고리',ylab='거래횟수',density=c(20,30),
        legend=(row.names(brd_nm_f_freq)),las=2)

barplot(buyer_nm_f_prop,
        main="카테고리 비율 분포비교",
        xlab='카테고리',ylab='거래횟수',las=2, horiz=TRUE,
        density=c(20,30),col=c("lightblue","pink"),
        beside=TRUE,
        legend=(row.names(pc_nm_f_freq)))

##ㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡ

##ㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡ

#다항척도
##pc  
unique(ctg$pc_nm)
length(unique(ctg$pc_nm))
ctg$pc_nm_f <- factor(ctg$pc_nm, levels= unique(ctg$pc_nm),labels = c(0:77))
ctg$pc_nm_f <- factor(ctg$pc_nm_f, levels= c(0:77),labels = unique(ctg$pc_nm))
str(ctg$pc_nm_f)

##pc 빈도
pc_nm_freq <- table(ctg$pc_nm)
pc_nm_freq
pc_nm_f_freq<-table(ctg$pc_nm_f)
pc_nm_f_freq

##pc 빈도분석 결과를 데이터프레임형태
pc_nm_df <- as.data.frame(pc_nm_f_freq)
pc_nm_df
View(pc_nm_df)

##pc 비율분석
pc_nm_prop <- prop.table(pc_nm_freq)
pc_nm_prop
pc_nm_f_prop <- prop.table(pc_nm_f_freq)
pc_nm_f_prop

##pc 비율분석 결과를 데이터 프레임형태
pc_nm_prop_df <- as.data.frame(pc_nm_f_prop)
pc_nm_prop_df
View(pc_nm_prop_df)

##pc 비율분석 소수자리정리
round(pc_nm_prop,3)
round(pc_nm_f_prop,3)

##pc 백분율분석 
pc_nm_pect <- round(pc_nm_prop,3)*100
pc_nm_pect
pc_nm_f_pect <- round(pc_nm_f_prop,3)*100
pc_nm_f_pect

##pc 백분율분석 결과를 데이터 프레임형태
pc_nm_pect_df <- as.data.frame(pc_nm_f_pect)
pc_nm_pect_df
View(pc_nm_pect_df)

##pc 그래프
par(mfrow=c(2,2))

barplot(pc_nm_f_freq,
        main="PC 분포비교",
        xlab='PC',ylab='거래건수',las=2)

barplot(pc_nm_f_freq,
        main="PC 분포비교",
        xlab='PC',ylab='거래건수',las=2, horiz=TRUE)

barplot(pc_nm_f_prop,
        main="PC 비율 분포비교",
        xlab='PC',ylab='거래건수',density=c(20,30),
        legend=(row.names(brd_nm_f_freq)),las=2)

barplot(pc_nm_f_prop,
        main="PC 비율 분포비교",
        xlab='PC',ylab='거래건수',las=2, horiz=TRUE,
        density=c(20,30),col=c("lightblue","pink"),
        beside=TRUE,
        legend=(row.names(pc_nm_f_freq)))
##ㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡ

#서열
##corner_nm 전체내용
ctg$corner_nm

##일부내용
head(ctg$corner_nm,10)
tail(ctg$corner_nm,15)

#구조파악
length(ctg$corner_nm)
NROW(ctg$corner_nm)
str(ctg$corner_nm)
summary(ctg$corner_nm)
library(psych)
psych::describe(ctg$corner_nm)
unique(ctg$corner_nm)
View(ctg$corner_nm)

##팩터형
unique(ctg$corner_nm)
length(unique(ctg$corner_nm))
ctg$corner_nm_f <- factor(ctg$corner_nm, levels= unique(ctg$corner_nm),labels = c(0:308))
ctg$corner_nm_f <- factor(ctg$corner_nm_f, levels= c(0:308),labels = unique(ctg$corner_nm))
head(ctg)
str(ctg)

##빈도분석
corner_nm_freq <- table(ctg$corner_nm)
corner_nm_freq
corner_nm_f_freq<-table(ctg$corner_nm_f)
corner_nm_f_freq

##코너 빈도분석 결과를 데이터프레임형태
corner_nm_df <- as.data.frame(corner_nm_f_freq)
corner_nm_df
View(corner_nm_df)

##코너 비율분석
corner_nm_prop <- prop.table(corner_nm_freq)
corner_nm_prop
corner_nm_f_prop <- prop.table(corner_nm_f_freq)
corner_nm_f_prop

##코너 비율분석 결과를 데이터 프레임형태
corner_nm_prop_df <- as.data.frame(corner_nm_f_prop)
corner_nm_prop_df
View(corner_nm_prop_df)

##비율을 기준으로 데이터 프레임 내림차순 정려
library(dplyr)
arrange(corner_nm_prop_df,desc(Freq))

##코너 비율분석 소수자리정리
round(corner_nm_prop,3); round(corner_nm_f_prop,3)

##코너 백분율분석 
corner_nm_pect <- round(corner_nm_prop,3)*100
corner_nm_pect
corner_nm_f_pect <- round(corner_nm_f_prop,3)*100
corner_nm_f_pect

##코너 백분율분석 결과를 데이터 프레임형태
corner_nm_pect_df <- as.data.frame(corner_nm_f_pect)
corner_nm_pect_df
arrange(corner_nm_pect_df,desc(Freq))

##그래프
par(mfrow=c(1,1))
barplot(corner_nm_f_freq,
        main='층별 분포비교',
        xlab='PC',ylab = '거래건수')

##ㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡ

##part_nm 전체내용
ctg$part_nm

##일부내용
head(ctg$part_nm,10)
tail(ctg$part_nm,15)

#구조파악
length(ctg$part_nm)
NROW(ctg$part_nm)
str(ctg$part_nm)
summary(ctg$part_nm)
library(psych)
psych::describe(ctg$part_nm)
unique(ctg$part_nm)
View(ctg$part_nm)

##팩터형
unique(ctg$part_nm)
length(unique(ctg$part_nm))
ctg$part_nm_f <- factor(ctg$part_nm, levels= unique(ctg$part_nm),labels = c(0:30))
ctg$part_nm_f <- factor(ctg$part_nm_f, levels= c(0:30),labels = unique(ctg$part_nm))
head(ctg)
str(ctg)

##part별 빈도분석
part_nm_freq <- table(ctg$part_nm)
part_nm_freq
part_nm_f_freq<-table(ctg$part_nm_f)
corner_nm_f_freq

##part 빈도분석 결과를 데이터프레임형태
part_nm_df <- as.data.frame(part_nm_f_freq)
part_nm_df
View(part_nm_df)

##part 비율분석
part_nm_prop <- prop.table(part_nm_freq)
part_nm_prop
part_nm_f_prop <- prop.table(part_nm_f_freq)
part_nm_f_prop

##part 비율분석 결과를 데이터 프레임형태
part_nm_prop_df <- as.data.frame(part_nm_f_prop)
part_nm_prop_df
View(part_nm_prop_df)

##비율을 기준으로 데이터 프레임 내림차순 정려
library(dplyr)
arrange(part_nm_prop_df,desc(Freq))

##part 비율분석 소수자리정리
round(part_nm_prop,3); round(part_nm_f_prop,3)

##part 백분율분석 
part_nm_pect <- round(part_nm_prop,3)*100
part_nm_pect
part_nm_f_pect <- round(part_nm_f_prop,3)*100
part_nm_f_pect

##part 백분율분석 결과를 데이터 프레임형태
part_nm_pect_df <- as.data.frame(part_nm_f_pect)
part_nm_pect_df
arrange(part_nm_pect_df,desc(Freq))

par(mfrow=c(1,1))
barplot(part_nm_f_freq,
        main='파트별 분포비교',
        xlab='파트',ylab = '거래건수')

##ㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡ

##buyer
unique(ctg$buyer_nm)
length(unique(ctg$buyer_nm))
ctg$buyer_nm_f <- factor(ctg$buyer_nm, levels= unique(ctg$buyer_nm),labels = c(0:34))
ctg$buyer_nm_f <- factor(ctg$buyer_nm_f, levels= c(0:34),labels = unique(ctg$buyer_nm))
summary(ctg$buyer_nm_f)

##pc  
unique(ctg$pc_nm)
length(unique(ctg$pc_nm))
ctg$pc_nm_f <- factor(ctg$pc_nm, levels= unique(ctg$pc_nm),labels = c(0:77))
ctg$pc_nm_f <- factor(ctg$pc_nm_f, levels= c(0:77),labels = unique(ctg$pc_nm))
summary(ctg$pc_nm_f)

##buyer에 따른 pc간 차이 교차 빈도분석
(table(ctg$buyer_nm,ctg$pc_nm))
(table(ctg$buyer_nm_f,ctg$pc_nm_f))
(table(ctg$buyer_nm_f,ctg$pc_nm_f,useNA = 'ifany'))

##교차분석내용 객체저장
buy_pc_freq <- table(ctg$buyer_nm_f,ctg$pc_nm_f)
buy_pc_freq

##pc에 따른 buyer간 차이 교차 빈도분석
(table(ctg$pc_nm,ctg$buyer_nm))
(table(ctg$pc_nm_f,ctg$buyer_nm_f))
(table(ctg$pc_nm_f,ctg$buyer_nm_f,useNA = 'ifany'))

##교차분석내용 객체저장
pc_buy_freq <- table(ctg$pc_nm_f,ctg$buyer_nm_f)
pc_buy_freq

##교차빈도분석 부분합 계산하기
addmargins(buy_pc_freq)
addmargins(buy_pc_freq,1)
addmargins(buy_pc_freq,2)

buy_pc_freq_sum <- addmargins(buy_pc_freq,2)
buy_pc_freq_sum

##교차빈도분석을 비율분석으로 변환
(prop.table(buy_pc_freq,1)) # 각 buyer에서 pc분포비율비교
(prop.table(buy_pc_freq,2)) # 각 pc에서 buyer분포비율비교

buy_pc_prop <- prop.table(buy_pc_freq,1)
buy_pc_prop

addmargins(round(buy_pc_prop,3),2)

##교차비율분석을 백분율 분석으로 변환
buy_pc_result <- round(buy_pc_prop,3)*100
buy_pc_result

addmargins(buy_pc_result,2)

##그래프
par(mfrow=c(1,1))

plot(pc_nm_f~buyer_nm_f, data = ctg,
     main='buyer에따른 PC 분포비교',
     xlab = 'buyer', ylab= 'PC',
     col = rainbow(length(levels(ctg$pc_nm_f))))

##ㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡ



