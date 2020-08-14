#dc_rate 칼럼
data$dc_rate <- round((data$dis_amt/data$tot_amt)*100)

#전체 변수컬럼명 파악
all_names <- names(data)

#범주형 변수컬럼명 파악
ctg_names <-c('custid','sales_date','sales_time','str_nm','goodcd','brd_nm',
              'corner_nm','pc_nm','part_nm','team_nm','buyer_nm','import_flg','inst_mon','inst_fee')

#범주형 변수컬럼 데이터셋 추출
ctg<-data[ctg_names]

##범주형 컬럼 // 명목척도 및 서열척도
##이항척도 (import_flg,inst_fee)
##다항척도 (custid,sales_date,goodcd,pc_nm,buyer_nm,inst_mon)
##서열척도 (str_nm,brd_nm,corner_nm,part_nm,team_nm)
##ㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡ

#이항척도
##improt_flg 
str(ctg$import_flg)
summary(ctg$import_flg)
ctg$import_flg_f <- factor(ctg$import_flg, levels = c(0,1), labels = c('국산','수입'))
str(ctg$improt_flg_f)

##improt_flg 빈도
improt_flg_freq <- table(ctg$improt_flg)
improt_flg_f_freq<-table(ctg$improt_flg_f)


##improt_flg 비율분석
improt_flg_prop <- prop.table(improt_flg_freq)
improt_flg_f_prop <- prop.table(improt_flg_f_freq)

##improt_flg 비율분석 소수자리정리
round(improt_flg_prop,3)
round(improt_flg_f_prop,3)

##pc 백분율분석 
improt_flg_pect <- round(improt_flg_prop,3)*100
improt_flg_f_pect <- round(improt_flg_f_prop,3)*100

##ㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡ

#이항척도
##inst_fee 
unique(ctg$inst_fee)
length(unique(ctg$inst_fee))
ctg$inst_fee_f <- factor(ctg$inst_fee, levels= c(0,1),labels = c('무이자','이자'))
str(ctg$inst_fee_f)

##inst_fee 빈도
inst_fee_freq <- table(ctg$inst_fee)
inst_fee_f_freq<-table(ctg$inst_fee_f)

##inst_fee 비율분석
inst_fee_prop <- prop.table(inst_fee_freq)
inst_fee_f_prop <- prop.table(inst_fee_f_freq)

##inst_fee 비율분석 소수자리정리
round(inst_fee_prop,3)
round(inst_fee_f_prop,3)

##inst_fee 백분율분석 
inst_fee_pect <- round(inst_fee_prop,3)*100
inst_fee_f_pect <- round(inst_fee_f_prop,3)*100

##ㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡ
##ㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡ

#다항척도
##custid  
unique(ctg$custid)
length(unique(ctg$custid))
ctg$custid_f <- factor(ctg$custid, levels= unique(ctg$custid),labels = c(0:29999))
ctg$custid_f <- factor(ctg$custid_f, levels= c(0:29999),labels = unique(ctg$custid))
str(ctg$custid_f)

##custid 빈도
custid_freq <- table(ctg$custid)
custid_f_freq<-table(ctg$custid_f)


##custid 빈도분석 결과를 데이터프레임형태
custid_df <- as.data.frame(custid_f_freq)

##custid 비율분석
custid_prop <- prop.table(custid_freq)
custid_f_prop <- prop.table(custid_f_freq)


##custid 비율분석 결과를 데이터 프레임형태
custid_prop_df <- as.data.frame(custid_f_prop)

##custid 비율분석 소수자리정리
round(custid_prop,3)
round(custid_f_prop,3)

##custid 백분율분석 
custid_pect <- round(custid_prop,3)*100
custid_f_pect <- round(custid_f_prop,3)*100

##custid 백분율분석 결과를 데이터 프레임형태
custid_pect_df <- as.data.frame(custid_f_pect)


##ㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡ

#다항척도
##sales_date
unique(ctg$sales_date)
length(unique(ctg$sales_date))
ctg$sales_date_f <- factor(ctg$sales_date, levels= unique(ctg$sales_date),labels = c(0:340))
ctg$sales_date_f <- factor(ctg$sales_date_f, levels= c(0:340),labels = unique(ctg$sales_date))
str(ctg$sales_date_f)

##sales_date 빈도
sales_date_freq <- table(ctg$sales_date)
sales_date_f_freq<-table(ctg$sales_date_f)


##sales_date 빈도분석 결과를 데이터프레임형태
sales_date_df <- as.data.frame(sales_date_f_freq)

##sales_date 비율분석
sales_date_prop <- prop.table(sales_date_freq)
sales_date_f_prop <- prop.table(sales_date_f_freq)


##sales_date 비율분석 결과를 데이터 프레임형태
sales_date_prop_df <- as.data.frame(sales_date_f_prop)

##sales_date 비율분석 소수자리정리
round(sales_date_prop,3)
round(sales_date_f_prop,3)

##sales_date 백분율분석 
sales_date_pect <- round(sales_date_prop,3)*100
sales_date_f_pect <- round(sales_date_f_prop,3)*100


##pc 백분율분석 결과를 데이터 프레임형태
sales_date_pect_df <- as.data.frame(sales_date_f_pect)

##ㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡ

#다항척도
##goodcd  
unique(ctg$goodcd)
length(unique(ctg$goodcd))
ctg$goodcd_f <- factor(ctg$goodcd, levels= unique(ctg$goodcd),labels = c(0:1756))
ctg$goodcd_f <- factor(ctg$goodcd_f, levels= c(0:1756),labels = unique(ctg$goodcd))
str(ctg$goodcd_f)

##goodcd 빈도
goodcd_freq <- table(ctg$goodcd)
goodcd_f_freq<-table(ctg$goodcd_f)

##goodcd 빈도분석 결과를 데이터프레임형태
goodcd_df <- as.data.frame(goodcd_f_freq)

##goodcd 비율분석
goodcd_prop <- prop.table(goodcd_freq)
goodcd_f_prop <- prop.table(goodcd_f_freq)


##goodcd 비율분석 결과를 데이터 프레임형태
goodcd_prop_df <- as.data.frame(goodcd_f_prop)

##goodcd 비율분석 소수자리정리
round(goodcd_prop,3)
round(goodcd_f_prop,3)

##goodcd 백분율분석 
goodcd_pect <- round(goodcd_prop,3)*100
goodcd_f_pect <- round(goodcd_f_prop,3)*100

##goodcd 백분율분석 결과를 데이터 프레임형태
goodcd_pect_df <- as.data.frame(goodcd_f_pect)

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
pc_nm_f_freq<-table(ctg$pc_nm_f)

##pc 빈도분석 결과를 데이터프레임형태
pc_nm_df <- as.data.frame(pc_nm_f_freq)

##pc 비율분석
pc_nm_prop <- prop.table(pc_nm_freq)
pc_nm_f_prop <- prop.table(pc_nm_f_freq)

##pc 비율분석 결과를 데이터 프레임형태
pc_nm_prop_df <- as.data.frame(pc_nm_f_prop)

##pc 비율분석 소수자리정리
round(pc_nm_prop,3)
round(pc_nm_f_prop,3)

##pc 백분율분석 
pc_nm_pect <- round(pc_nm_prop,3)*100
pc_nm_f_pect <- round(pc_nm_f_prop,3)*100

##pc 백분율분석 결과를 데이터 프레임형태
pc_nm_pect_df <- as.data.frame(pc_nm_f_pect)


##ㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡ

#다항척도
##buyer  
unique(ctg$buyer_nm)
length(unique(ctg$buyer_nm))
ctg$buyer_nm_f <- factor(ctg$buyer_nm, levels= unique(ctg$buyer_nm),labels = c(0:34))
ctg$buyer_nm_f <- factor(ctg$buyer_nm_f, levels= c(0:34),labels = unique(ctg$buyer_nm))
str(ctg$buyer_nm_f)

##buyer 빈도
buyer_nm_freq <- table(ctg$buyer_nm)
buyer_nm_f_freq<-table(ctg$buyer_nm_f)

##buyer 빈도분석 결과를 데이터프레임형태
buyer_nm_df <- as.data.frame(buyer_nm_f_freq)

##buyer 비율분석
buyer_nm_prop <- prop.table(buyer_nm_freq)
buyer_nm_f_prop <- prop.table(buyer_nm_f_freq)

##buyer 비율분석 결과를 데이터 프레임형태
buyer_nm_prop_df <- as.data.frame(buyer_nm_f_prop)

##buyer 비율분석 소수자리정리
round(buyer_nm_prop,3)
round(buyer_nm_f_prop,3)

##buyer 백분율분석 
buyer_nm_pect <- round(buyer_nm_prop,3)*100
buyer_nm_f_pect <- round(buyer_nm_f_prop,3)*100

##buyer 백분율분석 결과를 데이터 프레임형태
buyer_nm_pect_df <- as.data.frame(buyer_nm_f_pect)

##ㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡ

#다항척도
##inst_mon  
unique(ctg$inst_mon)
length(unique(ctg$inst_mon))
ctg$inst_mon_f <- factor(ctg$inst_mon, levels= unique(ctg$inst_mon),labels = c(0:11))
ctg$inst_mon_f <- factor(ctg$inst_mon_f, levels= c(0:11),labels = unique(ctg$inst_mon))
str(ctg$inst_mon_f)

##inst_mon 빈도
inst_mon_freq <- table(ctg$inst_mon)
inst_mon_f_freq<-table(ctg$inst_mon_f)


##inst_mon 빈도분석 결과를 데이터프레임형태
inst_mon_df <- as.data.frame(inst_mon_f_freq)

##inst_mon 비율분석
inst_mon_prop <- prop.table(inst_mon_freq)
inst_mon_f_prop <- prop.table(inst_mon_f_freq)

##inst_mon 비율분석 결과를 데이터 프레임형태
inst_mon_prop_df <- as.data.frame(inst_mon_f_prop)

##inst_mon 비율분석 소수자리정리
round(inst_mon_prop,3)
round(inst_mon_f_prop,3)

##inst_mon 백분율분석 
inst_mon_pect <- round(inst_mon_prop,3)*100
inst_mon_f_pect <- round(inst_mon_f_prop,3)*100

##inst_mon 백분율분석 결과를 데이터 프레임형태
inst_mon_pect_df <- as.data.frame(inst_mon_f_pect)

##ㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡ
##ㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡ

#서열
##str
unique(ctg$str_nm)
length(unique(ctg$str_nm))
ctg$str_nm_f <- factor(ctg$str_nm, levels= unique(ctg$str_nm),labels = c(0:3))
ctg$str_nm_f <- factor(ctg$str_nm_f, levels= c(0:3),labels = unique(ctg$str_nm))

##빈도분석
str_nm_freq <- table(ctg$str_nm)
str_nm_f_freq<-table(ctg$str_nm_f)


##str 빈도분석 결과를 데이터프레임형태
str_nm_df <- as.data.frame(str_nm_f_freq)

##str 비율분석
str_nm_prop <- prop.table(str_nm_freq)
str_nm_f_prop <- prop.table(str_nm_f_freq)


##str 비율분석 결과를 데이터 프레임형태
str_nm_prop_df <- as.data.frame(str_nm_f_prop)

##비율을 기준으로 데이터 프레임 내림차순 정려
library(dplyr)
arrange(str_nm_prop_df,desc(Freq))

##str 비율분석 소수자리정리
round(str_nm_prop,3); round(str_nm_f_prop,3)

##str 백분율분석 
str_nm_pect <- round(str_nm_prop,3)*100
str_nm_f_pect <- round(str_nm_f_prop,3)*100

##str 백분율분석 결과를 데이터 프레임형태
str_nm_pect_df <- as.data.frame(str_nm_f_pect)
arrange(str_nm_pect_df,desc(Freq))

##백분율을 기준으로 데이터 프레임 내림차순 정려
library(dplyr)
arrange(str_nm_pect_df,desc(Freq))

##ㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡ

#서열
##brd
unique(ctg$brd_nm)
length(unique(ctg$brd_nm))
ctg$brd_nm_f <- factor(ctg$brd_nm, levels= unique(ctg$brd_nm),labels = c(0:1881))
ctg$brd_nm_f <- factor(ctg$brd_nm_f, levels= c(0:1881),labels = unique(ctg$brd_nm))

##빈도분석
brd_nm_freq <- table(ctg$brd_nm)
brd_nm_f_freq<-table(ctg$brd_nm_f)

##brd 빈도분석 결과를 데이터프레임형태
brd_nm_df <- as.data.frame(brd_nm_f_freq)

##brd 비율분석
brd_nm_prop <- prop.table(brd_nm_freq)
brd_nm_f_prop <- prop.table(brd_nm_f_freq)

##brd 비율분석 결과를 데이터 프레임형태
brd_nm_prop_df <- as.data.frame(brd_nm_f_prop)

##비율을 기준으로 데이터 프레임 내림차순 정려
library(dplyr)
arrange(brd_nm_prop_df,desc(Freq))

##brd 비율분석 소수자리정리
round(brd_nm_prop,3); round(brd_nm_f_prop,3)

##brd 백분율분석 
brd_nm_pect <- round(brd_nm_prop,3)*100
brd_nm_f_pect <- round(brd_nm_f_prop,3)*100

##brd 백분율분석 결과를 데이터 프레임형태
brd_nm_pect_df <- as.data.frame(brd_nm_f_pect)
arrange(brd_nm_pect_df,desc(Freq))

##백분율을 기준으로 데이터 프레임 내림차순 정려
library(dplyr)
arrange(brd_nm_pect_df,desc(Freq))

##ㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡ

#서열
##corner 
unique(ctg$corner_nm)
length(unique(ctg$corner_nm))
ctg$corner_nm_f <- factor(ctg$corner_nm, levels= unique(ctg$corner_nm),labels = c(0:308))
ctg$corner_nm_f <- factor(ctg$corner_nm_f, levels= c(0:308),labels = unique(ctg$corner_nm))

##빈도분석
corner_nm_freq <- table(ctg$corner_nm)
corner_nm_f_freq<-table(ctg$corner_nm_f)

##코너 빈도분석 결과를 데이터프레임형태
corner_nm_df <- as.data.frame(corner_nm_f_freq)

##코너 비율분석
corner_nm_prop <- prop.table(corner_nm_freq)
corner_nm_f_prop <- prop.table(corner_nm_f_freq)

##코너 비율분석 결과를 데이터 프레임형태
corner_nm_prop_df <- as.data.frame(corner_nm_f_prop)

##비율을 기준으로 데이터 프레임 내림차순 정려
library(dplyr)
arrange(corner_nm_prop_df,desc(Freq))

##코너 비율분석 소수자리정리
round(corner_nm_prop,3); round(corner_nm_f_prop,3)

##코너 백분율분석 
corner_nm_pect <- round(corner_nm_prop,3)*100
corner_nm_f_pect <- round(corner_nm_f_prop,3)*100

##코너 백분율분석 결과를 데이터 프레임형태
corner_nm_pect_df <- as.data.frame(corner_nm_f_pect)
arrange(corner_nm_pect_df,desc(Freq))

##백분율을 기준으로 데이터 프레임 내림차순 정려
library(dplyr)
arrange(corner_nm_pect_df,desc(Freq))

##ㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡ

#서열
##part
unique(ctg$part_nm)
length(unique(ctg$part_nm))
ctg$part_nm_f <- factor(ctg$part_nm, levels= unique(ctg$part_nm),labels = c(0:30))
ctg$part_nm_f <- factor(ctg$part_nm_f, levels= c(0:30),labels = unique(ctg$part_nm))

##part별 빈도분석
part_nm_freq <- table(ctg$part_nm)
part_nm_f_freq<-table(ctg$part_nm_f)

##part 빈도분석 결과를 데이터프레임형태
part_nm_df <- as.data.frame(part_nm_f_freq)

##part 비율분석
part_nm_prop <- prop.table(part_nm_freq)
part_nm_f_prop <- prop.table(part_nm_f_freq)

##part 비율분석 결과를 데이터 프레임형태
part_nm_prop_df <- as.data.frame(part_nm_f_prop)

##비율을 기준으로 데이터 프레임 내림차순 정려
library(dplyr)
arrange(part_nm_prop_df,desc(Freq))

##part 비율분석 소수자리정리
round(part_nm_prop,3); round(part_nm_f_prop,3)

##part 백분율분석 
part_nm_pect <- round(part_nm_prop,3)*100
part_nm_f_pect <- round(part_nm_f_prop,3)*100

##part 백분율분석 결과를 데이터 프레임형태
part_nm_pect_df <- as.data.frame(part_nm_f_pect)
arrange(part_nm_pect_df,desc(Freq))

##백분율을 기준으로 데이터 프레임 내림차순 정려
library(dplyr)
arrange(part_nm_pect_df,desc(Freq))

##ㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡ

#서열
##team
unique(ctg$team_nm)
length(unique(ctg$team_nm))
ctg$team_nm_f <- factor(ctg$team_nm, levels= unique(ctg$team_nm),labels = c(0:4))
ctg$team_nm_f <- factor(ctg$team_nm_f, levels= c(0:4),labels = unique(ctg$team_nm))
str(ctg$team_nm_f)

##빈도분석
team_nm_freq <- table(ctg$team_nm)
team_nm_f_freq<-table(ctg$team_nm_f)

##team 빈도분석 결과를 데이터프레임형태
team_nm_df <- as.data.frame(team_nm_f_freq)

##team 비율분석
team_nm_prop <- prop.table(team_nm_freq)
team_nm_f_prop <- prop.table(team_nm_f_freq)


##team 비율분석 결과를 데이터 프레임형태
team_nm_prop_df <- as.data.frame(team_nm_f_prop)

##비율을 기준으로 데이터 프레임 내림차순 정려
library(dplyr)
arrange(team_nm_prop_df,desc(Freq))

##team 비율분석 소수자리정리
round(team_nm_prop,3); round(team_nm_f_prop,3)

##team 백분율분석 
team_nm_pect <- round(team_nm_prop,3)*100
team_nm_f_pect <- round(team_nm_f_prop,3)*100

##team 백분율분석 결과를 데이터 프레임형태
team_nm_pect_df <- as.data.frame(team_nm_f_pect)
arrange(team_nm_pect_df,desc(Freq))

##백분율을 기준으로 데이터 프레임 내림차순 정려
library(dplyr)
arrange(team_nm_pect_df,desc(Freq))

#dc_rate 칼럼
data$dc_rate <- round((data$dis_amt/data$tot_amt)*100)

#전체 변수컬럼명 파악
all_names <- names(data)

#범주형 변수컬럼명 파악
ctg_names <-c('custid','sales_date','sales_time','str_nm','goodcd','brd_nm',
              'corner_nm','pc_nm','part_nm','team_nm','buyer_nm','import_flg','inst_mon','inst_fee')

##ㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡ
##ㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡ
#연속형 변수컬럼명 도출
cnt_names <- setdiff(all_names,ctg_names)
cnt <- data[cnt_names]

##연속형 컬럼 // 등간척도 및 비율척도
##등간척도 (dc_reate)
##비율척도 (tot_amt,dis_amt,net_amt)

##ㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡ

#등간척도
cnt$dc_rate

#산술평균
mean(cnt$dc_rate)
mean(cnt$dc_rate, na.rm = TRUE)
mean(cnt$dc_rate, na.rm = TRUE, trim = 0.3)

#중앙값
median(cnt$dc_rate)

#최빈값
dc_freq <- table(cnt$dc_rate)
names(dc_freq)
names(which.max(dc_freq))

#팩터형
cnt$dc_rate_f <- factor(cnt$dc_rate, levels= c(0,5,10), labels = c(0,5,10))

#빈도수 계산
dc_rate_f_freq <- table(cnt$dc_rate_f)
names(dc_rate_f_freq)
names(which.max(dc_rate_f_freq))

#분산
var(cnt$dc_rate)
var(cnt$dc_rate, na.rm = TRUE)

#표준편차
sd(cnt$dc_rate)
sd(cnt$dc_rate,na.rm = TRUE)

#범위
range(cnt$dc_rate)
range(cnt$dc_rate, na.rm = TRUE)

#최대
max(cnt$dc_rate)
min(cnt$dc_rate)

#왜도와 첨도 계산용 패키지 설치와 로딩
install.packages("fBasics")
library(fBasics)

#왜도
skewness(cnt$dc_rate)
skewness(cnt$dc_rate, na.rm = TRUE)

#첨도
kurtosis(cnt$dc_rate)
kurtosis(cnt$dc_rate,na.rm = TRUE)

#간단한 플로팅
plot(cnt$dc_rate, type="p",
     pch=21, bg='blue')

##ㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡ

#비율형
##tot_amt
#산술평균
mean(cnt$tot_amt)
mean(cnt$tot_amt, na.rm = TRUE)
mean(cnt$tot_amt, na.rm = TRUE, trim = 0.3)

#중앙값
median(cnt$tot_amt)

#최빈값
tot_amt_freq <- table(cnt$tot_amt)
sort(tot_amt_freq, decreasing = TRUE)
names(tot_amt_freq)
names(which.max(tot_amt_freq))

#분산
var(cnt$tot_amt)
var(cnt$tot_amt, na.rm = TRUE)

#표준편차
sd(cnt$tot_amt)
sd(cnt$tot_amt,na.rm = TRUE)

#범위
range(cnt$tot_amt)
range(cnt$tot_amt, na.rm = TRUE)

#최대
max(cnt$tot_amt)
min(cnt$tot_amt)

#왜도
skewness(cnt$tot_amt)
skewness(cnt$tot_amt, na.rm = TRUE)

#첨도
kurtosis(cnt$tot_amt)
kurtosis(cnt$tot_amt,na.rm = TRUE)

#박스플롯
boxplot(cnt$tot_amt,main="박스플롯", ylab='토탈')

##ㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡ

#비율형
##dis_amt
#산술평균
mean(cnt$dis_amt)
mean(cnt$dis_amt, na.rm = TRUE)
mean(cnt$dis_amt, na.rm = TRUE, trim = 0.3)

#중앙값
median(cnt$dis_amt)

#최빈값
dis_amt_freq <- table(cnt$dis_amt)
sort(dis_amt_freq, decreasing = TRUE)
names(dis_amt_freq)
names(which.max(dis_amt_freq))

#분산
var(cnt$dis_amt)
var(cnt$dis_amt, na.rm = TRUE)

#표준편차
sd(cnt$dis_amt)
sd(cnt$dis_amt,na.rm = TRUE)

#범위
range(cnt$dis_amt)
range(cnt$dis_amt, na.rm = TRUE)

#최대
max(cnt$dis_amt)
min(cnt$dis_amt)

#왜도
skewness(cnt$dis_amt)
skewness(cnt$dis_amt, na.rm = TRUE)

#첨도
kurtosis(cnt$dis_amt)
kurtosis(cnt$dis_amt,na.rm = TRUE)

#박스플롯
boxplot(cnt$dis_amt,main="박스플롯", ylab='토탈')

##ㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡ

#비율형
##net_amt
#산술평균
mean(cnt$net_amt)
mean(cnt$net_amt, na.rm = TRUE)
mean(cnt$net_amt, na.rm = TRUE, trim = 0.3)

#중앙값
median(cnt$net_amt)

#최빈값
net_amt_freq <- table(cnt$net_amt)
sort(net_amt_freq, decreasing = TRUE)
names(net_amt_freq)
names(which.max(net_amt_freq))

#분산
var(cnt$net_amt)
var(cnt$net_amt, na.rm = TRUE)

#표준편차
sd(cnt$net_amt)
sd(cnt$net_amt,na.rm = TRUE)

#범위
range(cnt$net_amt)
range(cnt$net_amt, na.rm = TRUE)

#최대
max(cnt$net_amt)
min(cnt$net_amt)

#왜도
skewness(cnt$net_amt)
skewness(cnt$net_amt, na.rm = TRUE)

#첨도
kurtosis(cnt$net_amt)
kurtosis(cnt$net_amt,na.rm = TRUE)

#박스플롯
boxplot(cnt$net_amt,main="박스플롯", ylab='토탈')


