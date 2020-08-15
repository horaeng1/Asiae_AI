### team_nm(팀)
### buyer_nm(카테고리)
### import_flg(국내/수입)



#전체 데이터셋 로딩
main_data <- read.csv('C:/Users/ASIAE_22/Downloads/X_train.csv')


# 변수컬럼명 파악
colnames(main_data)


# 데이터셋 간단 조회
head(main_data)


# 내부구조 조회
str(main_data)







##범주형 변수컬럼 서브데이터셋 추출

### team_nm(팀) -- 서열

#간단기술통계
summary(main_data$team_nm)
#간단조회
str(main_data$team_nm)
#
table(main_data$team_nm, main_data$buyer_nm)

### buyer_nm(카테고리)  -- 명목(다항)

#간단기술통계
summary(main_data$buyer_nm)
#간단조회
str(main_data$buyer_nm)
#
table(main_data$, main_data$buyer_nm)






### import_flg(국내/수입)-- 명목(이항)


#전체내용
main_data$import_flg

#일부내용
head(main_data$import_flg, 10)
tail(main_data$import_flg, 10)
main_data$import_flg[10:20]

#구조파악
length(main_data$import_flg)
NROW(main_data$import_flg)
str(main_data$import_flg)

#기술통계량

summary(main_data$import_flg)

library(psych)
psych::describe(main_data$import_flg)

library(Hmisc)
Hmisc::describe(main_data$import_flg)

library(skimr)
skim(main_data$import_flg)

#팩터형 변수로 변환
main_data$import_flg_f <- factor(main_data$import_flg, levels = c(0,1), labels = c('국산', '수입'))

#팩터형 변수 추가내용 확인
head(main_data)
str(main_data)

#국산/수입 요약집계
##빈도분석
import_flg_freq <- table(main_data$import_flg)
import_flg_freq

import_flg_f_freq <- table(main_data$import_flg_f)
import_flg_f_freq

##비율분석
import_flg_prop <- prop.table(import_flg_freq)
import_flg_prop

import_flg_f_prop <- prop.table(import_flg_f_freq)
import_flg_f_prop

round(import_flg_prop, 3)   # 소수자릿수가 불일치하면 백분율 분석 불가
round(import_flg_f_prop, 3)

##백분율분석
import_flg_pect <- round(import_flg_prop, 3) * 100
import_flg_pect

import_flg_f_pect <- round(import_flg_f_prop, 3) *100
import_flg_f_pect

##
par(mfrow=c(2,2))

barplot(import_flg_f_freq,
        main="수입여부 판매건수 분포비교",
        xlab="수입여부", ylab="판매건수")

barplot(import_flg_f_freq,
        main="수입여부 판매건수 분포비교",
        xlab="수입여부", ylab="판매건수", horiz=TRUE)

barplot(import_flg_f_freq,
        main="수입여부 판매건수 분포비교",
        xlab="수입여부", ylab="판매건수", density=c(20,30),
        legend=rowname())





table(main_data$import_, main_data$buyer_nm)
