### team_nm(팀)
### buyer_nm(카테고리)
### import_flg(국내/수입)


#전체 데이터셋 로딩
main_data <- read.csv('C:/KK/1st_X_train.csv')

# 변수컬럼명 파악
colnames(main_data)

# 데이터셋 간단 조회
head(main_data)

# 내부구조 조회
str(main_data)



##범주형 변수컬럼 서브데이터셋 추출

### team_nm(팀) -- 서열

library(psych)
psych::describe(main_data$team_nm)

library(Hmisc)
Hmisc::describe(main_data$team_nm)

library(skimr)
skim(main_data$team_nm)

#전체내용
main_data$team_nm

#일부내용
head(main_data$team_nm, 10)
tail(main_data$team_nm, 10)
main_data$team_nm[10:20]

#구조파악
length(main_data$buyer_nm)
NROW(main_data$buyer_nm)
str(main_data$buyer_nm)

#기술통계량

summary(main_data$buyer_nm)

#팩터형 변수로 변환
buyer_nm_u <- unique(main_data$buyer_nm)
buyer_nm_u <- as.array(buyer_nm_u)
main_data$buyer_nm_f <- factor(main_data$buyer_nm, levels = buyer_nm_u, labels = c(0:34))
main_data$buyer_nm_f <- factor(main_data$buyer_nm_f, levels = c(0:34), labels = buyer_nm_u)

#팩터형 변수 추가내용 확인
head(main_data)
str(main_data)

#국산/수입 요약집계
##빈도분석
buyer_nm_freq <- table(main_data$buyer_nm)
buyer_nm_freq

buyer_nm_f_freq <- table(main_data$buyer_nm_f)
buyer_nm_f_freq

##비율분석
buyer_nm_prop <- prop.table(buyer_nm_freq)
buyer_nm_prop

buyer_nm_f_prop <- prop.table(buyer_nm_f_freq)
buyer_nm_f_prop

round(buyer_nm_prop, 3)   # 소수자릿수가 불일치하면 백분율 분석 불가
round(buyer_nm_f_prop, 3)

##백분율분석
buyer_nm_pect <- round(buyer_nm_prop, 3) * 100
buyer_nm_pect

buyer_nm_f_pect <- round(buyer_nm_f_prop, 3) *100
buyer_nm_f_pect

## 시각화
par(mfrow=c(2,2))

barplot(buyer_nm_f_freq,
        main="카테고리별 판매건수 분포비교",
        xlab="카테고리", ylab="판매건수")

barplot(buyer_nm_f_freq,
        main="카테고리 판매건수 분포비교",
        xlab="카테고리", ylab="판매건수", las=1, horiz=TRUE)

barplot(buyer_nm_f_prop,
        main="카테고리 판매건수 분포비교",
        xlab="카테고리", ylab="판매건수", density=c(20,30),
        #legend=rownames(buyer_nm_f_freq))
)

barplot(buyer_nm_f_prop,
        main="카테고리 판매건수 분포비교",
        xlab="카테고리", ylab="판매건수", las=1,
        horiz=TRUE, col=c("lightblue","pink"),
        beside=TRUE, #legend=rownames(buyer_nm_f_freq)
)

par(mfrow=c(1,1))
##



### buyer_nm(카테고리)  -- 명목(다항)

library(psych)
psych::describe(main_data$buyer_nm)

library(Hmisc)
Hmisc::describe(main_data$buyer_nm)

library(skimr)
skim(main_data$buyer_nm)

#전체내용
main_data$buyer_nm

#일부내용
head(main_data$buyer_nm, 10)
tail(main_data$buyer_nm, 10)
main_data$buyer_nm[10:20]

#구조파악
length(main_data$buyer_nm)
NROW(main_data$buyer_nm)
str(main_data$buyer_nm)

#기술통계량

summary(main_data$buyer_nm)

#팩터형 변수로 변환
buyer_nm_u <- unique(main_data$buyer_nm)
buyer_nm_u <- as.array(buyer_nm_u)
main_data$buyer_nm_f <- factor(main_data$buyer_nm, levels = buyer_nm_u, labels = c(0:34))
main_data$buyer_nm_f <- factor(main_data$buyer_nm_f, levels = c(0:34), labels = buyer_nm_u)

#팩터형 변수 추가내용 확인
head(main_data)
str(main_data)

#국산/수입 요약집계
##빈도분석
buyer_nm_freq <- table(main_data$buyer_nm)
buyer_nm_freq

buyer_nm_f_freq <- table(main_data$buyer_nm_f)
buyer_nm_f_freq

##비율분석
buyer_nm_prop <- prop.table(buyer_nm_freq)
buyer_nm_prop

buyer_nm_f_prop <- prop.table(buyer_nm_f_freq)
buyer_nm_f_prop

round(buyer_nm_prop, 3)   # 소수자릿수가 불일치하면 백분율 분석 불가
round(buyer_nm_f_prop, 3)

##백분율분석
buyer_nm_pect <- round(buyer_nm_prop, 3) * 100
buyer_nm_pect

buyer_nm_f_pect <- round(buyer_nm_f_prop, 3) *100
buyer_nm_f_pect

## 시각화
par(mfrow=c(2,2))

barplot(buyer_nm_f_freq,
        main="카테고리별 판매건수 분포비교",
        xlab="카테고리", ylab="판매건수")

barplot(buyer_nm_f_freq,
        main="카테고리 판매건수 분포비교",
        xlab="카테고리", ylab="판매건수", las=1, horiz=TRUE)

barplot(buyer_nm_f_prop,
        main="카테고리 판매건수 분포비교",
        xlab="카테고리", ylab="판매건수", density=c(20,30),
        #legend=rownames(buyer_nm_f_freq))
        )

barplot(buyer_nm_f_prop,
        main="카테고리 판매건수 분포비교",
        xlab="카테고리", ylab="판매건수", las=1,
        horiz=TRUE, col=c("lightblue","pink"),
        beside=TRUE, #legend=rownames(buyer_nm_f_freq)
        )

par(mfrow=c(1,1))
##



### import_flg(국내/수입)-- 명목(이항)

library(psych)
psych::describe(main_data$import_flg)

library(Hmisc)
Hmisc::describe(main_data$import_flg)

library(skimr)
skim(main_data$import_flg)

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

## 시각화
par(mfrow=c(2,2))

barplot(import_flg_f_freq,
        main="수입여부 판매건수 분포비교",
        xlab="수입여부", ylab="판매건수")

barplot(import_flg_f_freq,
        main="수입여부 판매건수 분포비교",
        xlab="수입여부", ylab="판매건수", horiz=TRUE)

barplot(import_flg_f_prop,
        main="수입여부 판매건수 분포비교",
        xlab="수입여부", ylab="판매건수", density=c(20,30),
        legend=rownames(import_flg_f_freq))

barplot(import_flg_f_prop,
        main="수입여부 판매건수 분포비교",
        xlab="수입여부", ylab="판매건수",
        horiz=TRUE, col=c("lightblue","pink"),
        beside=TRUE, legend=rownames(import_flg_f_freq))

par(mfrow=c(1,1))
##



##

##

table(main_data$import_, main_data$buyer_nm)
