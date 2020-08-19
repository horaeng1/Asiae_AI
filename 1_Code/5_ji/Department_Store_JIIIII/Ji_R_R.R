data <- read.csv(file='X_train.csv',
                 header = TRUE, sep=',',
                 stringsAsFactors = FALSE,
                 strip.white = TRUE,
                 na.strings = c(',','?','NA'))
install.packages('formattable')
library(ggplot2)
##---------------------------------------------------------------------
# inst_tot / 무이자 할부 = 1/ 유이자 할부 = 2/ 일시불  = 3

str(data)
data_tmp <- data

tmp <- as.data.frame(table(data_tmp$inst_mon, data_tmp$inst_fee))
tmp
names(tmp) <- c('inst_mon', 'inst_fee', 'inst_tot')
tmp
tmp$inst_tot <- c(3, 1, 1, 1, 1, 1 ,1, 1 ,1 ,1 ,1, 1, 3, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2 )
tmp

data_tmp <- merge(data_tmp, tmp, by = c('inst_mon', 'inst_fee'))

data_pos <- data_tmp
str(data_pos)

# inst_tot 팩터형 추가
# inst_tot / 무이자 할부 = 1/ 유이자 할부 = 2/ 일시불  = 3

data_pos$inst_tot <- as.factor(data_pos$inst_tot)
data_pos$inst_tot_f <- factor(data_pos$inst_tot, levels = c(1:3), labels = c("무이자 할부", "유이자 할부", "일시불"))

##---------------------------------------------------------------------
# - 데이터 제거
data_pos <- data_pos[data_pos$net_amt>=0,]
##---------------------------------------------------------------------
##---------------------------------------------------------------------
## dc_rate // data
from <- list(c(1:20))
to <- list(1)
from
from <- append(list(c(1*20:40)), from)
append(2, to)
to
for (i in c(2:10)){
  tmp <- c(((i*20)+1):((i+1)*20))
  from <- append(list(tmp), from)
  to <- append(i, to)
}
from
to

str(data)
dc_rate <- round((data$dis_amt/ data$tot_amt)*100, 0)
dc_rate
unique(dc_rate)

from <- list(0, c(1:5), c(6:65))
to <- list(0, 5, 10)
library(doBy)
dc_rate <- recodeVar(dc_rate , from , to)
dc_rate_f <- factor(dc_rate, levels = c(0,5,10), labels = c('0%','5%','10%'))

data$dc_rate <- dc_rate
data$dc_rate_f <- dc_rate_f
data_pos$dc_rate <- dc_rate
data_pos$dc_rate_f <- dc_rate_f
str(data_pos$dc_rate)
#-----------------------------------------------------

## dc_rate // data_pos
from <- list(c(1:20))
to <- list(1)
from
from <- append(list(c(1*20:40)), from)
append(2, to)
to
for (i in c(2:10)){
  tmp <- c(((i*20)+1):((i+1)*20))
  from <- append(list(tmp), from)
  to <- append(i, to)
}
from
to

str(data)
dc_rate <- round((data_pos$dis_amt/ data_pos$tot_amt)*100, 0)
dc_rate
unique(dc_rate)

from <- list(0, c(1:5), c(6:65))
to <- list(0, 5, 10)
library(doBy)
dc_rate <- recodeVar(dc_rate , from , to)
dc_rate_f <- factor(dc_rate, levels = c(0,5,10), labels = c('0%','5%','10%'))

data$dc_rate <- dc_rate
data$dc_rate_f <- dc_rate_f
data_pos$dc_rate <- dc_rate
data_pos$dc_rate_f <- dc_rate_f
str(data_pos$dc_rate)

##---------------------------------------------------------------------
# - 데이터 제거
data_pos <- data_pos[data_pos$net_amt>=0,]
##---------------------------------------------------------------------

#수입상품 변수 간단조회
str(data$import_flg)

#수입상품 팩터형으로 변환 0=국,1=수입
data$import_flg_f <- factor(data$import_flg,
                            levels = c(0,1),
                            labels = c('국산','수입'))

data_pos$import_flg_f <- factor(data_pos$import_flg,
                                levels = c(0,1),
                                labels = c('국산','수입'))

#수입 상품 변수 간단기술통계
summary(data$import_flg_f)

#지점에 따른 수입상품 간 차이 교차빈도분석
(table(data$str_nm, data$import_flg))
(table(data$str_nm_f,data$import_flg_f))
(table(data$str_nm_f,data$import_flg_f, useNA = 'ifany'))

#교차분석내용 객체저장
str_import_freq <- table(data$str_nm_f,data$import_flg_f)
str_import_freq

#수입상품에 따른 지점 간 차이 교차빈도분석
(table(data$import_flg,data$str_nm ))
(table(data$import_flg_f,data$str_nm_f ))
(table(data$import_flg_f,data$str_nm_f, useNA = 'ifany' ))

#교차분석내용 객체저장
import_str_freq <- table(data$import_flg_f,data$str_nm_f )
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

##ㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡ

##team
unique(data$team_nm)
length(unique(data$team_nm))
data$team_nm_f <- factor(data$team_nm, levels= unique(data$team_nm),labels = c(0:4))
data$team_nm_f <- factor(data$team_nm_f, levels= c(0:4),labels = unique(data$team_nm))
str(data$team_nm_f)

data_pos$team_nm_f <- factor(data_pos$team_nm, levels= unique(data_pos$team_nm),labels = c(0:4))
data_pos$team_nm_f <- factor(data_pos$team_nm_f, levels= c(0:4),labels = unique(data_pos$team_nm))

##team 빈도
team_nm_freq <- table(data$team_nm)
team_nm_freq
team_nm_f_freq<-table(data$team_nm_f)
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

##ㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡ

##ㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡ

#서열
##str
unique(data$str_nm)
length(unique(data$str_nm))
data$str_nm_f <- factor(data$str_nm, levels= unique(data$str_nm),labels = c(0:3))
data$str_nm_f <- factor(data$str_nm_f, levels= c(0:3),labels = unique(data$str_nm))

data_pos$str_nm_f <- factor(data_pos$str_nm, levels= unique(data_pos$str_nm),labels = c(0:3))
data_pos$str_nm_f <- factor(data_pos$str_nm_f, levels= c(0:3),labels = unique(data_pos$str_nm))

##빈도분석
str_nm_freq <- table(data$str_nm)
str_nm_f_freq<-table(data$str_nm_f)


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
round(str_nm_prop,4); round(str_nm_f_prop,4)

##str 백분율분석 
str_nm_pect <- round(str_nm_prop,4)*100
str_nm_f_pect <- round(str_nm_f_prop,4)*100

##str 백분율분석 결과를 데이터 프레임형태
str_nm_pect_df <- as.data.frame(str_nm_f_pect)
arrange(str_nm_pect_df,desc(Freq))

##백분율을 기준으로 데이터 프레임 내림차순 정려
library(dplyr)
arrange(str_nm_pect_df,desc(Freq))

##ㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡ
###할인율 team_nm
##---------------------------------------------------------------------
##---------------------------------------------------------------------(fin)
#team_nm factor
data_pos$team_nm_f <- factor(data_pos$team_nm, levels= unique(data_pos$team_nm),labels = c(0:4))
data_pos$team_nm_f <- factor(data_pos$team_nm_f, levels= c(0:4),labels = unique(data_pos$team_nm))

# team_nm / dc_rate / tot_amt 백분율
tmp <- aggregate(tot_amt ~ team_nm_f + dc_rate_f, data_pos, sum, drop=FALSE)
tmp[is.na(tmp)] <- 0
tmp

temp <- matrix(as.numeric(tmp$tot_amt), ncol=length(unique(tmp$team_nm_f)),  byrow=TRUE)
colnames(temp) <- levels(tmp$team_nm_f)
rownames(temp) <- levels(tmp$dc_rate_f)
temp <- as.table(temp)

tmp_prop <- prop.table(temp, 2)
tmp_prop <- round(tmp_prop, 4)*100
tmp_prop <- as.data.frame(tmp_prop)
#tmp_prop <- tmp_prop[-c(10:15),]
tmp_prop
names(tmp_prop) <- c('할인율','팀', '금액')

#prop 그래프
ggplot(as.data.frame(tmp_prop), aes(x=팀, y=금액, fill=할인율)) +
  ggtitle("할인율에 따른 팀별 판매금액 비교")+
  geom_bar(stat="identity")+
  geom_text(aes(y=금액, label = paste(금액,"%")),position = position_stack(vjust = 0.5), color = "black", size=4)+
  theme(axis.text.x = element_text(angle=0, hjust = 0.5, vjust=0, color="black", size=8),
        axis.text.y = element_text(angle=0, hjust = 0.5, vjust=0, color="black", size=12),
        plot.title = element_text(family="serif", face = "bold", hjust= 0.5, size=10))

#---------------------------------------------------
#건수 및 금액
# team_nm / dc_rate / tot_amt
tmp <- aggregate(tot_amt ~ team_nm_f + dc_rate_f, data_pos, sum, drop=FALSE)
tmp[is.na(tmp)] <- 0
tmp

temp <- matrix(as.numeric(tmp$tot_amt), ncol=length(unique(tmp$team_nm_f)),  byrow=TRUE)
colnames(temp) <- levels(tmp$team_nm_f)
rownames(temp) <- levels(tmp$dc_rate_f)
temp <- as.table(temp)

temp <- as.data.frame(temp)
temp <- temp[-c(10:15),]
temp

names(temp) <- c('할인율','팀', '금액')

# 그래프
ggplot(as.data.frame(temp), aes(x=팀, y=금액, fill=할인율)) +
  ggtitle("할인율에 따른 팀별 판매금액 비교")+
  geom_bar(stat="identity")+
  geom_text(aes(y=금액, label = paste(금액)),position = position_stack(vjust = 0.5), color = "black", size=4)+
  theme(axis.text.x = element_text(angle=0, hjust = 0.5, vjust=0, color="black", size=8),
        axis.text.y = element_text(angle=0, hjust = 0.5, vjust=0, color="black", size=12),
        plot.title = element_text(family="serif", face = "bold", hjust= 0.5, size=10))

#----------------------------------------------------------------------

##---------------------------------------------------------------------(fin)
# team_nm / dc_rate / count 백분율
tmp <- table(data_pos$dc_rate_f, data_pos$team_nm_f)

tmp_prop <- prop.table(tmp, 2)
tmp_prop <- round(tmp_prop, 5) *100
tmp_prop <- as.data.frame(tmp_prop)

names(tmp_prop) <- c('할인율', '팀', '건수')
tmp_prop <- tmp_prop[-c(10:15),]
tmp_prop

#prop 그래프
ggplot(as.data.frame(tmp_prop), aes(x=팀, y=건수, fill=할인율)) +
  ggtitle("할인율에 따른 팀별 판매건수 비교")+
  geom_bar(stat="identity")+
  geom_text(aes(y=건수, label = paste(건수,"%")),position = position_stack(vjust = 0.5), color = "black", size=4)+
  theme(axis.text.x = element_text(angle=0, hjust = 0.5, vjust=0, color="black", size=8),
        axis.text.y = element_text(angle=0, hjust = 0.5, vjust=0, color="black", size=12),
        plot.title = element_text(family="serif", face = "bold", hjust= 0.5, size=10))

#---------------------------------------------------
#건수 및 금액
# team_nm / dc_rate / count
tmp <- table(data_pos$dc_rate_f, data_pos$team_nm_f)
tmp

tmp <- as.data.frame(tmp)

names(tmp) <- c('할인율', '팀', '건수')
tmp <- tmp[-c(10:15),]
tmp

# 그래프
ggplot(as.data.frame(tmp), aes(x=팀, y=건수, fill=할인율)) +
  ggtitle("할인율에 따른 팀별 판매건수 비교")+
  geom_bar(stat="identity")+
  geom_text(aes(y=건수, label = paste(건수)),position = position_stack(vjust = 0.5), color = "black", size=4)+
  theme(axis.text.x = element_text(angle=0, hjust = 0.5, vjust=0, color="black", size=8),
        axis.text.y = element_text(angle=0, hjust = 0.5, vjust=0, color="black", size=12),
        plot.title = element_text(family="serif", face = "bold", hjust= 0.5, size=10))


##---------------------------------------------------------------------
### 할인율 str_nm
##---------------------------------------------------------------------(fin)
#str_nm factor
data_pos$str_nm_f <- factor(data_pos$str_nm, levels= unique(data_pos$str_nm),labels = c(0:3))
data_pos$str_nm_f <- factor(data_pos$str_nm_f, levels= c(0:3),labels = unique(data_pos$str_nm))

# str_nm / dc_rate / tot_amt  백분율
tmp <- aggregate(tot_amt ~ str_nm_f + dc_rate_f, data_pos, sum, drop=FALSE)
tmp[is.na(tmp)] <- 0
tmp

temp <- matrix(as.numeric(tmp$tot_amt), ncol=length(unique(tmp$str_nm_f)),  byrow=TRUE)
colnames(temp) <- levels(tmp$str_nm_f)
rownames(temp) <- levels(tmp$dc_rate_f)
temp <- as.table(temp)
tmp_prop <- prop.table(temp, 2)
tmp_prop <- round(tmp_prop, 4)*100
tmp_prop <- as.data.frame(tmp_prop)

tmp_prop
names(tmp_prop) <- c('할인율', '지점', '금액')

#grop 그래프
ggplot(as.data.frame(tmp_prop), aes(x=지점, y=금액, fill=할인율)) +
  ggtitle("할인율에 따른 지점별 판매금액 비교")+
  geom_bar(stat="identity")+
  geom_text(aes(y=금액, label = paste(금액,"%")),position = position_stack(vjust = 0.5), color = "black", size=4)+
  theme(axis.text.x = element_text(angle=0, hjust = 0.5, vjust=0, color="black", size=8),
        axis.text.y = element_text(angle=0, hjust = 0.5, vjust=0, color="black", size=12),
        plot.title = element_text(family="serif", face = "bold", hjust= 0.5, size=10))

##---------------------------------------------------------------------(fin)
# str_nm / dc_rate / tot_amt  건수 및 금액
tmp <- aggregate(tot_amt ~ str_nm_f + dc_rate_f, data_pos, sum, drop=FALSE)
tmp[is.na(tmp)] <- 0
tmp

temp <- matrix(as.numeric(tmp$tot_amt), ncol=length(unique(tmp$str_nm_f)),  byrow=TRUE)
colnames(temp) <- levels(tmp$str_nm_f)
rownames(temp) <- levels(tmp$dc_rate_f)
temp <- as.table(temp)

temp <- as.data.frame(temp)

temp
names(temp) <- c('할인율', '지점', '금액')

# 그래프
ggplot(as.data.frame(temp), aes(x=지점, y=금액, fill=할인율)) +
  ggtitle("할인율에 따른 지점별 판매금액 비교")+
  geom_bar(stat="identity")+
  geom_text(aes(y=금액, label = paste(금액)),position = position_stack(vjust = 0.5), color = "black", size=4)+
  theme(axis.text.x = element_text(angle=0, hjust = 0.5, vjust=0, color="black", size=8),
        axis.text.y = element_text(angle=0, hjust = 0.5, vjust=0, color="black", size=12),
        plot.title = element_text(family="serif", face = "bold", hjust= 0.5, size=10))

##---------------------------------------------------------------------(fin)
# str_nm / dc_rate / count 백분율
tmp <- table(data_pos$dc_rate_f, data_pos$str_nm_f)

tmp_prop <- prop.table(tmp, 2)
tmp_prop <- round(tmp_prop, 4) *100
tmp_prop <- as.data.frame(tmp_prop)

names(tmp_prop) <- c('할인율', '지점', '건수')
tmp_prop

#prop 그래프
ggplot(as.data.frame(tmp_prop), aes(x=지점, y=건수, fill=할인율)) +
  ggtitle("할인율에 따른 지점별 판매건수 비교")+
  geom_bar(stat="identity")+
  geom_text(aes(y=건수, label = paste(건수,"%")),position = position_stack(vjust = 0.5), color = "black", size=4)+
  theme(axis.text.x = element_text(angle=0, hjust = 0.5, vjust=0, color="black", size=8),
        axis.text.y = element_text(angle=0, hjust = 0.5, vjust=0, color="black", size=12),
        plot.title = element_text(family="serif", face = "bold", hjust= 0.5, size=10))

##---------------------------------------------------------------------(fin)
# str_nm / dc_rate / count 건수 및 금액
tmp <- table(data_pos$dc_rate_f, data_pos$str_nm_f)

tmp <- as.data.frame(tmp)

names(tmp) <- c('할인율', '지점', '건수')
tmp

# 그래프
ggplot(as.data.frame(tmp), aes(x=지점, y=건수, fill=할인율)) +
  ggtitle("할인율에 따른 지점별 판매건수 비교")+
  geom_bar(stat="identity")+
  geom_text(aes(y=건수, label = paste(건수)),position = position_stack(vjust = 0.5), color = "black", size=4)+
  theme(axis.text.x = element_text(angle=0, hjust = 0.5, vjust=0, color="black", size=8),
        axis.text.y = element_text(angle=0, hjust = 0.5, vjust=0, color="black", size=12),
        plot.title = element_text(family="serif", face = "bold", hjust= 0.5, size=10))


##---------------------------------------------------------------------
### 할인율 import_flg
##---------------------------------------------------------------------(fin)
# import_flg factor
data_pos$import_flg_f <- factor(data_pos$import_flg,
                                levels = c(0,1),
                                labels = c('국산','수입'))

# import_flg / dc_rate / tot_amt  백분율
tmp <- aggregate(tot_amt ~ import_flg_f + dc_rate_f, data_pos, sum, drop=FALSE)
tmp[is.na(tmp)] <- 0
tmp

temp <- matrix(as.numeric(tmp$tot_amt), ncol=length(unique(tmp$import_flg_f)),  byrow=TRUE)
colnames(temp) <- levels(tmp$import_flg_f)
rownames(temp) <- levels(tmp$dc_rate_f)
temp <- as.table(temp)
tmp_prop <- prop.table(temp, 2)
tmp_prop <- round(tmp_prop, 4)*100
tmp_prop <- as.data.frame(tmp_prop)
tmp_prop
names(tmp_prop) <- c('할인율', '수입여부', '금액')

#prop 그래프
ggplot(as.data.frame(tmp_prop), aes(x=수입여부, y=금액, fill=할인율)) +
  ggtitle("할인율에 따른 국산/수입 판매금액 비교")+
  geom_bar(stat="identity")+
  geom_text(aes(y=금액, label = paste(금액,"%")),position = position_stack(vjust = 0.5), color = "black", size=4)+
  theme(axis.text.x = element_text(angle=0, hjust = 0.5, vjust=0, color="black", size=8),
        axis.text.y = element_text(angle=0, hjust = 0.5, vjust=0, color="black", size=12),
        plot.title = element_text(family="serif", face = "bold", hjust= 0.5, size=10))

##---------------------------------------------------------------------
# import_flg / dc_rate / tot_amt 판매건수 및 금액
tmp <- aggregate(tot_amt ~ import_flg_f + dc_rate_f, data_pos, sum, drop=FALSE)
tmp[is.na(tmp)] <- 0
tmp

temp <- matrix(as.numeric(tmp$tot_amt), ncol=length(unique(tmp$import_flg_f)),  byrow=TRUE)
colnames(temp) <- levels(tmp$import_flg_f)
rownames(temp) <- levels(tmp$dc_rate_f)
temp <- as.table(temp)
temp <- as.data.frame(temp)
temp
names(temp) <- c('할인율', '수입여부', '금액')

# 그래프
ggplot(as.data.frame(temp), aes(x=수입여부, y=금액, fill=할인율)) +
  ggtitle("할인율에 따른 국산/수입 판매금액 비교")+
  geom_bar(stat="identity")+
  geom_text(aes(y=금액, label = paste(금액)),position = position_stack(vjust = 0.5), color = "black", size=4)+
  theme(axis.text.x = element_text(angle=0, hjust = 0.5, vjust=0, color="black", size=8),
        axis.text.y = element_text(angle=0, hjust = 0.5, vjust=0, color="black", size=12),
        plot.title = element_text(family="serif", face = "bold", hjust= 0.5, size=10))

##---------------------------------------------------------------------(fin)
##---------------------------------------------------------------------(fin)
# import_flg / dc_rate / count  백분율
tmp <- table(data_pos$dc_rate_f, data_pos$import_flg_f)
tmp_prop <- prop.table(tmp, 2)
tmp_prop <- round(tmp_prop, 4) *100

tmp_prop <- as.data.frame(tmp_prop)

names(tmp_prop) <- c('할인율', '수입여부', '건수')
tmp_prop

#prop 그래프
ggplot(as.data.frame(tmp_prop), aes(x=수입여부, y=건수, fill=할인율)) +
  ggtitle("할인율에 따른 수입여부 판매건수 비교")+
  geom_bar(stat="identity")+
  geom_text(aes(y=건수, label = paste(건수,"%")),position = position_stack(vjust = 0.5), color = "black", size=4)+
  theme(axis.text.x = element_text(angle=0, hjust = 0.5, vjust=0, color="black", size=8),
        axis.text.y = element_text(angle=0, hjust = 0.5, vjust=0, color="black", size=12),
        plot.title = element_text(family="serif", face = "bold", hjust= 0.5, size=10))

##---------------------------------------------------------------------
# import_flg / dc_rate / count  판매건수 및 금액
tmp <- table(data_pos$dc_rate_f, data_pos$import_flg_f)

tmp <- as.data.frame(tmp)

names(tmp) <- c('할인율', '수입여부', '건수')
tmp

# 그래프
ggplot(as.data.frame(tmp), aes(x=수입여부, y=건수, fill=할인율)) +
  ggtitle("할인율에 따른 수입여부 판매건수 비교")+
  geom_bar(stat="identity")+
  geom_text(aes(y=건수, label = paste(건수)),position = position_stack(vjust = 0.5), color = "black", size=4)+
  theme(axis.text.x = element_text(angle=0, hjust = 0.5, vjust=0, color="black", size=8),
        axis.text.y = element_text(angle=0, hjust = 0.5, vjust=0, color="black", size=12),
        plot.title = element_text(family="serif", face = "bold", hjust= 0.5, size=10))

##---------------------------------------------------------------------
##---------------------------------------------------------------------
##할부요인
#-------------------------------------------------------------
# Team_nm
#-------------------------------------------------------------
# team_nm / inst_tot / count 백분율
tmp <- table(data_pos$inst_tot_f, data_pos$team_nm_f)
tmp_prop <- prop.table(tmp, 2)
tmp_prop <- round(tmp_prop, 4) *100
tmp_prop
tmp_prop <- as.data.frame(tmp_prop)
names(tmp_prop) <- c('할부요인', '팀', '건수')
tmp_prop <- tmp_prop[-c(10:15),]

#prop 그래프
ggplot(as.data.frame(tmp_prop), aes(x=팀, y=건수, fill=할부요인)) +
  ggtitle("할부요인에 따른 팀별 판매건수 비교")+
  geom_bar(stat="identity")+
  geom_text(aes(y=건수, label = paste(건수,"%")),position = position_stack(vjust = 0.5), color = "black", size=4)+
  theme(axis.text.x = element_text(angle=0, hjust = 0.5, vjust=0.5, color="black", size=8),
        axis.text.y = element_text(angle=0, hjust = 0.5, vjust=0.5, color="black", size=12),
        plot.title = element_text(family="serif", face = "bold", hjust= 0.5, size=10))

#-------------------------------------------------------------
# team_nm / inst_tot / count 판매건수 및 금액
tmp <- table(data_pos$inst_tot_f, data_pos$team_nm_f)
tmp 

tmp <- as.data.frame(tmp)
names(tmp) <- c('할부요인', '팀', '건수')
tmp <- tmp[-c(10:15),]

# 그래프
ggplot(as.data.frame(tmp), aes(x=팀, y=건수, fill=할부요인)) +
  ggtitle("할부요인에 따른 팀별 판매건수 비교")+
  geom_bar(stat="identity")+
  geom_text(aes(y=건수, label = paste(건수)),position = position_stack(vjust = 0.5), color = "black", size=4)+
  theme(axis.text.x = element_text(angle=0, hjust = 0.5, vjust=0.5, color="black", size=8),
        axis.text.y = element_text(angle=0, hjust = 0.5, vjust=0.5, color="black", size=12),
        plot.title = element_text(family="serif", face = "bold", hjust= 0.5, size=10))

#-------------------------------------------------------------

# team_nm / inst_tot / tot_amt  백분율
tmp <- aggregate(tot_amt ~ team_nm_f + inst_tot_f, data_pos, sum, drop=FALSE)
tmp[is.na(tmp)] <- 0
tmp

temp <- matrix(as.numeric(tmp$tot_amt), ncol=length(unique(tmp$team_nm_f)),  byrow=TRUE)
colnames(temp) <- levels(tmp$team_nm_f)
rownames(temp) <- levels(tmp$inst_tot_f)
temp <- as.table(temp)
tmp_prop <- prop.table(temp, 2)
tmp_prop <- round(tmp_prop, 4)*100
tmp_prop <- as.data.frame(tmp_prop)
tmp_prop <- tmp_prop[-c(10:15),]
tmp_prop
names(tmp_prop) <- c('할부요인', '팀', '금액')

#prop 그래프
ggplot(as.data.frame(tmp_prop), aes(x=팀, y=금액, fill=할부요인)) +
  ggtitle("할부요인에 따른 팀별 판매금액 비교")+
  geom_bar(stat="identity")+
  geom_text(aes(y=금액, label = paste(금액,"%")),position = position_stack(vjust = 0.5), color = "black", size=4)+
  theme(axis.text.x = element_text(angle=0, hjust = 0.5, vjust=0.5, color="black", size=8),
        axis.text.y = element_text(angle=0, hjust = 0.5, vjust=0.5, color="black", size=12),
        plot.title = element_text(family="serif", face = "bold", hjust= 0.5, size=10))

#-------------------------------------------------------------

# team_nm / inst_tot / tot_amt  판매건수 및 금액
tmp <- aggregate(tot_amt ~ team_nm_f + inst_tot_f, data_pos, sum, drop=FALSE)
tmp[is.na(tmp)] <- 0
tmp

temp <- matrix(as.numeric(tmp$tot_amt), ncol=length(unique(tmp$team_nm_f)),  byrow=TRUE)
colnames(temp) <- levels(tmp$team_nm_f)
rownames(temp) <- levels(tmp$inst_tot_f)
temp <- as.table(temp)

temp <- as.data.frame(temp)
temp <- temp[-c(10:15),]
temp
names(temp) <- c('할부요인', '팀', '금액')

#그래프
ggplot(as.data.frame(temp), aes(x=팀, y=금액, fill=할부요인)) +
  ggtitle("할부요인에 따른 팀별 판매금액 비교")+
  geom_bar(stat="identity")+
  geom_text(aes(y=금액, label = paste(금액)),position = position_stack(vjust = 0.5), color = "black", size=4)+
  theme(axis.text.x = element_text(angle=0, hjust = 0.5, vjust=0.5, color="black", size=8),
        axis.text.y = element_text(angle=0, hjust = 0.5, vjust=0.5, color="black", size=12),
        plot.title = element_text(family="serif", face = "bold", hjust= 0.5, size=10))

#-------------------------------------------------------------
# 할인요인 Str_nm
#-------------------------------------------------------------
# str_nm / inst_tot / count 백분율
tmp <- table(data_pos$inst_tot_f, data_pos$str_nm_f)
tmp_prop <- prop.table(tmp, 2)
tmp_prop <- round(tmp_prop, 4) *100
tmp_prop
tmp_prop <- as.data.frame(tmp_prop)
names(tmp_prop) <- c('할부요인', '지점', '건수')
tmp_prop

#prop 그래프
ggplot(as.data.frame(tmp_prop), aes(x=지점, y=건수, fill=할부요인)) +
  ggtitle("할부요인에 따른 지점별 판매건수 비교")+
  geom_bar(stat="identity")+
  geom_text(aes(y=건수, label = paste(건수,"%")),position = position_stack(vjust = 0.5), color = "black", size=4)+
  theme(axis.text.x = element_text(angle=0, hjust = 0.5, vjust=0.5, color="black", size=8),
        axis.text.y = element_text(angle=0, hjust = 0.5, vjust=0.5, color="black", size=12),
        plot.title = element_text(family="serif", face = "bold", hjust= 0.5, size=10))

#-------------------------------------------------------------
# str_nm / inst_tot / count 판매건수 및 금액
tmp <- table(data_pos$inst_tot_f, data_pos$str_nm_f)
tmp
tmp <- as.data.frame(tmp)
names(tmp) <- c('할부요인', '지점', '건수')
tmp

# 그래프
ggplot(as.data.frame(tmp), aes(x=지점, y=건수, fill=할부요인)) +
  ggtitle("할부요인에 따른 지점별 판매건수 비교")+
  geom_bar(stat="identity")+
  geom_text(aes(y=건수, label = paste(건수)),position = position_stack(vjust = 0.5), color = "black", size=4)+
  theme(axis.text.x = element_text(angle=0, hjust = 0.5, vjust=0.5, color="black", size=8),
        axis.text.y = element_text(angle=0, hjust = 0.5, vjust=0.5, color="black", size=12),
        plot.title = element_text(family="serif", face = "bold", hjust= 0.5, size=10))

#-------------------------------------------------------------
# str_nm / inst_tot / tot_amt 백분율
tmp <- aggregate(tot_amt ~ str_nm_f + inst_tot_f, data_pos, sum, drop=FALSE)
tmp[is.na(tmp)] <- 0
tmp

temp <- matrix(as.numeric(tmp$tot_amt), ncol=length(unique(tmp$str_nm_f)),  byrow=TRUE)
colnames(temp) <- levels(tmp$str_nm_f)
rownames(temp) <- levels(tmp$inst_tot_f)
temp <- as.table(temp)
tmp_prop <- prop.table(temp, 2)
tmp_prop <- round(tmp_prop, 4)*100
tmp_prop <- as.data.frame(tmp_prop)
tmp_prop
names(tmp_prop) <- c('할부요인', '지점', '금액')

#prop 그래프
ggplot(as.data.frame(tmp_prop), aes(x=지점, y=금액, fill=할부요인)) +
  ggtitle("할부요인에 따른 지점별 판매금액 비교")+
  geom_bar(stat="identity")+
  geom_text(aes(y=금액, label = paste(금액,"%")),position = position_stack(vjust = 0.5), color = "black", size=4)+
  theme(axis.text.x = element_text(angle=0, hjust = 0.5, vjust=0.5, color="black", size=8),
        axis.text.y = element_text(angle=0, hjust = 0.5, vjust=0.5, color="black", size=12),
        plot.title = element_text(family="serif", face = "bold", hjust= 0.5, size=10))

#-------------------------------------------------------------
# str_nm / inst_tot / tot_amt 판매건수 및 금액
tmp <- aggregate(tot_amt ~ str_nm_f + inst_tot_f, data_pos, sum, drop=FALSE)
tmp[is.na(tmp)] <- 0
tmp

temp <- matrix(as.numeric(tmp$tot_amt), ncol=length(unique(tmp$str_nm_f)),  byrow=TRUE)
colnames(temp) <- levels(tmp$str_nm_f)
rownames(temp) <- levels(tmp$inst_tot_f)
temp <- as.table(temp)
temp <- as.data.frame(temp)
temp
names(temp) <- c('할부요인', '지점', '금액')

# 그래프
ggplot(as.data.frame(temp), aes(x=지점, y=금액, fill=할부요인)) +
  ggtitle("할부요인에 따른 지점별 판매금액 비교")+
  geom_bar(stat="identity")+
  geom_text(aes(y=금액, label = paste(금액)),position = position_stack(vjust = 0.5), color = "black", size=4)+
  theme(axis.text.x = element_text(angle=0, hjust = 0.5, vjust=0.5, color="black", size=8),
        axis.text.y = element_text(angle=0, hjust = 0.5, vjust=0.5, color="black", size=12),
        plot.title = element_text(family="serif", face = "bold", hjust= 0.5, size=10))

#-------------------------------------------------------------
#-------------------------------------------------------------
# 할인요인 import_flg
#-------------------------------------------------------------
# import_flg / inst_tot / count 백분율
tmp <- table(data_pos$inst_tot_f, data_pos$import_flg_f)
tmp_prop <- prop.table(tmp, 2)
tmp_prop <- round(tmp_prop, 4) *100
tmp_prop
tmp_prop <- as.data.frame(tmp_prop)
names(tmp_prop) <- c('할부요인', '수입여부', '건수')
tmp_prop

#prop 그래프
ggplot(as.data.frame(tmp_prop), aes(x=수입여부, y=건수, fill=할부요인)) +
  ggtitle("할부요인에 따른 국산/수입 판매건수 비교")+
  geom_bar(stat="identity")+
  geom_text(aes(y=건수, label = paste(건수,"%")),position = position_stack(vjust = 0.5), color = "black", size=4)+
  theme(axis.text.x = element_text(angle=0, hjust = 0.5, vjust=0.5, color="black", size=8),
        axis.text.y = element_text(angle=0, hjust = 0.5, vjust=0.5, color="black", size=12),
        plot.title = element_text(family="serif", face = "bold", hjust= 0.5, size=10))

#-------------------------------------------------------------
# import_flg / inst_tot / count 판매 건수 및 금액
tmp <- table(data_pos$inst_tot_f, data_pos$import_flg_f)
tmp
tmp <- as.data.frame(tmp)
names(tmp) <- c('할부요인', '수입여부', '건수')
tmp

# 그래프
ggplot(as.data.frame(tmp), aes(x=수입여부, y=건수, fill=할부요인)) +
  ggtitle("할부요인에 따른 국산/수입 판매건수 비교")+
  geom_bar(stat="identity")+
  geom_text(aes(y=건수, label = paste(건수)),position = position_stack(vjust = 0.5), color = "black", size=4)+
  theme(axis.text.x = element_text(angle=0, hjust = 0.5, vjust=0.5, color="black", size=8),
        axis.text.y = element_text(angle=0, hjust = 0.5, vjust=0.5, color="black", size=12),
        plot.title = element_text(family="serif", face = "bold", hjust= 0.5, size=10))

#-------------------------------------------------------------
# import_flg / inst_tot / tot_amt 백분율
tmp <- aggregate(tot_amt ~ import_flg_f + inst_tot_f, data_pos, sum, drop=FALSE)
tmp[is.na(tmp)] <- 0
tmp

temp <- matrix(as.numeric(tmp$tot_amt), ncol=length(unique(tmp$import_flg_f)),  byrow=TRUE)
colnames(temp) <- levels(tmp$import_flg_f)
rownames(temp) <- levels(tmp$inst_tot_f)
temp <- as.table(temp)
tmp_prop <- prop.table(temp, 2)
tmp_prop <- round(tmp_prop, 4)*100
tmp_prop <- as.data.frame(tmp_prop)
tmp_prop
names(tmp_prop) <- c('할부요인', '수입여부', '금액')

#prop 그래프
ggplot(as.data.frame(tmp_prop), aes(x=수입여부, y=금액, fill=할부요인)) +
  ggtitle("할부요인에 따른 국산/수입 판매금액 비교")+
  geom_bar(stat="identity")+
  geom_text(aes(y=금액, label = paste(금액,"%")),position = position_stack(vjust = 0.5), color = "black", size=4)+
  theme(axis.text.x = element_text(angle=0, hjust = 0.5, vjust=0.5, color="black", size=8),
        axis.text.y = element_text(angle=0, hjust = 0.5, vjust=0.5, color="black", size=12),
        plot.title = element_text(family="serif", face = "bold", hjust= 0.5, size=10))

#-------------------------------------------------------------
# import_flg / inst_tot / tot_amt  판매건수 및 금액
tmp <- aggregate(tot_amt ~ import_flg_f + inst_tot_f, data_pos, sum, drop=FALSE)
tmp[is.na(tmp)] <- 0
tmp

temp <- matrix(as.numeric(tmp$tot_amt), ncol=length(unique(tmp$import_flg_f)),  byrow=TRUE)
colnames(temp) <- levels(tmp$import_flg_f)
rownames(temp) <- levels(tmp$inst_tot_f)
temp <- as.table(temp)
temp <- as.data.frame(temp)
temp
names(temp) <- c('할부요인', '수입여부', '금액')

# 그래프
ggplot(as.data.frame(temp), aes(x=수입여부, y=금액, fill=할부요인)) +
  ggtitle("할부요인에 따른 국산/수입 판매금액 비교")+
  geom_bar(stat="identity")+
  geom_text(aes(y=금액, label = paste(금액)),position = position_stack(vjust = 0.5), color = "black", size=4)+
  theme(axis.text.x = element_text(angle=0, hjust = 0.5, vjust=0.5, color="black", size=8),
        axis.text.y = element_text(angle=0, hjust = 0.5, vjust=0.5, color="black", size=12),
        plot.title = element_text(family="serif", face = "bold", hjust= 0.5, size=10))

#-------------------------------------------------------------