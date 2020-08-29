# tidyr 패키지 설치
install.packages('tidyr')
library('tidyr')

# directory 설정 변경하기
setwd("C:/Users/Olivia/Documents/R_basic")

# dataframe에 csv파일들 넣기
(train1 <- read.csv('Y_train.csv', sep = ',', header = T, stringsAsFactors = FALSE))
(train2 <- read.csv('X_train.csv', sep = ',', header = T, stringsAsFactors = FALSE))

# join을 위한 패키지 설치
install.packages('dplyr')
library(dplyr)

# custid 기준으로 Y_train + X_train full_join하기
all <- full_join(train1,train2,by = 'custid')
View(all)

# all 중 환불 데이터 행만 추출
refund <-subset(all, tot_amt<0)

# 'part_nm'컬럼 값 오타수정 및 통일화
refund$part_nm <- recode (refund$part_nm,
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

# 수정한 'part_nm'까지 적용해서 우리팀이 필요한 열만 추출해 csv파일로 저장
our <- refund %>% select(gender,sales_date,str_nm,part_nm,tot_amt,dis_amt,net_amt)
write.csv(our, file="our_data.csv",fileEncoding='cp949')





# 성별변수 내부구조 조회
str(our$gender)

# 라이브러리 이용한 성별 기술통계량
install.packages('psych')
library(psych)
psych::describe(our$gender)

install.packages('acepack')
library(acepack)

install.packages('Hmisc')
library(Hmisc)
Hmisc::describe(our$gender)

install.packages('skimr')
library(skimr)
skim(our$gender)

# 성별변수 팩터형 변수로 변환
our$gender_f <- factor(our$gender,levels = c(0,1),
                       labels = c('M','F'))    #새로운 컬럼에 저장됨
View(our) 

# 성별변수 간단기술통계
summary(our$gender_f)





# 년월 추출
our$Month_Yr <- format(as.Date(our$sales_date), "%Y-%m")

# 날짜 오름차순 정렬해서 중복값 없애기 -> 몇개인지 확인(12개)
(MYlist <- sort(unique(our$Month_Yr)))

# 각 월을 팩터형으로 변환 (1:2000-05~12:2001-04)
our$MY_num <- factor(our$Month_Yr,
                    levels = c(MYlist),
                    labels = c(MYlist))
summary(our$MY_num)





# 성별(gender)에 따른 년월별 건수석 차이 교차빈도분석
table(our$gender, our$Month_Yr)
table(our$gender_f, our$MY_num)
(gen_MY_freq <- table(our$gender_f,our$MY_num)) # 교차분석내용 객체저장

# 년월에 따른 성별 간 건수 차이 교차빈도분석
table(our$Month_Yr, our$gender)
table(our$MY_num, our$gender_f)
(MY_gen_freq <- table(our$MY_num, our$gender_f)) # 교차분석내용 객체저장

# 교차빈도분석 부분합(margin) 계산하기
addmargins(gen_MY_freq)
addmargins(gen_MY_freq,1)
addmargins(gen_MY_freq,2)

(gen_MY_freq_sum <- addmargins(gen_MY_freq,2))

# 교차빈도분석을 비율분석으로 변환
prop.table(gen_MY_freq,1)
prop.table(gen_MY_freq,2)

(gen_MY_prop <- prop.table(gen_MY_freq,1))

addmargins(round(gen_MY_prop,3),2)

# 교차비율분석을 백분율 분석으로 변환
(gen_MY_result <- round(gen_MY_prop,3)*100)

addmargins(gen_MY_result,2)

# 성별(gender)변수와 년월(MY) 변수 간 교차빈도분석 시각화
par(mfrow=c(2,2))  # 2*2 그래프 프레임 만들기

barplot(gen_MY_freq,
        main = '구매년월에 따른 성별분포 비교: Stacked',
        xlab='구매년월', ylab='성별',
        col=c('lightblue','pink'), legend=rownames(gen_MY_freq))

barplot(gen_MY_freq,
        main = '구매년월에 따른 성별분포 비교: Grouped',
        xlab='구매년월', ylab='성별', beside = TRUE,
        col=c('lightblue','pink'), legend=rownames(gen_MY_freq))

barplot(MY_gen_freq,
        main = '성별에 따른 구매년월 분포 비교: Stacked',
        xlab='성별', ylab='월별 구매횟수',
        col=rainbow(12), legend=rownames(MY_gen_freq))

barplot(MY_gen_freq,
        main = '성별에 따른 구매년월 분포 비교: Grouped',
        xlab='성별', ylab='월별 구매횟수', beside = TRUE,
        col=rainbow(12), legend=rownames(MY_gen_freq))

# 성별(gender)변수와 년월(MY) 변수 간 교차비율분석 시각화
par(mfrow=c(2,2))  # 2*2 그래프 프레임 만들기

plot(gender_f ~ MY_num, data = our,
     main = '구매년월에 따른 성별분포 비교',
     xlab='구매년월', ylab='성별',
     col = rainbow(length(unique(our$gender_f))))

plot(MY_num ~ gender_f, data = our,
     main = '성별에 따른 구매년월 분포 비교',
     xlab='성별', ylab='구매년월',
     col = rainbow(length(levels(our$MY_num))))

mosaicplot(MY_num ~ gender_f, data = our,
           main = '구매년월에 따른 성별 분포 비교',
           xlab='구매년월', ylab='성별',
           col = rainbow(length(unique(our$gender_f))))

mosaicplot(gender_f ~ MY_num, data = our,
           main = '성별에 따른 구매년월 분포 비교',
           xlab='성별', ylab='구매년월',
           col = rainbow(length(levels(our$MY_num))))



# 구매년월 변수와 지점(str_nm)변수간 관련성
# 지점(str_nm)변수 팩터화
our$store_f <- factor(our$)






