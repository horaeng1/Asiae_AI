#UTF-8로 저장된 고객주문정보 CSV 파일을 읽어서 df라는 이름의 데이터 프레임으로 저장한다.
df <- read.csv("CUST_ORDER_OUTPUT2.utf8.csv",header=TRUE)
View(df)

#일부 데이터에 값이 아닌 *가 들어가 있는 경우가 있어서 사용할 수 없는 데이터는 제거한다.
df <- df[df$SEX != "*",]
#df에 저장된 데이터의 개수를 구한다.
nrow(df)

# 성별 구매 건수 -------------------------------------------------------------------------------------
#구매일자, 성별, 구매 건수만 추출하여 별도의 데이터 프레임을 만든다.
sex1 <- subset(df, select=c(ORDER_DATE,SEX,QTY))
#구매일자에서 월 정보만 추출하기 위해 lubridate 라이브러리를 설치한다.
install.packages("lubridate")
library(lubridate)
#ORDER_DATE 열의 날짜를 추출하여 month라는 열에 추가한다.
sex2 <- cbind(sex1, month=month(sex1$ORDER_DATE))
#데이터 조작을 위해 reshape2 라이브러리를 설치한다.
install.packages('reshape2')
library(reshape2)
#성별로 구분하여 월별 구매 건수의 총합을 구해서 확인한다.
dcast(sex2, SEX ~ month,value.var='QTY',sum)
#F나 M대신 female, male이라고 표시하기 위한 변환함수를 만든다.
changeGender <- function(x) { if (x[2] == 'M') { return("male") } else { return("female") } }
#변환함수를 이용하여 gender 열을 추가한다.
sex2$gender <- apply(sex2,2,changeGender)

#성별 별로 월별 구매 건수를 누적 막대 그래프로 표시한다.
library(ggplot2)
qplot(month,data=sex2,geom="bar",fill=gender)
#성별 별로 월 별 구매 건수를 별도의 막대 그래프로 표시한다.
ggplot(sex2,aes(month))+geom_bar()+facet_wrap(~gender)

# 연령대 구매 건수 ----------------------------------------------------------------------------------
#구매일자, 나이, 구매 건수만 추출하여 별도의 데이터 프레임으로 만든다.
age1 <- subset(df, select=c(ORDER_DATE,AGE,QTY))
#추출된 데이터 프레임 데이터 일부를 확인한다.
head(age1)
#ORDER_DATE 열에 저장되어 있는 날짜를 이용하여 month 함수로 월만 추출해서 month라는 새로운 열을 추가한다.
age2 <- cbind(age1, month=month(age1$ORDER_DATE))
#연령별로 구분하여 월별 구매 건수의 총합을 구해서 확인한다.
dcast(age2, AGE~month, value.var="QTY",sum)
#그래프에 표시할 때, 10대, 20대, 30대와 같이 표시될 수 있도록 나이에 “대”를 붙여서 ages라는 열을 추가한다.
age2 <- cbind(age2, ages=paste(age2$AGE,"대"))
View(age2)
#연령 별로 월 별 구매 건수를 누적 막대 그래프로 표시한다.
qplot(month,data=age2,geom="bar",fill=ages)
#연령 별로 월 별 구매 건수를 별도의 막대 그래프로 표시한다.
ggplot(age2,aes(month))+geom_bar()+facet_wrap(~ages)

#성별/연령대 구매 건수 --------------------------------------------------------------------------
#구매일자, 성별, 나이, 구매 건수만 추출하여 별도의 데이터 프레임을 만든다.
sexage1 <- subset(df,select=c(ORDER_DATE,SEX,AGE,QTY))
#추출된 데이터를 확인한다.
head(sexage1)
#원본 데이터를 분석하기 쉬운 형태로 나누어서 다시 새로운 형태로 만들기 위해 plyr 패키지를 사용한다.
library(plyr)
#ddply() 함수를 사용하여 날짜, 성별, 나이로 그룹을 지어 구매 수량을 합쳐서 확인한다.
sexage2 <- ddply(sexage1, .(ORDER_DATE,SEX,AGE), summarize, Sum_F=sum(QTY))
#합쳐진 데이터를 확인한다.
head(sexage2)
#ORDER_DATE 열에 저장되어 있는 날짜를 이용하여 month 함수로 월만 추출해서 month라는 새로운 열을 추가한다.
sexage3 <- cbind(sexage2, month=month(sexage2$ORDER_DATE))
#ages열에는 나이에 “대”를 붙여서 추가한다.
sexage3 <- cbind(sexage3, ages=paste(sexage3$AGE,"대"))
#정리된 데이터를 확인한다.
head(sexage3)
#성별과 연령을 기준으로 월별 구매 건수를 별도의 막대 그래프로 표시한다.
ggplot(sexage3,aes(month))+geom_bar()+facet_wrap(~SEX+ages)

# 연령대/성별 구매 건수 -----------------------------------------------------------------------------
#ddply() 함수를 사용하여 날짜, 나이, 성별로 그룹을 지어 구매 수량을 합쳐서 확인한다.
sexage2 <- ddply(sexage1, .(ORDER_DATE,AGE,SEX), summarise, Sum_F=sum(QTY))
#month() 함수로 월만 추출하여 month열을 추가한다.
sexage3 <- cbind(sexage2, month=month(sexage2$ORDER_DATE))
#ages열에는 나이에 “대”를 붙여서 추가한다.
sexage3 <- cbind(sexage3, ages=paste(sexage3$AGE,"대"))
#연령과 성별을 기준으로 월별 구매 건수를 별도의 막대 그래프로 표시한다.
ggplot(sexage3,aes(month))+geom_bar()+facet_wrap(~ages+SEX)

# 월별 고객 단위 구매 금액 ------------------------------------------------------------------------
#구매일자, 고객 번호, 구매 금액만 추출하여 별도의 데이터 프레임을 만든다.
cust1 <- subset(df, select=c(ORDER_DATE,CUST_SERIAL_NO,PRICE))
#추출된 데이터를 확인한다.
head(cust1)
#month() 함수로 월만 추출하여 month열을 추가한다.
cust2 <- cbind(cust1, month=month(cust1$ORDER_DATE))
#추출된 데이터의 개수를 확인한다.
nrow(cust2)
#ddply() 함수를 사용하여 고객 번호와 월로 그룹을 지어 구매 금액을 합친다.
cust3 <- ddply(cust2, .(CUST_SERIAL_NO,month),summarize,Sum_F=sum(PRICE))
#합친 데이터의 개수를 확인한다.
nrow(cust3)
head(cust3)
#df에 month() 함수로 월만 추출하여 month열을 추가하고 df2를 만든다.
df2 <- cbind(df, month=month(df$ORDER_DATE))
#aggregate() 함수를 이용하여 고객 번호와 월을 기준으로 구매 금액을 합친다.
aggdata <- aggregate(PRICE~CUST_SERIAL_NO+month,data=df2,sum)
#그래프의 범례로 사용하기 위해 월에 “월”을 붙여서 real_month라는 열을 추가한다.
aggdata <- cbind(aggdata, real_month=paste(aggdata$month,"월"))
head(aggdata)
#월별 총 구매 금액의 최대값과 최소값, 평균값 등을 비교하기 위해 상자 차트로 표시한다.
p <- ggplot(aggdata, aes(real_month,PRICE))
p+geom_boxplot(aes(fill=real_month))
# 500,000원 미만으로 필터 ---------------------------------------------------------------------
#구매 금액의 총합이 50만원 미만인 고객을 추출한다.
cust4 <- subset(cust3,Sum_F<500000)
#추출된 고객의 수를 확인한다.
nrow(cust4)
#월별 총 구매 금액이 50만원 미만인 구매에 대해서 최대값과 최소값, 평균값 등을 비교하기 위해 상자 차트로 표시한다.
p <- ggplot(subset(aggdata,PRICE<500000),aes(real_month,PRICE))
p+geom_boxplot(aes(fill=real_month))
# 요일별 구매 금액 합계 ----------------------------------------------------------------------------
#구매 요일, 구매 건수만 추출하여 별도의 데이터 프레임을 만든다.
day1 <- subset(df,select=c(ORDER_WEEKDAY,PRICE))
#추출된 데이터를 확인한다.
head(day1)
#ddply() 함수를 사용하여 요일별로 구매 금액을 합친다.
day2 <- ddply(day1,.(ORDER_WEEKDAY),summarize,Sum_F=sum(as.numeric(PRICE)))
#합친 데이터를 확인한다.
day2
#ddply() 함수를 사용하여 요일별로 구매 금액을 합친 결과를 aggdata.summary에 저장한다.
aggdata.summary <- ddply(day1,.(ORDER_WEEKDAY),summarize,Sum_F=sum(as.numeric(PRICE)))
#그래프에 출력될 때 자동으로 정렬해서 출력되도록 요일 앞에 1부터 7까지의 숫자를 붙이는 함수를 만들어서 적용한다.
changeRday <- function(x) { if (length(grep(x[1],"월"))>0){return("1_월")} 
  else if (length(grep(x[1],"화"))>0){return("2_화")}
  else if (length(grep(x[1],"수"))>0){return("3_수")}
  else if (length(grep(x[1],"목"))>0){return("4_목")}
  else if (length(grep(x[1],"금"))>0){return("5_금")}
  else if (length(grep(x[1],"토"))>0){return("6_토")}
  else if (length(grep(x[1],"일"))>0){return("7_일")}}
aggdata.summary$rday <- apply(aggdata.summary,1,changeRday)
aggdata.summary
#요일별 구매 금액의 총합을 막대 그래프로 표시한다.
ggplot(aggdata.summary,aes(rday,Sum_F))+geom_bar(stat="identity")

#요일별 구매 상품수 -----------------------------------------------------------------------------
#구매 요일과 구매 수량을 추출하여 별도의 데이터 프레임을 만든다.
dayamt1 <- subset(df,select=c(ORDER_WEEKDAY,QTY))
#ddply() 함수를 사용하여 요일별로 구매 수량을 합친 결과를 dayamt2에 저장한다.
dayamt2 <- ddply(dayamt1,.(ORDER_WEEKDAY),summarize,Sum_F=sum(QTY))
#요일 이름 변경 함수를 적용하여 rday열을 추가한다.
dayamt2$rday <- apply(dayamt2,1,changeRday)
#생성된 데이터를 확인한다.
dayamt2
#요일별 총 구매 수량이 2개를 초과하는 구매에 대해서 최대값과 최소값, 평균값 등을 비교하기 위해 상자 차트로 표시한다.
p <- ggplot(subset(df,QTY>2),aes(ORDER_WEEKDAY,QTY));
p + geom_boxplot(aes(fill=ORDER_WEEKDAY));
# 전체 고객의 구매 수량 상위 100개의 세분류 상품에 대해 wordcloud로 표현 ------------------
#상품 분류 코드와 구매 수량 데이터만 추출한다.
dgroup1 <- subset(df,select=c(LGROUP,MGROUP,SGROUP,DGROUP,QTY))
#추출한 데이터를 확인한다.
head(dgroup1)
#동일한 상품 분류 코드에 대한 누적 구매수량을 구한다.
dgroup2 <- ddply(dgroup1, .(LGROUP,MGROUP,SGROUP,DGROUP),summarize,Sum_F=sum(QTY))
#구한 결과를 확인한다.
head(dgroup2)
nrow(dgroup2)
#조인을 하기 위해서, 4개의 상품 분류 코드를 하나의 문자열로 합친 다음 LMSD_ICODE 열에 저장한다.
dgroup2 <- cbind(dgroup2, LMSD_ICODE=
paste(dgroup2$LGROUP,"|",dgroup2$MGROUP,"|",dgroup2$SGROUP,"|",dgroup2$DGROUP))
#상품 분류 코드에 따른 분류 이름이 저장된 CSV 파일을 읽는다.
vhd <- read.csv('VHD_GOODSKIND.csv',header=TRUE)
#조인을 하기위해서, 4개의 상품 분류 코드를 하나의 문자열로 합친 다음 LMSD_ICODE 열에 저장한다.
vhd <- cbind(vhd, LMSD_ICODE=
paste(vhd$LGROUP,"|",vhd$MGROUP,"|",vhd$SGROUP,"|",vhd$DGROUP))
#LMSD_ICODE와 DGROUP_NAME만 추출한다.
vhd2 <- subset(vhd,select=c(LMSD_ICODE,DGROUP_NAME))
#두개의 데이터 프레임을 LMSD_ICODE 열을 기준으로 조인하여 합친다.
dgroup2 <- merge(x=dgroup2,y=vhd2,by='LMSD_ICODE',all=TRUE)

#구매 수량 기준으로 내림차순 정렬을 한다.
dgroup3 <- dgroup2[c(order(-dgroup2$Sum_F)),]
dgroup3
#정렬된 데이터를 확인한다.
head(dgroup3)
#dgroup3에서 상위 100개의 합계만 추출하여 별도의 벡터를 만든다.
vec1 <- c()
for (j in 1:100) {
  print(j)
  print(dgroup3$Sum_F[j])
  vec1 <- c(vec1,as.numeric((dgroup3$Sum_F[j])))
}
#만들어진 벡터를 확인한다.
vec1
#wordcloud를 만들기 위해 라이브러리를 로드한다.
install.packages("wordcloud")
library(wordcloud)
#색상 팔레트를 만든다.
palete <- brewer.pal(9,"Set1")
#wordcloud를 이용하여 상위 100개의 상품 세분류 이름을 시각화한다.
wordcloud(dgroup3$DGROUP_NAME,freq=vec1,scale=c(5,1),rot.per=0.25,min.freq=2,random.order=F,random.color=T,colors=palete)
# 구매 내역 중 “패션/잡화”와 “뷰티” 대분류에 대한 연관성을 분석 -------------------------------
# LGROUP 코드가 10이나 20인 데이터만 추출한다.
anal.df <- subset(df,LGROUP==c(10,20))
# ORDER_DATE를 month() 함수로 월만 추출하여 month 컬럼을 추가한다.
anal.df <- cbind(anal.df, month=month(anal.df$ORDER_DATE))
#추출된 데이터의 개수를 확인한다.
nrow(anal.df)
#고객 번호와 구매 월, 구매 요일을 이용하여 ID를 생성한다.
anal.df$ID <- paste(anal.df$CUST_SERIAL_NO,anal.df$month,anal.df$ORDER_WEEKDAY,sep="_")
#조인을 위해 LMSD_ICODE를 추가한다.
anal.df <- cbind(anal.df, LMSD_ICODE=
paste(anal.df$LGROUP,"|",anal.df$MGROUP,"|",anal.df$SGROUP,"|",anal.df$DGROUP))
#데이터 구조를 확인한다.
str(anal.df)
#두개의 데이터 프레임을 조인한다.
anal.df <- merge(x=anal.df,y=vhd2,by='LMSD_ICODE',all=TRUE)
#데이터 구조를 확인한다.
str(anal.df)
#연관성 분석을 위해서 리스트 형식으로 변환한다.
library(data.table)
uni.id <- unique(anal.df$ID)
list.x <- data.table(anal.df)
list.x <- list.x[,unique(DGROUP_NAME),by=ID]
list.x <- data.frame(list.x[,lapply(.SD,list),by=ID])
list.x <- list.x[,2]
names(list.x) <- paste("Tr",c(1:length(uni.id)),sep="")
#대용량의 데이터로부터 효율적으로 연관 규칙을 도출하기 위해 사용하는 arules 패키지를 설치한다.
install.packages("arules")
install.packages("arulesViz")
library(arules)
library(arulesViz)
Arule.list <- as(list.x,"transactions")
#apriori() 함수를 사용하여 연관성 분석을 수행한다.
Arule.result <- apriori(Arule.list,parameter=list(supp=0.0001,confidence=0.0001))
#분석 결과를 확인한다.
Arule.result
#78개 룰에 대한 분석 결과를 시각화한다.
plot(Arule.result)
#패션/잡화와 뷰티가 어떻게 연관되어 있는지 상세히 표시한다.
plot(Arule.result,method="grouped")
#각 상품에 대한 연관성을 관계도를 이용하 보여준다.
plot(Arule.result,method="graph",control=list(type="items"))
#연관성 분석 결과를 CSV 파일로 저장한다.
write.csv(as(Arule.result,"data.frame"), "result.csv",row.names=FALSE)

# 등산화 한가지 분류에 대한 룰을 선택하여 시각화-----------------------------------------------
#연관성 분석을 수행한 결과 만들어지는 Arule.result에서 등산화 관련 부분만 추출한다.
test <- subset(Arule.result, subset = rhs %in% "등산화")
#등산화와 같이 구매하는 연관된 상품들을 시각화한다.
plot(test,method="graph",control=list(type="items"))
