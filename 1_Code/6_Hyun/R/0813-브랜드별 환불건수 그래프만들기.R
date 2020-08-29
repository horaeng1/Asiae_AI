my<- read.csv(file = 'XY_train.csv', header=TRUE, sep=',', stringsAsFactors = FALSE, strip.white = TRUE, na.strings = c('.','?','NA'))
my
class(my)
str(my)


library(readr)
my_tb<- read_delim(file = 'XY_train.csv', col_names = TRUE, delim=',', trim_ws = TRUE, na= c('.','?','NA'))
my_tb

library(data.table)
my_dt<- fread(input='XY_train.csv', header=TRUE, sep = ',', stringsAsFactors = FALSE, strip.white = TRUE, na.strings = c('.','?','NA'))
my_dt

head(my)
tail(my)

str(my)

library(dplyr)
glimpse(my)

summary(my)

library(psych)
psych::describe(my)

search()

library(Hmisc)
Hmisc::describe(my)

my<- read.csv(file = 'XY_train.csv', header=TRUE, sep=',', stringsAsFactors = FALSE, strip.white = TRUE, na.strings = c('.','?','NA'))
my
class(my)
str(my)

ctg_names<-c('custid', 'gender', 'str_nm', 'goodcd', 'brd_nm', 'corner_nm', 'pc_nm', 'part_nm', 'team_nm', 'buyer_nm', 'import_flg', 'tot_amt', 'inst_fee', 'refund')
ctg<- my[ctg_names]

head(ctg)

str(ctg$gender)
table(ctg$tot_amt, ctg$custid)
table(ctg$custid, ctg$gender)

#6-6
refund<-read.csv(file='real_sibal.csv',
                 header = TRUE, sep = ',',
                 stringsAsFactors = FALSE,
                 strip.white = TRUE,
                 na.strings = c(',','?','NA'))
View(refund)

str(refund)

ctg_names<-c('custid', 'gender', 'str_nm', 'goodcd', 'brd_nm', 'corner_nm', 'pc_nm', 'part_nm', 'team_nm', 'buyer_nm', 'import_flg', 'tot_amt', 'inst_fee', 'refund')

ctg<- refund[ctg_names]
head(ctg)

str(ctg$refund)
ctg$refund_y<- factor(ctg$refund, levels=c(0,1), labels = c('유', '무'))
summary(ctg$refund_y)

str(ctg$brd_nm)

table(ctg$refund, ctg$brd_nm)
table(ctg$refund_y, ctg$brd_nm)
table(ctg$refund_y, ctg$brd_nm, useNA='ifany')

re_br_freq<-table(ctg$refund_y, ctg$brd_nm)
re_br_freq

View(re_br_freq)
table(ctg$brd_nm, ctg$refund)
table(ctg$brd_nm, ctg$refund_y)
table(ctg$brd_nm, ctg$refund_y, useNA='ifany')

br_re_freq<- table(ctg$brd_nm, ctg$refund_y)
br_re_freq

write.csv(br_re_freq,file="br.csv", fileEncoding = "UTF-8")
sb<-read.csv(file='br.csv',encoding = "UTF-8")
str(sb$X)

View(sb$유)
unique(sb$유)

#6-7
addmargins(br_re_freq)
addmargins(br_re_freq,1)
addmargins(re_br_freq)
addmargins(re_br_freq,1)
addmargins(re_br_freq,2)

re_br_freq_sum<-addmargins(re_br_freq,2)
re_br_freq_sum

prop.table(re_br_freq,1)
prop.table(re_br_freq,2)

re_br_prop<-prop.table(re_br_freq,1)
re_br_prop
View(re_br_prop)
addmargins(round(re_br_prop,3),2)

View(addmargins(round(re_br_prop,3),2))

View(head(re_br_prop))

par(mfrow=c(2,2))
barplot(re_br_freq,
        main="백화점 환불 유무에 따른 브랜드 환불분포비교",
        xlab="브랜드",ylab="건수", xlim=c(0,1880), ylim=c(0,19000),
        col=c("lightblue","pink"), legend=rownames(re_br_freq))
#xlim=c(0,5000), ylim=c(0,1880)  


barplot(re_br_freq,
        main="백화점 환불 유무에 따른 브랜드 환불분포비교",
        xlab="브랜드", ylab="건수",beside=TRUE,
        col=c("lightblue","pink"),legend=rownames(re_br_freq))

barplot(br_re_freq,
        main="백화점 브랜드에 따른 환불유무비교",
        xlab="유무",ylab="건수",
        col=c("lightblue","pink"),legend=rownames(br_re_freq)) 

barplot(br_re_freq,
        main="백화점 브랜드에 따른 환불유무비교",
        xlab="유무",ylab="건수",
        col=c("lightblue","pink"), legend=rownames(br_re_freq),beside=TRUE)  

addmargins(re_br_freq,1)
View(addmargins(re_br_freq,1))

ctg$refund_f<- as.factor(ctg$refund)
ctg$refund_f

ctg$brd_nm_f<- as.factor(ctg$brd_nm)
ctg$brd_nm_f

str(ctg)           
str(ctg$refund)
length(unique(ctg$brd_nm))
par(mfrow=c(3,3))
head(ctg)
tmp <- head(ctg)
str(tmp)
length(unique(ctg$brd_nm))
ttmp <- factor(ctg$brd_nm, levels = unique(ctg$brd_nm), labels = c(0:1881))
ctg$brd_nm_f <- factor(ttmp, levels = c(0:1881), labels= unique(ctg$brd_nm))

# ----------------------------------------------- ㅇ
plot(ctg$refund ~ ctg$brd_nm_f)
# -----------------------------------------------

plot(brd_nm_f ~ refund, data = ctg,
     main="백화점 브랜드에 따른 환불유무 분포비교",
     xlab="브랜드(brd_nm)", ylab="환불유무(refund)",
     col=rainbow(length(unique(ctg$refund))))

plot(refund_y ~ brd_nm, data = ctg,
     main="백화점 브랜드에 따른 환불유무 분포비교",
     xlab="환불유무(refund)", ylab="브랜드(brd_nm)",
     col=rainbow(length(levels(ctg$refund_y))))

summary(ctg$brd_nm)
is.na(ctg$brd_nm)

install.packages('naniar')
library(naniar)
n_miss(ctg$brd_nm)
#------------------------
View(br_re_freq$Var1)
plot(br_re_freq$Var1)



#-----------------------------------------------------
sb
summary(sb)
Hmisc::describe(sb)


from <- list(c(1:20))
to <- list(1)
for (i in c(1:99)){
        tmp <- c(((i*20)+1):((i+1)*20))
        from <- append(list(tmp) , from)
        to <- append(list((i+1)), to)
}
from
to
library(doBy)
sb$유_f <- recodeVar(sb$유, src = from, tgt = to)
str(sb)


length(unique(sb$유_f))
plot(sb$유_f)
