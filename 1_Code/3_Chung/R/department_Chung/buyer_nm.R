#------------------------------------------------buyer_nm_f------------------------------------------------
#------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------

length(unique(data_inst$buyer_nm))
data_inst$buyer_nm_f <- factor(data_inst$buyer_nm, levels= unique(data_inst$buyer_nm),labels = c(0:34))
data_inst$buyer_nm_f <- factor(data_inst$buyer_nm_f, levels= c(0:34),labels = unique(data_inst$buyer_nm))
table(data_inst$buyer_nm_f)

#-------------------------------------buyer_nm /dc_rate / count-------------------------------------------------------

library(doBy)

tmp <- table(data_inst$dc_rate_f, data_inst$buyer_nm_f)
tmp_prop <- prop.table(tmp, 2)
tmp_prop <- round(tmp_prop, 4) *100

tmp_prop <- as.data.frame(tmp_prop)
names(tmp_prop) <- c('할인율', '카테고리별', '건수')
tmp_prop

table(data_inst$buyer_nm_f) ###############################행사장, 조리식품, 청과곡물,점외 

ggplot(as.data.frame(tmp_prop), aes(x=카테고리별, y=건수, fill=할인율)) +
  ggtitle("할인율에 따른 카테고리별 판매건수 비교")+
  geom_bar(stat="identity")+
  geom_text(aes(y=건수, label = paste(건수,"%")),position = position_stack(vjust = 0.5), color = "black", size=3)+
  theme(axis.text.x = element_text(angle=90, vjust=0, color="black", size=10),
        plot.title = element_text(family="serif", face = "bold", hjust= 0.5, size=20))



#---------------------------------------buyer_nm /dc_rate / net_amt-----------------------------------------------

tmp <- aggregate(net_amt ~ buyer_nm_f + dc_rate_f, data_inst, sum, drop = FALSE)
tmp[is.na(tmp)]<-0
tmp

temp <-matrix(as.numeric(tmp$net_amt), ncol = length(unique(tmp$buyer_nm_f)), byrow=TRUE)
colnames(temp) <-levels(tmp$buyer_nm_f)
rownames(temp)<-levels(tmp$dc_rate_f)
temp <- as.table(temp)
temp
tmp_prop <- prop.table(temp,2)
tmp_prop <-round(tmp_prop ,4)*100
tmp_prop <-as.data.frame(tmp_prop)
tmp_prop
names(tmp_prop) <-c('할인율','카테고리별','금액')

ggplot(as.data.frame(tmp_prop), aes(x=카테고리별, y=금액, fill=할인율)) +
  ggtitle("할인율에 따른 카테고리별 판매금액 비교")+
  geom_bar(stat="identity")+
  geom_text(aes(y=금액, label = paste(금액,"%")),position = position_stack(vjust = 0.5), color = "black", size=3)+
  theme(axis.text.x = element_text(angle=90, hjust = 1, vjust=0, color="black", size=10),
        plot.title = element_text(family="serif", face = "bold", hjust= 0.5, size=20))

#--------------------------------------------buyer_nm /inst_tot / count---------------------------------------------------------
# inst_tot 팩터형 추가
# inst_tot / 무이자 할부 = 1/ 유이자 할부 = 2/ 일시불  = 3
#data_inst$inst_tot_f <- factor(data_inst$inst_tot, levels = c(1:3), labels = c("무이자 할부", "유이자 할부", "일시불"))
#-------------------------------------------------------------

tmp <- table(data_inst$inst_tot_f, data_inst$buyer_nm_f)
tmp_prop <-prop.table(tmp,2)
tmp_prop <- round(tmp_prop,4)*100
tmp_prop <-as.data.frame(tmp_prop)
names(tmp_prop) <- c('할부요인','카테고리별', '건수')
tmp_prop

ggplot(as.data.frame(tmp_prop), aes(x=카테고리별, y=건수, fill=할부요인)) +
  ggtitle("할부요인에 따른 카테고리별 판매건수 비교")+
  geom_bar(stat="identity")+
  geom_text(aes(y=건수, label = paste(건수,"%")),position = position_stack(vjust = 0.5), color = "black", size=3)+
  theme(axis.text.x = element_text(angle=90, hjust = 1, vjust=0, color="black", size=10),
        plot.title = element_text(family="serif", face = "bold", hjust= 0.5, size=20))

#--------------------------------------------buyer_nm /inst_tot / net_amt-------------------------------------------------------------

tmp <- aggregate(net_amt ~ buyer_nm_f + inst_tot_f, data_inst, sum, drop=FALSE)
tmp[is.na(tmp)] <- 0
tmp

temp <- matrix(as.numeric(tmp$net_amt), ncol=length(unique(tmp$buyer_nm_f)),  byrow=TRUE)
colnames(temp) <- levels(tmp$buyer_nm_f)
rownames(temp) <- levels(tmp$inst_tot_f)
temp
temp <- as.table(temp)
tmp_prop <- prop.table(temp, 2)
tmp_prop <- round(tmp_prop, 4)*100
tmp_prop <- as.data.frame(tmp_prop)
tmp_prop
names(tmp_prop) <- c('할부요인', '카테고리별', '금액')
tmp_prop
ggplot(as.data.frame(tmp_prop), aes(x=카테고리별, y=금액, fill=할부요인)) +
  ggtitle("할부요인에 따른 카테고리별 판매금액 비교")+
  geom_bar(stat="identity")+
  geom_text(aes(y=금액, label = paste(금액,"%")),position = position_stack(vjust = 0.5), color = "black", size=3)+
  theme(axis.text.x = element_text(angle=90, hjust = 0.5, vjust=0.5, color="black", size=10),
        plot.title = element_text(family="serif", face = "bold", hjust= 0.5, size=20))


