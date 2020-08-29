df <-read.csv(file = 'XY_train.csv',na.strings = 'NA')
df$refund<-ifelse(df$tot_amt < 0, 1, 0)
df
summary(df)
#----환불 유무------------------
#install.packages("dplyr")
library(dplyr)
df_refund <- split(df,df$refund)
df_refund
str(df_refund)

df_x <- df[df$refund==1,]
str(df_x)#리펀 유
View(df_x)
df_y <- df[df$refund==0,]
str(df_y) 
View(df_y)
#리펀 없는

#-----------------브랜드별 환불 아닌거 카운트-------------
df_brd_y <- df_y%>% group_by(df_y$brd_nm) %>% summarise(n = n())
df_brd_y
df_brd_y <- data.frame(df_brd_y)
class(df_brd_y)



str(df_brd_y)
str(df_y)
df_y <- merge(df_y, df_brd_y, by.x = c("brd_nm"), by.y = c("df_y.brd_nm"))
names(df_y)

str(df_y)




View(df_y)



#-------------- 브랜드별 환불카운트---------
df_brd_x <- df_x%>% group_by(df_x$brd_nm) %>% summarise(n = n())
df_brd_x
View(df_brd_x)
df_brd_x <- data.frame(df_brd_x)

class(df_brd_x)
str(df_brd_x)
df_x <- merge(df_x,df_brd_x,by.x=c("brd_nm"),by.y=c("df_x.brd_nm"))
names(df_x)
str(df_x)
View(df_x)

write.csv(df_x, file='brand_data.csv', row.names = FALSE)



#--


df$part_nm <- recode(df$part_nm,
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

