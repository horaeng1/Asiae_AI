seq(1:100)
a <- 3
b <- 4.5
c <- a+b
print(c)

four <- NA
is.na (four)

a <- 'hello'
print(a)

c(TRUE,TRUE)&c(TRUE, FALSE)

C(TRUE,TRUE) && c(TRUE,FALSE)

c(FALSE,TRUE)&&c(TRUE,FALSE)

sex <- factor ( " m " , c ( " m " , " f " ) )
sex

levels (sex) <- c('male ', 'female ')


seq_len(4)


one <- 100
two <- 75
three <-80
four <- NA
is.na(three)


c(TRUE,TRUE)&c(TRUE,FALSE)
c(TRUE,TRUE)&&c(TRUE,FALSE)


x <- c ( " a " , " b " , " c " )
x [1:2]


1:5


h= list(a=list(val=c(1,2,3)), b=list(val=c(1,2,3,4)))

h[1]

$a
$a$val


matrix ( c (1 , 2 , 3 , 4 , 5 , 6 , 7 , 8 , 9) , nrow =3)
matrix(c(1:9), nrow=3)
x <- matrix ( c (1 , 2 , 3 , 4 , 5 , 6 , 7 , 8 , 9) , nrow =3, byrow = T)

x [1:2 , ]

d <- data.frame (x = c (1 , 2 , 3 , 4 , 5) , y = c (2 , 4 , 6 , 8 , ))
d
d [ , c("x"),drop=FALSE]

sum(c(1, 2, 3, NA), na.rm=TRUE)



for (i in 1:10){
if (i%%3==0){
    print("짝")}
else { print(i)}}

f<-c(3,6,9)
f

for (i in 1:10){
  if (i %% 3 == c(3,6,9)){
      print("짝")}
  else{print(i) }}


for (i in 1:10){
  if (i %in% c(3,6,9)){
    print("짝")}
  else{print(i)}
  }


for (i in 1:100){
  if (i%/%10==3&i%%10==3|i%/%10==6&i%%10==6|i%/%10==9&i%%10==9){
    print("짝짝")}
  else if (i%/%10==3|i%/%10==6|i%/%10==9|i%%10==3|i%%10==6|i%%10==9|i%%3==0){
    print("짝")}
  else {print(i)}
}


for (i in 1:100){
  if (i%/%10 %in% c(3,6,9)&i%%10 %in% c(3,6,9)){
    print("짝짝")}
  else if (i%/%10 %in% c(3,6,9)|i%%10 %in% c(3,6,9)){
    print("짝")}
  else {print(i)}
  }

x<-matrix(1:100, nrow=10)
x
matrix(ifelse((x%/%10 %in% c(3,6,9)) & (x%%10 %in% c(3,6,9)),"짝짝",
        ifelse((x%/%10 %in% c(3,6,9))| (x%%10 %in% c(3,6,9)),"짝", x)),nrow=10, byrow=T)


y <-matrix(1:100, nrow=10)
matrix(ifelse((y%/%10 %in% c(3,6,9)&y%%10 %in% c(3,6,9)),"짝짝",
       ifelse((y%/%10 %in% c(3,6,9)|y%%10 %in% c(3,6,9)),"짝",y)),nrow=10, byrow=T)

plot(y, type="l")

f <- function(df2) {
  df2$a <- c(1, 2, 3)
}
df <- data.frame(a=c(4, 5, 6))
f(df)
df

install.packages("doBy")
library(doBy)


summary(iris)
a<- iris
a

summaryBy(Sepal.Width + Sepal.Length ~ Species, iris)


####
f <- function(df2){
  df2$a<- c(1,2,3)
}

df <- data.frame(a=c(4,5,6))
f(df)
df

####
f<-function(df) {
  df$a<- c(1,2,3)
  return(df)
}


df <- data.frame(a=c(4,5,6))
df <-f(df)
df

sum(1:10)

d<- matrix(1:9,ncol=3)
d

apply(d,1,sum)

apply(d,2,sum)

(result<- lapply(1:3, function(x){x*2}))

(x <- list(a=1:3, b=4:6))

tapply(1:10, rep(1,10),sum)

tapply(1:10, 1:10 %%2 ==1,sum)

install.packages("mlbench")
library(mlbench)
data(Ozone)
plot(Ozone$V8, Ozone$V9)

plot(Ozone$V8, Ozone$V9, xlab='Sandburg Temperature', ylab="E1 Mote Temperature",main='Ozone')

plot(Ozone$V8, Ozone$V9, xlab='Sandburg Temperature', ylab="E1 Mote Temperature",main='Ozone',col="#FF0000")

min(Ozone$V8, na.rm=TRUE)
min(Ozone$V9, na.rm=TRUE)
max(Ozone$V8, na.rm=TRUE)
max(Ozone$V9, na.rm=TRUE)

plot(Ozone$V8, Ozone$V9, xlab='Sandburg Temperature', ylab="E1 Mote Temperature",
     main='Ozone',xlim=c(0,100), ylim=c(0,90))

data(cars)
str(cars)

head(cars)
plot(cars)
plot(cars, type="l", lty='dashed')

head(Ozone[,c("V6","V7")])
plot(Ozone$V6, Ozone$V7, xlab="Windspeed", ylab="Humidity", main="Ozone",
     pch=20, cex=.5)

plot(jitter(Ozone$V6), jitter(Ozone$V7), xlab="Windspeed", ylab="Humidity", main="Ozone",
     pch=20, cex=.5)
