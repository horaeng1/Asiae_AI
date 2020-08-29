#1.변수와 데이터의 정의
#숫자형 변수
x <- 3
x
class(x) #type랑 같음

#문자형 변수
user_name <- 'james'
user_name
class(user_name)

#논리형 변수
my.Var100 <- TRUE
my.Var100
class(my.Var100)

#기타변수
var1 <- NULL
var2 <- NA
var3 <- NaN
var4 <- Inf

class(var1)

#메모리상의 생성된 객체 확인
ls()

#데이터 객체
a <-365
a1 <- -28.5
a2 <- '홍길동'
a3 <- 'james'
a4 <- TRUE
a5 <- NA

#메모리에 생성된 스칼라변수 조회
a
class(a)


#벡터 데이터 객체의 변수정의
b<-c(33,-5,20:23,12,-2:3)
b1 <-c('james','anna')
b2 <- c(T,FALSE,T,TRUE,F)
b; class(b)

#결측값이 포함된 벡터
h <- c()
h1 <- c(NA)
h2 <- c(33, NA ,0:23,12,-2:3)
h3 <- c('james',NA,'anna')
h4 <- c(T,FALSE,NA,T,TRUE,F)
h; class(h3)

#벡터 인덱싱
#벡터 데이터 중 필요한 요소 추려서 보기
b[1]
b[c(1,2)]
b[c(4,2)]

#연속된 요소 선택
b[2:4]
b[(2:4)]
b[c(2:4)]
b[c(2,3:7,10)]

#마이너스 기호를 사용한 차집합개념요소
b[-1]
b[-c(2,4)]

#연속된 요소 제외 선택
b[-2:-4]
b[-(2:4)]
b[-c(2:4)]

#벡터연산
p <- c(3,4,5)
q <- c(1,2)

#숫자 벡터간 결합
j <- c(p,q)
j

#데이터객체 : matrix
m1 <- c(10,-2, 5:7)
m2 <- c(2, 12:14, -5)

#행을 기준으로 벡터를 위아래 결합
mxr <- rbind(m1,m2)
mxr

#행을 기준으로 벡터를 좌우 결합
(mxr <- cbind(m1,m2))

#행이름 열이름 확인
rownames(mxr)
colnames(mxr)


