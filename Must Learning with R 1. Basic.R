A = 2
print(A)

A == 2
A != 2

B = c(2, 3, 4, 5)
print(B)

x1 = c(1:10)
x1_2 = seq(from = 1, to = 10, by = 1)

x2 = seq(from = 1, to = 10, by = 1)
x3 = seq(from = 1, to = 10, by = 0.5)
y = rep(c(1, 2), 5)
z = rep(c(1, 2, 3, 4), c(1, 2, 3, 4))

M = matrix(
  data = x1, nrow = 5
)

M2=  matrix(
  data = x1, ncol = 5
)

df = data.frame(
  X1 = x1, col_2 = x1_2, col_3 = x2, col_4 = y)
head(df)

s1 = sample(letters,10)
df = data.frame(X1 = x1, S1 = s1)

# 벡터는 length, 행렬은 dim
length(x1)
dim(M)

A = seq(1:5)
print(A)

for(i in A){
  print(i)
}

B = c()
for(k in seq(from = 1, to = 10, by = 1)){
  B = c(B, k)
}
B

# data indexing
A[2]
A[1:3]
A[-3]
A[c(1, 3, 5)]

df[1,]
df[,1]
df[c(1, 2, 3), -3]

# String 이해하기
Numeric_Vector = c(1:20)
Chr_Vector = c("A","B","C")

str(Numeric_Vector)
str(Chr_Vector)

# String 시간(날짜)형태 변수 다루기
date_o = "2020-01-04"
date_c = as.Date(date_o, format = "%Y-%m-%d")
str(date_c)

DATE_O2 = "2015-02-04 23:13:23"
DATE_P = as.POSIXct(DATE_O2, format = "%Y-%m-%d %H:%M:%S")
str(DATE_P)
format(DATE_P,"%A")

# as와 is를 이용해 strings 확인 및 변경
x = c(1:10)
x1 = as.integer(x)
x2 = as.numeric(x)
x3 = as.factor(x)
x4 = as.character(x)

summary(x1)
str(x2)
summary(x2)
str(x3)
summary(x3)
str(x4)

y = c("str", 'str2', "str3", "str4")
is.integer(x)
is.numeric(x)
is.factor(y)
is.character(y)

# sample()을 통한 데이터 무작위 추출하기
S1 = sample(1:45, 6, replace = F)
print(S1)

set.seed(1234)
S2 = sample(1:45, 6, replace = F)
print(S2)

S3 = sample(letters,10)
print(S3)

S4 = sample(LETTERS, 10)
print(S4)

# 조건문(if) 활용하기
A = c(1:5)

if( 7 %in% A){  # %in% A에 속해 있는지 확인하는 논리문
  print("True")
}else{
  print("False")
}

# function()을 통해 사용자함수 만들기
plus_one = function(x){
  y = x+1
  return(y)
}
plus_one(10)

# Exercise
s1 = sample(1:45, 6, replace = F)
print(s1)

a = seq(from = 1, to = 99, by = 2)
print(a)
b = rep(1:5, c(2, 2, 2, 2, 2))
print(b)

x = c(1:9)
M = matrix(
  data = x, nrow = 3
)
M

Quadratic = function(x, y){
  z = x^2 + y + 10
  return(z)
}

for(i in c(2:9)){
  for(j in c(1:9)){
    print(i*j)
  }
  print(" ")
}