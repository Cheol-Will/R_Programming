IMDB <- read.csv('data/IMDB-Movie-Data.csv')
View(IMBD)

is.na(IMDB$Metascore)[1:20]

# Metascore 변수 내에 결측치 갯수
sum(is.na(IMDB$Metascore)) 

# IMDB 내 모든 변수별 결측치 갯수
colSums(is.na(IMDB))

# delete NA
# NA가 존재하는 행 전체 삭제
IMDB2 = na.omit(IMDB)
IMDB2
dim(IMDB)
dim(IMDB2)

# 특정 변수(열)에 NA가 존재하는 경우 삭제
complete.cases(IMDB[,12]) # NA가 존재하는가 여부 확인
IMDB3 = IMDB[complete.cases(IMDB[,12]),]
colSums(is.na(IMDB3))

# 결측치를 특정값으로 대체할 경우
# Rawdata를 새로운 변수에 복사 
IMDB$Metascore2 = IMDB$Metascore

# 결측치 대체
IMDB$Metascore2[is.na(IMDB$Metascore2)]=58.99 

# 결측치 생략하고 계산
mean(IMDB$Revenue..Millions.) # 결과로 NA가 나옴
mean(IMDB$Revenue..Millions.,na.rm = TRUE) # NA 생략하고 계산

# 결측치 처리를 위한 데이터의 분포 탐색
library(ggplot2)

ggplot(IMDB,aes(x=Revenue..Millions.)) +
  geom_histogram(fill='royalblue', alpha = 0.4) +
  ylab('') +
  xlab("Revenue_Millions") +
  theme_classic()

ggplot(IMDB,aes(x = "",y=Revenue..Millions.)) +
  geom_boxplot(fill='red', alpha = 0.4,outlier.color = 'red') +
  xlab('') +
  ylab("Revenue_Millions") +
  theme_classic()

summary(IMDB$Revenue..Millions.)

# 6. 이상치(Outlier) 뽑아내기
ggplot(IMDB,aes(x=as.factor(Year),y=Revenue..Millions.))+
  geom_boxplot(aes(fill=as.factor(Year)),outlier.colour = 'red',alpha=I(0.4))+
  xlab("년도") + ylab("수익") + guides(fill = FALSE) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90))


# Outlier인 데이터 제거하기

# 1분위수 계산
Q1 = quantile(IMDB$Revenue..Millions.,probs = c(0.25),na.rm = TRUE) 
# 3분위수 계산
Q3 = quantile(IMDB$Revenue..Millions.,probs = c(0.75),na.rm = TRUE)

LC = Q1 - 1.5 * (Q3 - Q1) # 아래 울타리
UC = Q3 + 1.5 * (Q3 - Q1) # 위 울타리

IMDB2 = subset(IMDB,
               Revenue..Millions. >  LC & Revenue..Millions. < UC)

# 7.문자열 추출
# 첫번째 obs의 Actors변수에서 1 ~ 18번째에 해당하는 문자열 추출
substr(IMBD$Actors[1], 1, 18)

# 문자열 붙이기
paste(IMDB$Actors[1],"_",'A')
paste("asdfasdf", '-', "ASDF")
paste("asdfasdf","_",'A',sep="") # 띄어쓰기 없이 붙이기
paste("asdfasdf","_",'A',sep="||") # 띄어쓰기 없이 붙이기

# 문자열 분리
strsplit(as.character(IMDB$Actors[1]), split= ",") 
strsplit("강,철,석", split = ",")

# 문자열 대체
IMDB$Genre2=gsub(","," ",IMDB$Genre) # , 를 띄어쓰기로 대체 
gsub(",", " ", c("강,철,석", "코,딩"))

# 텍스트 마이닝 
library(tm) # tm 패키지 설치 필요

CORPUS = Corpus(VectorSource(IMDB$Genre2)) # 코퍼스 생성
CORPUS_TM = tm_map(CORPUS,removePunctuation) # 특수문자 제거
CORPUS_TM = tm_map(CORPUS_TM, removeNumbers) # 숫자 제거 
CORPUS_TM = tm_map(CORPUS_TM, tolower) # 알파벳 모두 소문자로 바꾸기


TDM=DocumentTermMatrix(CORPUS_TM) # 문서행렬 생성
inspect(TDM)

TDM = as.data.frame(as.matrix(TDM)) # 문서행렬을 데이터프레임 형태로 만들어주기.
head(TDM) 















