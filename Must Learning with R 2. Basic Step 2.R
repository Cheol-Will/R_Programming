# Load Data
HR = read.csv('data/HR_comma_sep.csv')

# Take a look at data
head(HR, n = 5)
str(HR) # structure
summary(HR)

# left는 이직여부로 1은 이직을 나타낸다. 즉 명목형 변수에 속해야한다.
HR$Work_accident = as.factor(HR$Work_accident)
HR$left = as.factor(HR$left)
HR$promotion_last_5years = as.factor(HR$promotion_last_5years)
summary(HR$left)

# Data Handling

# 조건에 맞는 값 할당하기 ifelse
HR$satisfaction_level_group_1 = ifelse(HR$satisfaction_level > 0.5, "High", "Low")
HR$satisfaction_level_group_1 = as.factor(HR$satisfaction_level_group_1)
summary(HR$satisfaction_level_group_1)

HR$satisfaction_level_group_2 = ifelse(HR$satisfaction_level > 0.8, "High", 
                                       ifelse(HR$satisfaction_level > 0.5, "Mid", "Low"))
HR$satisfaction_level_group_2 = as.factor(HR$satisfaction_level_group_2)
summary(HR$satisfaction_level_group_2)

# 조건에 맞는 데이터 "추출"
summary(HR$salary)
HR_High = subset(HR, salary == "high")
HR_High
summary(HR_High$salary)
str(HR_High)

HR_High_IT = subset(HR, salary == "high" & sales == "IT")
HR_High_IT
summary(HR_High_IT)

# 조건에 맞는 집계 데이터 만들기
install.packages("plyr")
library(plyr)
SS=ddply(HR, # 분석할 Data Set 설정
         c("sales","salary"),summarise, # 집계 기준 변수 설정 -> Groupby
         M_SF = mean(satisfaction_level), # 컬럼명 및 계산 함수 설정 -> Groupby 뒤에 오는 내용
         COUNT =length(sales), 
         M_WH = round(mean(average_montly_hours),2))
View(SS)


#ggplot2 기본 시각화
library(ggplot2)
library(ggthemes)
HR$salary = factor(HR$salary, levels = c("low", "medium", 'high'))

ggplot(HR) # 연습장 준비
ggplot(HR, aes(x = salary)) # 축 설정
ggplot(HR, aes(x = salary)) + geom_bar(fill = 'royalblue')
ggplot(HR,aes(x=salary)) + geom_bar(aes(fill=salary)) # 여기서 salary는 변수이므로 당연히 aes에 넣는다

# 그냥 bar plot
ggplot(HR,aes(x=salary)) + geom_bar(fill = 'royalblue')
ggplot(HR,aes(x=salary)) + geom_bar(aes(fill=left)) # left변수로 색 채우기 면적: fill, 선: col

# 그냥 히스토그램
ggplot(HR, aes(x = satisfaction_level)) + geom_histogram()
ggplot(HR, aes(x = satisfaction_level)) + geom_histogram(binwidth = 0.01,col='red',fill='royalblue') 

# density - 1차원에 대해 KDE로 보여준다
ggplot(HR,aes(x=satisfaction_level)) + geom_density()
ggplot(HR,aes(x=satisfaction_level)) + geom_density(fill = "royalblue")

# boxplot
ggplot(HR, aes(x = left, y = satisfaction_level)) + geom_boxplot(aes(fill = "left")) + 
  xlab("이직여부") + ylab("만족도") + ggtitle("Boxplot") + labs(fill = "이직 여부")
  
ggplot(HR, aes(x = left, y = satisfaction_level)) + 
  geom_boxplot(aes(fill = "left"), alpha = I(0.4)) + geom_jitter(aes(col = left),alpha = I(0.4)) + 
  xlab("이직여부") + ylab("만족도") + ggtitle("Boxplot") + labs(fill = "이직 여부")

ggplot(HR,aes(x=left,y=satisfaction_level)) +
  geom_boxplot(aes(fill = salary),alpha = I(0.4), outlier.colour = 'red') +
  xlab("이직여부") + ylab("만족도") + ggtitle("Boxplot") + labs(fill = "임금 수준") 

ggplot(HR, aes(x = average_montly_hours, y = satisfaction_level)) + 
  geom_point()

ggplot(HR, aes(x = average_montly_hours, y = satisfaction_level)) + 
  geom_point(aes(col = left), alpha = I(0.5)) +
  labs(col = '이직 여부') + xlab("평균 근무시간") + ylab("만족도")


# Exercise
# 1 
length(HR)
nrow(HR)
ncol(HR)
dim(HR)

# 2
str(HR$salary)

# 3
HR$salary_New = ifelse(HR$salary == "low", 1, 
                       ifelse(HR$salary == "medium", 2, 3))

# 4
Medium_Left = subset(HR, left == 1 & salary_New == 2)

# 5
SS = ddply(Medium_Left,
           c("sales"), summarise,
           Time_spend_Mean = mean(time_spend_company))






