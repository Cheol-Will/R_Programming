HR <- read.csv('data/HR_comma_sep.csv')
View(HR)

# 변수에 대한 요약 값 살펴보기
summary(HR$salary)
summary(HR$satisfaction_level)

# 분위수 계산
quantile(HR$satisfaction_level, probs = c(0.1, 0.3, 0.6, 0.9))

# 합, 평균, 표준편차
sum(HR$satisfaction_level)
mean(HR$last_evaluation)
sd(HR$satisfaction_level)

# 다중 변수의 합, 평균 구하기
colMeans(HR[1:5])
colSums(HR[1:5])

# 빈도 테이블 작성하기
TABLE = as.data.frame(table(HR$sales))
View(TABLE)

TABLE2 = as.data.frame(xtabs(~ HR$salary + HR$sales))
View(TABLE2)

# Exercise
mean(HR$last_evaluation)

sd(HR$last_evaluation)

table(HR$sales)

as.data.frame(table(HR$sales))
TABLE = as.data.frame(xtabs(~ HR$left + HR$salary))
View(TABLE)
