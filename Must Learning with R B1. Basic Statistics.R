library(ggplot2)

# 난수 생성
# 확률 변수 생성
RB = rbinom(n = 400, size = 1, prob = 0.6)
RB
ggplot(NULL)+
  geom_bar(aes(x = as.factor(RB), fill = as.factor(RB))) + 
  theme_bw() +
  xlab("") + ylab("") +
  scale_x_discrete(labels = c("실패","성공")) +
  theme(legend.position = 'none')  

# 이항분포에 관하여

# 난수 생성
# 사건의 발생확률 생성
X = c()
P = c()

for(k in 1:10){
  
  RDB = dbinom(x = k, size = 10,prob = 0.4)
  
  X = c(X,k)
  P = c(P,RDB)
  
}

ggplot(NULL) +
  geom_bar(aes(x = X, y = P),stat = 'identity') +
  theme_bw() +
  scale_x_continuous(breaks = seq(1,10)) +
  xlab("성공횟수") + ylab("확률")

X = c()
P = c()


for(k in 1:10){
  
  RDB = dbinom(x = k, size = 10,prob = 0.8)
  
  X = c(X,k)
  P = c(P,RDB)
  
}

# 이항분포
ggplot(NULL) +
  geom_bar(aes(x = X, y = P),stat = 'identity') +
  theme_bw() +
  scale_x_continuous(breaks = seq(1,10)) +
  xlab("성공횟수") + ylab("확률")


# 다항분포
rmultinom(n = 1,size = 100,prob = c(0.2,0.5,0.3))
t(rmultinom(n = 1,size = 10,prob = c(0.2,0.5,0.3)))
RM = as.data.frame(t(rmultinom(n = 1,size = 10,prob = c(0.2,0.5,0.3))))
RM = colSums(RM)
ggplot(NULL) +
  geom_bar(aes(x = names(RM), y= RM,fill = names(RM)),stat = 'identity') +
  theme_bw() +
  theme(legend.position = 'none') +
  scale_x_discrete(labels = c("1","2","3")) +
  xlab("") + ylab("")


# 포아송 분포
RP = rpois(n = 100 ,lambda = 2)

ggplot(NULL) +
  geom_bar(aes(x = as.factor(RP),fill = as.factor(RP))) +
  theme_bw() +
  xlab("성공횟수") + ylab("빈도") +
  theme(legend.position = 'none')
# 포아송 분포에서 평균이 20일 때 x가 15이하일 확률
ppois(q = 15, lambda = 20, lower.tail = T)
