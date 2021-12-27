state <- read.csv(file = "Introduction to Statistical Programming/practical_statistic_data/state.csv")
View(state)
str(state)

library(dplyr)
library(tidyr)
library(ggplot2)

## 대표값 계산
## 평균과 중간값
mean(state$Population)
mean(state$Population, trim = 0.1) # 절사평균으로 앞 뒤 0.1의 비율 제거
median(state[["Population"]])

# Population 가중치로 살인률 계산
weighted.mean(state[["Murder.Rate"]], w = state[["Population"]])

library("matrixStats")
weightedMedian(state$Murder.Rate, w = state$Population)

## Continuous에 관하여
## 산포도에 관하여
## 분산, 사분위범위, 중위절대편차
sd(state$Population)
IQR(state[["Population"]]) # 0.75quantile - 0.25quantile
mad(state$Population) # 중간값 -> 편차 -> 절대값


## 데이터의 분포에 관하여
## 분산, boxplot, hist, kde, quantile
sd(state$Population)
IQR(state$Population) # 0.75quantile - 0.25quantile
mad(state$Population) # 중앙값 편차 -> 절대값

# Percentiles and Boxplots
quantile(state$Murder.Rate, p = c(.05, .25, .5, .75, .95))
boxplot(state$Population/1000000, ylab = "Population (millions)")
ax.set_ylabel("Population (millions)")

# Frequency Table and Histograms
breaks <- seq(from = min(state$Population),
              to = max(state$Population), length = 11)
hist(state$Population, breaks = breaks)

# Density Estimates
# Density is an alternative to histograms that can provide more insight into the distribution of the data points.

hist(state$Murder.Rate, freq = F)
lines(density(state$Murder.Rate), lwd = 3, col = "blue")


## Binary and Categorical에 관하여
## 대표값
## mode, expected value, bar chart, pie chart
dfw = read.csv(file = "Introduction to Statistical Programming/practical_statistic_data/dfw_airline.csv")
str(dfw)
barplot(as.matrix(dfw)/6, cex.axis = 0.8, cex.names = 0.7,
        xlab = "Cause of Delay", ylab = "Count")

## correlation coefficient
sp500_px = read.csv(file = "Introduction to Statistical Programming/practical_statistic_data/sp500_data.csv.gz")
sp500_sym <- read.csv(file = "Introduction to Statistical Programming/practical_statistic_data/sp500_sectors.csv")
etfs <- sp500_px[row.names(sp500_px) > '2012-07-01', 
                 sp500_sym[sp500_sym$sector == 'etf', 'symbol']]
str(etfs)
library(corrplot)
corrplot(cor(etfs), method = "ellipse")

## scatter plot
telecom <- sp500_px[row.names(telecom) > '2012-07-01',
                    sp500_sym[sp500_sym$sector == 'telecommunications_services', 'symbol']]
telecom <- telecom[,]
telecom_cor <- cor(telecom)

plot(telecom$HBAN, telecom$CCL, xlab = "ATT (T)", ylab = "verizon (VZ)")


## Hexagonal binning and Contours 
kc_tax =  read.csv(file = "Introduction to Statistical Programming/practical_statistic_data/kc_tax.csv.gz")
kc_tax0 <- subset(kc_tax, TaxAssessedValue < 750000 & 
                    SqFtTotLiving > 100 &
                    SqFtTotLiving < 3500)
nrow(kc_tax0)

ggplot(kc_tax0, (aes(x = SqFtTotLiving, y = TaxAssessedValue))) + 
  stat_binhex(color = "white") + 
  theme_bw() + 
  scale_fill_gradient(low = "white", high = "blue") +
  labs(x = "Finished Square Feet", y = "Tax-Assessed Value")

ggplot(kc_tax0, aes(SqFtTotLiving, TaxAssessedValue)) +
  theme_bw() + 
  geom_point(color='blue', alpha=0.1) + 
  geom_density2d(color='white') + 
  labs(x='Finished Square Feet', y='Tax-Assessed Value')

## 

lc_loans = read.csv(file = "Introduction to Statistical Programming/practical_statistic_data/lc_loans.csv")
library(descr)
CrossTable(lc_loans$grade, lc_loans$status,
           prop.c = F, prop.chisq = F, prop.t = F)

airline_stats <- read.csv(file = "Introduction to Statistical Programming/practical_statistic_data/airline_stats.csv", stringsAsFactors = F)
airline_stats$airline <- ordered(airline_stats$airline, 
                                 levels=c('Alaska', 'American', 'Jet Blue', 'Delta', 'United', 'Southwest'))

boxplot(pct_carrier_delay ~ airline, data = airline_stats, ylim = c(0, 50))
ggplot(data=airline_stats, aes(airline, pct_carrier_delay)) + 
  ylim(0, 50) + 
  geom_violin(draw_quantiles = c(.25,.5,.75), linetype=2) +
  geom_violin(fill=NA, size=1.1) +
  theme_bw()

ggplot(subset(kc_tax0, ZipCode %in% c(98188, 98105, 98108, 98126)),
       aes(x=SqFtTotLiving, y=TaxAssessedValue)) + 
  stat_binhex(colour='white') + 
  theme_bw() + 
  scale_fill_gradient(low='gray95', high='black') +
  labs(x='Finished Square Feet', y='Tax-Assessed Value') +
  facet_wrap('ZipCode')



