library(ggplot2)
loans_income <- read.csv('Introduction to Statistical Programming/practical_statistic_data/loans_income.csv')
View(loans_income)
samp_data <- data.frame(income = sample(loans_income, 1000, T), type = "data_dist")
samp_mean_05 <- data.frame(
  income = tapply(sample(loans_income, 1000*5, T),
                  rep(1:1000, rep(5, 1000)), FUN = mean),
  type = "mean_of_5")

# BootStrap  ----------------------------------------------
library(boot)
x <- seq(from=-3, to=3, length=300)
stat_fun <- function(x, idx) median(x[idx])
boot_obj <- boot(loans_income, R=100, statistic=stat_fun)

boot_obj

# Normal Dist  ----------------------------------------------
norm_samp <- rnorm(100)
qqnorm(norm_samp)
abline(a = 0, b = 1, col = "blue")

sp500_px <- read.csv('Introduction to Statistical Programming/practical_statistic_data/sp500_data.csv.gz')
View(sp500_px)
sp500_px <- diff(log(sp500_px[sp500_px>0]))
qqnorm(sp500_px)
abline(a = 0, b= 1, col = "grey")

# Binary Dist  ----------------------------------------------
dbinom(x = 2, size = 5, p = 0.1)
pbinom(2, 5, 0.1)

# Poisson Dist  ----------------------------------------------
rpois(100, lambda = 2)

# Exponential Dist  ----------------------------------------------
rexp(n = 100, rate = 0.2)



















