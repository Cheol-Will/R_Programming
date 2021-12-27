session_times <- read.csv('Introduction to Statistical Programming/practical_statistic_data/web_page_data.csv')

View(session_times)

ggplot(session_times, aes(x=Page, y = Time)) + 
  geom_boxplot()

mean_a <- mean(session_times[session_times["Page"] == "Page A", "Time"])
mean_b <- mean(session_times[session_times["Page"] == "Page B", "Time"])
mean_b - mean_a

table(session_times["Page"])

perm_fun <- function(x, nA, nB)
{
  n <- nA + nB
  idx_b <- sample(1:n, nB) # Among 1~n, random
  idx_a <- setdiff(1:n, idx_b) # Among 1~n, not B
  mean_diff <- mean(x[idx_b]) - mean(x[idx_a])
  
  return(mean_diff)
}
perm_diffs <- rep(0, 100)
for(i in 1:100){
  perm_diffs[i] = perm_fun(session_times[, "Time"], 21, 15)
}
perm_diffs
hist(perm_diffs, xlab = "S T differences", breaks = 15)
abline(v = mean_b - mean_a)
