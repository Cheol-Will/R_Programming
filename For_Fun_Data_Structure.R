df4A = read.delim("C:/Rstudy/R Programming/datastructure4A.txt", header = T)
df4B = read.delim("C:/Rstudy/R Programming/datastructure4B.txt", header = T)

df <- c(df4A$Total, df4B$Total)
df$Total
mean(df)
median(df)
