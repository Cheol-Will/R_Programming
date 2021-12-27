library(tidyr)
library(data.table)
df = read.csv("C:/Rstudy/R Programming/Must_Learning_with_R_data/players_20.csv", header = TRUE, 
                stringsAsFactors = FALSE)
head(df)
str(df)

# only field players
df_ <- df[1:100, ]; row.names(df_) <- df_$short_name; head(df_)
FIFA_FIELD = subset(df_, df_$team_position != "GK "); FIFA_FIELD

# 100 players
FIFA_FIELD2 = FIFA_FIELD[1:100, c(2, 10:40, 42:48, 61:71)]; FIFA_FIELD2
FIFA_FIELD3 <- FIFA_FIELD2[, apply(is.na(FIFA_FIELD2), FUN = sum, MARGIN = 2) == 0]; FIFA_FIELD3

# standardization
SCALED = as.data.frame(scale(FIFA_FIELD3))


# 여기부터 내가 직접 데이터에 맞춤
df_ <- df[1:100, ]; row.names(df_) <- df_$short_name; head(df_)
mytype <- NULL
for(i in df_[1,]){
  mytype <- c(mytype, typeof(i) == "integer")
  print(typeof(i))
}

FIFA_NUM <- df_[, mytype]; str(FIFA_NUM)
FIFA_NUM1 <- FIFA_NUM[, apply(is.na(FIFA_NUM), FUN = sum, MARGIN = 2) == 0]; FIFA_NUM1
SCALED = as.data.frame(scale(FIFA_NUM1))


install.packages("corrplot")
library(corrplot)
library(RColorBrewer)
Corr_mat = cor(SCALED)
corrplot(Corr_mat, method = "color", outline = T, addgrid.col = "darkgray", 
         order = "hclust", addrect = 4, rect.col = "black", rect.lwd = 5, cl.pos = "b", 
         tl.col = "indianred4", tl.cex = 0.5, cl.cex = 0.5, addCoef.col = "white", 
         number.digits = 2, number.cex = 0.3, col = colorRampPalette(c("darkred", 
                                                                       "white", "midnightblue"))(100))

install.packages("factoextra")
install.packages("FactoMineR")
library(factoextra)
library(FactoMineR)


# PCA 실행
Principal_Component = PCA(SCALED[1:100,],graph = FALSE)
Principal_Component$eig[1:5,]
Principal_Component$eig

# 누적 variance
fviz_screeplot(Principal_Component, addlabels = TRUE, ylim = c(0, 50))

# Dim.1은 제 1주성분(PC1)을 의미합니다.
Principal_Component$var$coord[1:34, ]
sort(Principal_Component$var$coord[, 1])


# bi-plot
fviz_pca_var(Principal_Component, 
             col.var = "contrib", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE) 
fviz_pca_biplot(Principal_Component, repel = FALSE)













