library(fpc)
library(dbscan)
library(factoextra)
dados <- read.table("classescute.txt")
View(dados)
dados2 <- dados[,1:21]
View(dados2)

dados2$Age <- as.numeric(as.factor(dados2$Age))
dados2$Sex <- as.numeric(as.factor(dados2$Sex))
dados2$On.thyroxine <- as.numeric(as.factor(dados2$On.thyroxine))
dados2$Query.on.thyroxine <- as.numeric(as.factor(dados2$Query.on.thyroxine))
dados2$On.antithyroid.medication <- as.numeric(as.factor(dados2$On.antithyroid.medication))
dados2$Sick <- as.numeric(as.factor(dados2$Sick))
dados2$Pregnant <- as.numeric(as.factor(dados2$Pregnant))
dados2$Thyroid.surgery <- as.numeric(as.factor(dados2$Thyroid.surgery))
dados2$I131.treatment <- as.numeric(as.factor(dados2$I131.treatment))
dados2$Query.hypothyroid <- as.numeric(as.factor(dados2$Query.hypothyroid))
dados2$Query.Hiperthyroid <- as.numeric(as.factor(dados2$Query.Hiperthyroid))
dados2$Lithium <- as.numeric(as.factor(dados2$Lithium))
dados2$Goitre <- as.numeric(as.factor(dados2$Goitre))
dados2$Tumor <- as.numeric(as.factor(dados2$Tumor))
dados2$Hypopituitary <- as.numeric(as.factor(dados2$Hypopituitary))
dados2$Psych <- as.numeric(as.factor(dados2$Psych))
dados2$TSH <- as.numeric(as.factor(dados2$TSH))
dados2$T3 <- as.numeric(as.factor(dados2$T3))
dados2$TT4 <- as.numeric(as.factor(dados2$TT4))
dados2$T4U <- as.numeric(as.factor(dados2$T4U))
dados2$FTI <- as.numeric(as.factor(dados2$FTI))

dbscan::kNNdistplot(dados2, k =  3)
abline(h = 25 , lty = 2) #eps=25 eh o valor otimal quando minpoints=3

set.seed(123)
# fpc package
res.fpc <- fpc::dbscan(dados2, eps = 25, MinPts = 3)
res.fpc
# dbscan package
res.db <- dbscan::dbscan(dados2, 25, 3)
res.db

#Plot of clusters
#fviz_cluster(res.fpc, dados2, geom = "point", xlab = "x value", ylab = "y value")

#Comparison between the obtained partitions with the true class each object belongs to
CM <- table(res.fpc$cluster,dados$novodiag)
CM

accuracy <- sum(diag(CM))/nrow(dados)
accuracy <-100*accuracy
accuracy

errorratio <- 100- accuracy
errorratio

precision <- diag(CM) / colSums(CM)
precision

recall <- diag(CM) / rowSums(CM)
recall

f1 <-  ifelse(precision + recall == 0, 0, 2 * precision * recall / (precision + recall))
f1

dados$novodiag <- NULL
dados$cluster <- res.fpc$cluster
str(dados)
dados$cluster <- as.factor(dados$cluster)
str(dados)
write.table(dados, file = "dados_clusters_dbscan.txt")

