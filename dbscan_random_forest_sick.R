library(pROC)
library(ggplot2)
library(plyr)
library(gam)
library(reshape)
library(randomForest)
library(MASS)
dados <- read.table("sick_clusters_dbscan.txt")
str(dados)
dados <- dados[which(!dados$cluster==0),]
str(dados)
dados$cluster <- as.factor(dados$cluster)
str(dados)

#Random training (70%) and test Data (30%)
set.seed(432016) # setting a seed to  get the same sequence of random numbers (for replication)
m <- nrow(dados)
imp <- sample(1:m, 0.70*m, prob = rep(1/m,m)) #create index for test and train samples
dados.train <- dados[imp,]
dados.test <- dados[-imp,]

f <- as.formula(paste('cluster ~', paste(colnames(dados.train)[1:21], collapse='+')))
rf <- randomForest(f, dados.train, importance = TRUE)

prediction <- predict(rf,dados.test[,-22])
conf_matrix <- table(dados.test[,22], prediction)
conf_matrix

pred.my <- as.numeric(prediction)
table(pred.my,dados.test[,22])

accuracy <- (sum(diag(conf_matrix)))/sum(conf_matrix)
accuracy <-100*accuracy
accuracy

errorratio <- 100- accuracy
errorratio

precision <- diag(conf_matrix) / colSums(conf_matrix)
precision

recall <- diag(conf_matrix) / rowSums(conf_matrix)
recall

f1 <-  ifelse(precision + recall == 0, 0, 2 * precision * recall / (precision + recall))
f1
