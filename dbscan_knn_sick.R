library(pROC)
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

#The metric used was the Minkowski distance

suppressWarnings(suppressMessages(library(kknn)))
model <- train.kknn(cluster ~ ., data = dados.train, kmax = 20)
model

prediction <- predict(model, dados.test[,-22])
prediction

CM <- table(dados.test[,22], prediction)
CM

pred.my <- as.numeric(prediction)
table(pred.my,dados.test[,22])

accuracy <- (sum(diag(CM)))/sum(CM)
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
