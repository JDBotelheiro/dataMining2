library(fpc) 
library(cluster)
library(factoextra)
library(pROC)

dados<-classescute
View(dados)

dados2 <- dados[,1:21]
View(dados2)

dados3<-read.table("classescute.txt",header=TRUE)
View(dados3)

library(cluster)
dados_eu<-agnes(dados2,  metric = "euclidean",
                stand = FALSE, method = "complete", keep.data = FALSE)

m<-mean(dados_eu$height) #10.25208
m
s<-sd(dados_eu$height) #22.95983
s
limsup<-m+1.25*s #38.95186
limsup
lx<-cutree(as.hclust(dados_eu),k=NULL,h=limsup)
table(lx) #suggests 196 clusters, and the data are assigned to each cluster by lx

dados3$novodiag<-lx
View(dados3)
str(dados3)

dados<-dados3
dados$novodiag<-as.factor(dados$novodiag)
str(dados)

#Random training (70%) and test Data (30%)
set.seed(432016) # setting a seed to  get the same sequence of random numbers (for replication)
m <- nrow(dados)
imp <- sample(1:m, 0.70*m, prob = rep(1/m,m)) #create index for test and train samples
dados.train <- dados[imp,]
dados.test <- dados[-imp,]

#The metric used was the Minkowski distance

suppressWarnings(suppressMessages(library(kknn)))
model <- train.kknn(novodiag ~ ., data = dados.train, kmax = 20)
model
#Melhor k:7

prediction <- predict(model, dados.test[, -22])
prediction

CM <- table(dados.test[, 22], prediction)
CM

accuracy <- (sum(diag(CM)))/sum(CM)
accuracy <-100*accuracy
accuracy #47.11982

errorratio <- 100- accuracy
errorratio #52.88018

precision <- diag(CM) / colSums(CM)
precision <-100*precision
precision

recall <- diag(CM) / rowSums(CM)
recall<-100*recall
recall

f1 <-  ifelse(precision + recall == 0, 0, 2 * precision * recall / (precision + recall))
f1 

plot(model)

