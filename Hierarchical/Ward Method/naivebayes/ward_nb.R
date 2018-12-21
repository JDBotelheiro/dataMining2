library(fpc) 
library(cluster)
library(factoextra)
library(pROC)
library(factoextra)
library(pROC)
library(e1071)

dados<-classescute
View(dados)

dados2 <- dados[,1:21]
View(dados2)

dados3<-classescute
View(dados3)

library(cluster)
dados_eu<-agnes(dados2,  metric = "euclidean",
                stand = FALSE, method = "ward", keep.data = FALSE)

m<-mean(dados_eu$height) #17.29214
m
s<-sd(dados_eu$height) #85.01628
s
limsup<-m+1.25*s #123.5625
limsup
lx<-cutree(as.hclust(dados_eu),k=NULL,h=limsup)
table(lx) #suggests 97 clusters, and the data are assigned to each cluster by lx

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

#Fitting the Naive Bayes model
Naive_Bayes_Model=naiveBayes(novodiag ~., dados.train)

prediction <- predict(Naive_Bayes_Model,dados.test[,-22])
conf_matrix <- table(dados.test[,22], prediction)
conf_matrix

accuracy <- (sum(diag(conf_matrix)))/sum(conf_matrix)
accuracy <-100*accuracy
accuracy #74.13594

errorratio <- 100- accuracy
errorratio #25.86406

precision <- diag(conf_matrix) / colSums(conf_matrix)
precision 

recall <- diag(conf_matrix) / rowSums(conf_matrix)
recall

f1 <-  ifelse(precision + recall == 0, 0, 2 * precision * recall / (precision + recall))
f1
