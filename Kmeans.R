
library(stringr)
library(e1071)
library(mlr)
library(caret)
library(dplyr)
library(mlbench)
library(cluster)
library(factoextra)
library(cluster)
library(plyr)
library(gam)
library(reshape)
library(randomForest)
library(MASS)
set.seed(415)

dados <- read.table("classescute.txt")
true.class <- dados[,22]
teste <- do.call(cbind, lapply(dados[, 2:16], as.numeric))
dadosF <- data.frame(dados[,1], teste, dados[,17:21])	
dados.new <- scale(dadosF[,1:21])
set.seed(2018)
result <- kmeans(dados.new, centers=9)
result$size
result$centers
result$cluster
table(result$cluster, true.class)
rc <- result$cluster

#random Forest
traiin <- data.frame(dadosF, rc)
traiin <- na.omit(traiin)
trainIdx <- sample(nrow(traiin), 1120)
train <-traiin[trainIdx, ]
test <-traiin[-trainIdx, ]
diagnosticoteste <- test[,22]
rf <- randomForest(traiin[,22]~., traiin[,1:21], ntree= 500, na.action=na.exclude)
resultsRF <- predict(rf, test[,1:21])
data.frame(resultsRF, test[,22])
CM <- table(resultsRF,test[,22])

accuracy <- (sum(diag(CM)))/sum(CM)
accuracy <-100*accuracy
accuracy

errorratio <- 100- accuracy
0.01*errorratio

precision <- diag(CM) / colSums(CM)
precision <-100*precision
round(0.01*precision, digits=3) 

recall <- diag(CM) / rowSums(CM)
recall<-100*recall
round(0.01*recall, digits=3) 

f1 <-  ifelse(precision + recall == 0, 0, 2 * precision * recall / (precision + recall))
round(0.01*f1 , digits=3) 




#naiveBayes
traiin <- data.frame(dadosF, rc)
traiin <- na.omit(traiin)
trainIdx <- sample(nrow(traiin), 1120)
train <-traiin[trainIdx, ]
Diagnostico <- train[,22]
test <-traiin[-trainIdx, ]
diagnosticoteste <- test[,22]
Naive_Bayes_Model<- naiveBayes(as.factor(Diagnostico)~.,train[,1:21])
NB_Predictions=predict(Naive_Bayes_Model, test[,1:21])
NB_Predictions <- as.factor(NB_Predictions)
diagnosticoteste <- as.factor(diagnosticoteste)
CM <- table(diagnosticoteste, NB_Predictions)

accuracy <- (sum(diag(CM)))/sum(CM)
accuracy <-100*accuracy
accuracy

errorratio <- 100- accuracy
0.01*errorratio

precision <- diag(CM) / colSums(CM)
precision <-100*precision
round(0.01*precision, digits=3) 

recall <- diag(CM) / rowSums(CM)
recall<-100*recall
round(0.01*recall, digits=3) 

f1 <-  ifelse(precision + recall == 0, 0, 2 * precision * recall / (precision + recall))
round(0.01*f1 , digits=3) 







#DecisionTrees
library(rpart)
library(rpart.plot)

traiin <- data.frame(dadosF, rc)
traiin <- na.omit(traiin)
trainIdx <- sample(nrow(traiin), 1120)
train <-traiin[trainIdx, ]
Diagnostico <- train[,22]
test <-traiin[-trainIdx, ]
diagnosticoteste <- test[,22]
decisionTree <- rpart(Diagnostico~., train[,1:21], method = 'class')
dtPred <- predict(decisionTree, test[,1:21], type = 'class')
CM <- table(as.factor(diagnosticoteste), dtPred)

 
accuracy <- (sum(diag(CM)))/sum(CM)
accuracy <-100*accuracy
accuracy

errorratio <- 100- accuracy
0.01*errorratio

precision <- diag(CM) / colSums(CM)
precision <-100*precision
round(0.01*precision, digits=3) 

recall <- diag(CM) / rowSums(CM)
recall<-100*recall
round(0.01*recall, digits=3) 

f1 <-  ifelse(precision + recall == 0, 0, 2 * precision * recall / (precision + recall))
round(0.01*f1 , digits=3) 






#KNN
install.packages(“class”)
library(class)
traiin <- data.frame(dadosF, rc)
traiin <- na.omit(traiin)
trainIdx <- sample(nrow(traiin), 1120)
train <-traiin[trainIdx, ]
Diagnostico <- train[,22]
test <-traiin[-trainIdx, ]
diagnosticoteste <- test[,22]
knnPred <- knn(train = train[,1:21] , test = test[,1:21],cl = Diagnostico, k=10)

accuracy <- (sum(diag(CM)))/sum(CM)
accuracy <-100*accuracy
accuracy

errorratio <- 100- accuracy
0.01*errorratio

precision <- diag(CM) / colSums(CM)
precision <-100*precision
round(0.01*precision, digits=3) 

recall <- diag(CM) / rowSums(CM)
recall<-100*recall
round(0.01*recall, digits=3) 

f1 <-  ifelse(precision + recall == 0, 0, 2 * precision * recall / (precision + recall))
round(0.01*f1 , digits=3) 





