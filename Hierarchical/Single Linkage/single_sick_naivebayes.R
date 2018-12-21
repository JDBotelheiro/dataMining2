library(pROC)
library(e1071)
dados <- read.table("singlesickcluster.txt")
str(dados)
dados$novodiag <- as.factor(dados$novodiag)
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
accuracy

errorratio <- 100- accuracy
errorratio

precision <- diag(conf_matrix) / colSums(conf_matrix)
precision

recall <- diag(conf_matrix) / rowSums(conf_matrix)
recall

f1 <-  ifelse(precision + recall == 0, 0, 2 * precision * recall / (precision + recall))
f1
