library(rpart)

dados <- completecluster
str(dados)
dados$novodiag <- as.factor(dados$novodiag)
str(dados)

tree.1 <- rpart(novodiag ~ Age + Sex + On.thyroxine + Query.on.thyroxine + On.antithyroid.medication + Sick + Pregnant + Thyroid.surgery + I131.treatment + Query.hypothyroid + Query.Hiperthyroid + Lithium + Goitre + Tumor + Hypopituitary + Psych + TSH + T3 + TT4 + T4U + FTI, data=dados, method="class")
tree.1

#Ploting the tree
plot(tree.1)
text(tree.1, pretty=0)

library(rpart.plot)

prp(tree.1,type = 4, extra = 3, col="darkgreen",split.col="deeppink")

#Trying to find the tree size that minimizes misclassification rate.
printcp(tree.1)

bestcp <- tree.1$cptable[which.min(tree.1$cptable[,"xerror"]),"CP"]

# We want the cp value (with a simpler tree) that minimizes the xerror
# (cross-validated error and use it to prune the tree). 
#cp is the complexity parameter and is the amount by which splitting that
#node improved the relative error.

#The value of cp should be least, so that the cross-validated error rate
#is minimum.

#Plotcp() provides a graphical representation to the cross validated error 
#summary. The cp values are plotted against the geometric mean to depict 
#the deviation until the minimum value is reached.

plotcp(tree.1)
tree1.pruned <- prune(tree.1, cp = bestcp)


#Ploting the pruned tree (the tree is the same)
prp(tree1.pruned,type = 4, extra = 3, col="darkgreen",split.col="deeppink")

#Confusion matrix
conf.matrix <- table(dados$novodiag , predict(tree1.pruned,type="class"))
print(conf.matrix)

accuracy <- (sum(diag(conf.matrix)))/sum(conf.matrix)
accuracy <- 100*accuracy
accuracy #34.68718

errorratio <- 100- accuracy
errorratio #65.31282

precision <- diag(conf.matrix) / colSums(conf.matrix)
precision

recall <- diag(conf.matrix) / rowSums(conf.matrix)
recall

f1 <-  ifelse(precision + recall == 0, 0, 2 * precision * recall / (precision + recall))
f1
