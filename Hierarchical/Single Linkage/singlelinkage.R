#Single Linkage

dados<-read.table("classescute.txt",header=TRUE)
View(dados)

dados2 <- dados[,1:21]
View(dados2)

library(cluster)
dados_eu<-agnes(dados2,  metric = "euclidean",
                stand = FALSE, method = "single", keep.data = FALSE)

m<-mean(dados_eu$height) #4.961348
m
s<-sd(dados_eu$height) #5.177504
s
limsup<-m+1.25*s #11.43323
limsup
pltree(dados_eu,main="Single Linkage", cex=0.83,xlab="")
abline(h=limsup,col="green",lwd=2)
lx<-cutree(as.hclust(dados_eu),k=NULL,h=limsup)
table(lx) #suggests 310 clusters, and the data are assigned to each cluster by lx

dados_eu_2<-cutree(dados_eu,310)
table(dados_eu_2,dados[,22])

accuracy<-(sum(diag(table(dados_eu_2,dados[,22])))/nrow(dados))*100
accuracy #1.054269
errorratio<-100-accuracy
errorratio #98.94573

dados$novodiag<-lx
write.table(dados, file="singlecluster.txt")
