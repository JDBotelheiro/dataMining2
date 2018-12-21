#Ward Sick Method

dados<-sickclasses
View(dados)

dados2 <- dados[,1:21]
View(dados2)

library(cluster)
dados_eu<-agnes(dados2,  metric = "euclidean",
                stand = FALSE, method = "ward", keep.data = FALSE)

m<-mean(dados_eu$height) #17.29214
m
s<-sd(dados_eu$height) #85.01628
s
limsup<-m+1.25*s #1235625
limsup
pltree(dados_eu,main="Ward Sick Method", cex=0.83,xlab="")
abline(h=limsup,col="green",lwd=2)
lx<-cutree(as.hclust(dados_eu),k=NULL,h=limsup)
table(lx) #suggests 32 clusters, and the data are assigned to each cluster by lx

dados_eu_2<-cutree(dados_eu,32)
table(dados_eu_2,dados[,22])

accuracy<-(sum(diag(table(dados_eu_2,dados[,22])))/nrow(dados))*100
accuracy #3.763109
errorratio<-100-accuracy
errorratio #96.23689

dados$novodiag<-lx
write.table(dados,file="wardclustersick.txt")

