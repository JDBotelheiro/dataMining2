#Single Sick Linkage

dados<-sickclasses
View(dados)

dados2 <- dados[,1:21]
View(dados2)

library(cluster)
dados_eu<-agnes(dados2,  metric = "euclidean",
                stand = FALSE, method = "single", keep.data = FALSE)

m<-mean(dados_eu$height) #9.407967
m
s<-sd(dados_eu$height) #8.325131
s
limsup<-m+1.25*s #19.81438
limsup
pltree(dados_eu,main="Single Sick Linkage", cex=0.83,xlab="")
abline(h=limsup,col="green",lwd=2)
lx<-cutree(as.hclust(dados_eu),k=NULL,h=limsup)
table(lx) #suggests 90 clusters, and the data are assigned to each cluster by lx

dados_eu_2<-cutree(dados_eu,90)
table(dados_eu_2,dados[,22])

accuracy<-(sum(diag(table(dados_eu_2,dados[,22])))/nrow(dados))*100
accuracy #5.490438
errorratio<-100-accuracy
errorratio #94.50956

dados$novodiag<-lx
write.table(dados, file="singlesickcluster.txt")
