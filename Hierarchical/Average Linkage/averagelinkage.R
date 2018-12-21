#Average Linkage

dados<-read.table("classescute.txt",header=TRUE)
View(dados)

dados2 <- dados[,1:21]
View(dados2)

library(cluster)
dados_eu<-agnes(dados2,  metric = "euclidean",
                stand = FALSE, method = "average", keep.data = FALSE)

m<-mean(dados_eu$height) #7.639076
m
s<-sd(dados_eu$height) #12.8416
s
limsup<-m+1.25*s #23.69107
limsup
pltree(dados_eu,main="Average Linkage", cex=0.83,xlab="")
abline(h=limsup,col="green",lwd=2)
lx<-cutree(as.hclust(dados_eu),k=NULL,h=limsup)
table(lx) #suggests 230 clusters, and the data are assigned to each cluster by lx

dados_eu_2<-cutree(dados_eu,230)
table(dados_eu_2,dados[,22])

accuracy<-(sum(diag(table(dados_eu_2,dados[,22])))/nrow(dados))*100
accuracy #9.557553
errorratio<-100-accuracy
errorratio #90.44245

dados$novodiag<-lx
write.table(dados, file="averagecluster.txt")


