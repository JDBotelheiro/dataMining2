#Complete linkage classes

dados<-read.table("classescute.txt",header=TRUE)
View(dados)

dados2 <- dados[,1:21]
View(dados2)

library(cluster)
dados_eu<-agnes(dados2,  metric = "euclidean",
                  stand = FALSE, method = "complete", keep.data = FALSE)

m<-mean(dados_eu$height) #10.25208
m
s<-sd(dados_eu$height) #22.95983
s
limsup<-m+1.25*s #38.95186
limsup
pltree(dados_eu,main="Complete linkage", cex=0.83,xlab="")
abline(h=limsup,col="green",lwd=2)
lx<-cutree(as.hclust(dados_eu),k=NULL,h=limsup)
table(lx) #suggests 196 clusters, and the data are assigned to each cluster by lx

dados_eu_2<-cutree(dados_eu,196)
table(dados_eu_2,dados[,22])

accuracy<-(sum(diag(table(dados_eu_2,dados[,22])))/nrow(dados))*100
accuracy #2.67%
errorratio<-100-accuracy
errorratio #97.33%

