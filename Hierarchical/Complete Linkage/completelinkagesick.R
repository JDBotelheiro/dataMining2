#Complete linkage sick - usar apenas quando o complete linkage classes dizer esta doente

dados<-sickclasses
View(dados)

dados2 <- dados[,1:21]
View(dados2)

library(cluster)
dados_eu<-agnes(dados2,  metric = "euclidean",
                stand = FALSE, method = "complete", keep.data = FALSE)

write.table(dados, file="completesick.txt")

m<-mean(dados_eu$height) #20.35049
m
s<-sd(dados_eu$height) #40.66408
s
limsup<-m+1.25*s #71.18059
limsup
pltree(dados_eu,main="Complete linkage sick", cex=0.83,xlab="")
abline(h=limsup,col="green",lwd=2)
lx<-cutree(as.hclust(dados_eu),k=NULL,h=limsup)
table(lx) #suggests 59 clusters, and the data are assigned to each cluster by lx

dados_eu_2<-cutree(dados_eu,59)
table(dados_eu_2,dados[,22])

accuracy<-(sum(diag(table(dados_eu_2,dados[,22])))/nrow(dados))*100
accuracy #3.516348
errorratio<-100-accuracy
errorratio #96.48365
