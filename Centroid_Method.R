library(fpc)
library(cluster)
library(mclust)
library(NbClust)

#loading data
data = read.table("/Users/Joao/Desktop/Projecto II/classescute.txt")
data = na.omit(data)
View(data)

data$Age <- as.numeric(data$Age)
data$Sex <- as.numeric(data$Sex)
data$On.thyroxine <- as.numeric(data$On.thyroxine)
data$Query.on.thyroxine <- as.numeric(data$Query.on.thyroxine)
data$On.antithyroid.medication <- as.numeric(data$On.antithyroid.medication)
data$Sick <- as.numeric(data$Sick)
data$Pregnant <- as.numeric(data$Pregnant)
data$Thyroid.surgery <- as.numeric(data$Thyroid.surgery)
data$I131.treatment <- as.numeric(data$I131.treatment)
data$Query.hypothyroid <- as.numeric(data$Query.hypothyroid)
data$Query.Hiperthyroid <- as.numeric(data$Query.Hiperthyroid)
data$Lithium <- as.numeric(data$Lithium)
data$Goitre <- as.numeric(data$Lithium)
data$Tumor <- as.numeric(data$Tumor)
data$Hypopituitary <- as.numeric(data$Hypopituitary)
data$Psych <- as.numeric(data$Psych)
data$TSH <- as.numeric(data$TSH)
data$T3 <- as.numeric(data$T3)
data$TT4 <- as.numeric(data$TT4)
data$T4U <- as.numeric(data$T4U)
data$FTI <- as.numeric(data$FTI)

true.class<- data$novodiag
data$novodiag <- NULL
str(data)

data.scale = scale(data)

#Hierarchical clustering
hclust.data = hclust(dist(data.scale), method = "centroid")

ks <- 2:20

#1 - applaying
ASW <- sapply(ks, FUN=function(k) {
  fpc::cluster.stats(dist(data.scale), cutree(hclust.data, k))$avg.silwidth
})
plot(ks, ASW, type="l")
abline(v=ks[which.max(ASW)], col="red", lty=2)
ks[which.max(ASW)]

#2 - applaying WSS
require(cluster)
fviz_nbclust(data.scale, hcut, method = "silhouette",hc_method = "complete")

#3 - applaying Gap Statistic
# Compute gap statistic
set.seed(123)
gap_stat <- clusGap(data.scale, FUN = hcut, K.max = 10, B = 50)
# Plot gap statistic
fviz_gap_stat(gap_stat)

hc_2 <- cutree(hclust.data, 2)
hc_15 <- cutree(hclust.data, 15)


entropy <- function(cluster, true.class) {
  k <- max(cluster, true.class)
  cluster <- factor(cluster, levels = 1:k)
  true.class<- factor(true.class, levels = 1:k)
  m <- length(cluster)
  mi <- table(cluster)
  
  cnts <- split(true.class, cluster)
  cnts <- sapply(cnts, FUN = function(n) table(n))
  p <- sweep(cnts, 1, rowSums(cnts), "/")
  p[is.nan(p)] <- 0
  e <- -p * log(p, 2)
  sum(rowSums(e, na.rm = TRUE) * mi/m)
}

purity <- function(cluster, true.class) {
  k <- max(cluster, true.class)
  cluster <- factor(cluster, levels = 1:k)
  true.class<- factor(true.class, levels = 1:k)
  m <- length(cluster)
  mi <- table(cluster)
  
  cnts <- split(true.class, cluster)
  cnts <- sapply(cnts, FUN = function(n) table(n))
  p <- sweep(cnts, 1, rowSums(cnts), "/")
  p[is.nan(p)] <- 0
  
  sum(apply(p, 1, max) * mi/m)
}


set.seed(123)
random.sample.2 <- sample(1:2, nrow(data.scale), replace = TRUE)
random.sample.15 <- sample(1:15, nrow(data.scale), replace = TRUE)

r <- rbind(
  hc1 = c(
    unlist(fpc::cluster.stats(dist(data.scale), hc_2, as.intenger(true.class), compareonly = TRUE)),
    entropy = entropy(hc_2, as.intenger(true.class)),
    purity = purity(hc_2, as.intenger(true.class))
  ),
  hc2 = c(
    unlist(fpc::cluster.stats(dist(data.scale), hc_15, as.intenger(true.class), compareonly = TRUE)),
    entropy = entropy(hc_15, as.intenger(true.class)),
    purity = purity(hc_15, as.intenger(true.class))
  ),
  random2 = c(
    unlist(fpc::cluster.stats(dist(data.scale), random.sample.2, as.intenger(true.class), compareonly = TRUE)),
    entropy = entropy(random.sample.2, as.intenger(true.class)),
    purity = purity(random.sample.2, as.intenger(true.class))
  ),
  random15 = c(
    unlist(fpc::cluster.stats(dist(data.scale), random.sample.15, as.intenger(true.class), compareonly = TRUE)),
    entropy = entropy(random.sample.15, as.intenger(true.class)),
    purity = purity(random.sample.15, as.intenger(true.class))
  )
)
r



CM <- table(hc_2,true.class)
CM

accuracy <- sum(diag(CM))/nrow(data)
accuracy <-100*accuracy
accuracy  #2.00%

errorratio <- 100- accuracy
errorratio

precision <- diag(CM) / colSums(CM)
precision

recall <- diag(CM) / rowSums(CM)
recall

f1 <-  ifelse(precision + recall == 0, 0, 2 * precision * recall / (precision + recall))
f1

dados$novodiag <- NULL
dados$cluster <- hc_dados2
str(dados)
dados$cluster <- as.factor(dados$cluster)
str(dados)
write.table(dados, file = "classes_clusters_centroid.txt")

