#Cluster Analysis
library(readxl)
set.seed(5)
#FILTREI SOMENTE OS PROCESSADOS
cluster <- read_excel("~/Desktop/cluster.xlsx", sheet = "cluster")
head(cluster)
#View(cluster)

#tirando o código do produto e o nome da categoria (deixando apenas o número da categoria, legenda no excel)
cluster1 <- cluster[,-c(1,5)]
head(cluster1)
#View(cluster1)

#Cluster 1 Pre-processing
summary(cluster1)

#boxplot(cluster1$`Sum of quantity`, main = "Boxplot sum of quantity")

#mean(cluster1$category1)
#sd(cluster1$category1)

cluster2 <- scale(cluster1) # [xi - mean(x)]/[sd(x)]
head(cluster2)
#View(cluster2)

euclid <- dist(cluster2, method = "euclidean")

#HIERARCHICAL
hierarchical <- hclust(euclid, method = "ward.D")

plot(hierarchical)

groups <- cutree(hierarchical, k = 4)

rect.hclust(hierarchical, k = 4, border = "red")

#K-MEANS
wss <- c(rep(0, nrow(cluster2)-1))
bss <- c(rep(0,nrow(cluster2)-1))

for (i in 1:(nrow(cluster2)-1)){
  kmeans_temp <- kmeans(cluster2, centers = i)
  wss[i] <- kmeans_temp$tot.withinss
  bss[i] <- kmeans_temp$betweenss
}

plot(1:(nrow(cluster2)-1), wss, type = "b",xlab = "Number of Clusters",
     ylab = "Distance", col = "blue")

par(new = T)

plot(1:(nrow(cluster2)-1), bss, type = "b", axes = F, xlab = "", ylab = "",
     col = "dark red")

#It plateaus after k = 4
par(new = F)

partition <- kmeans(cluster2, 4)

centers <- partition$centers
centers

#DESCALING COLUMNS
# x = column number, y = column reference name
descale <- function(x,y){
  for (i in 1:nrow(centers)){
    centers[i,x] <<- centers[i,x] * sd(y) + mean(y) 
  } 
}

descale(1, cluster1$`Sum of quantity`)
descale(2, cluster1$`Sum of price`)
descale(3, cluster1$liquid_cost)
descale(4, cluster1$category1)
centers 

library(cluster)
euclidean_sq <- euclid * euclid
plot(silhouette(partition$cluster, euclidean_sq))

cluster <- cbind(cluster, membership = partition$cluster)
head(cluster)

write.csv(cluster,file="ProductMembership.csv")
