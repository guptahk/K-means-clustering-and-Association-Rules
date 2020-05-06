data <- read.csv("europeanJobs.txt", sep="\t")

set.seed(12354675)
jobs_data <- read.table("europeanJobs.txt", header = TRUE)
rownames(jobs_data) <- jobs_data$Country
# randomly select 90% of the data
index <- sample(nrow(data), size = 0.8*nrow(data))
subset <- data[index,]
str(data)
library(factoextra)
#distance matrix
distance <- get_dist(jobs_data)
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

library(cluster)  
k2 <- kmeans(subset[,-1], centers = 2, nstart = 25)
k3 <- kmeans(subset[,-1], centers = 3, nstart = 25)
k4 <- kmeans(subset[,-1], centers = 4, nstart = 25)
k5 <- kmeans(subset[,-1], centers = 5, nstart = 25)
k6<- kmeans(subset[,-1], centers = 6, nstart = 25)
library(ggplot2)
# plots to compare
p1 <- fviz_cluster(k2, geom = "point",  data = subset[,-1]) + ggtitle("k = 2")
p2 <- fviz_cluster(k3, geom = "point",  data = subset[,-1]) + ggtitle("k = 3")
p3 <- fviz_cluster(k4, geom = "point",  data = subset[,-1]) + ggtitle("k = 4")
p4 <- fviz_cluster(k5, geom = "point",  data = subset[,-1]) + ggtitle("k = 5")
p5<- fviz_cluster(k6, geom = "point",  data = subset[,-1]) + ggtitle("k = 6")

grid.arrange(p1, p2, p3, p4,p5, nrow = 3)

##Determinging Number of clusters
# within cluster sum of squares
wss <- c()

for (i in 1:12) 
{
  # calculating total wss for each cluster size with 25 random iterations for each
  wss[i] <- sum(kmeans(subset[,-1], centers=i, nstart = 25)$withinss)
}

# plotting wss vs number of clusters
plot(1:12, wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")

##Plotting R-Square vs Cluster Size. Cluster size = 3 seems to be appropriate.
# r-square
r_square <- c()

for (i in 1:12) 
{
  # calculating total between sum square for each cluster size
  bss <- sum(kmeans(subset[,-1], centers=i, nstart = 25)$betweenss)
  
  # calculating total sum square for each cluster size
  tss <- sum(kmeans(subset[,-1], centers=i, nstart = 25)$totss)
  
  r_square[i] <- bss/tss
}

# plotting wss vs number of clusters
plot(1:12, r_square, type="b", xlab="Number of Clusters",ylab="R-Square")


kmeans_clustering <- kmeans(subset[,-1], centers=3, nstart=25)
fviz_cluster(kmeans_clustering, data = subset[,-1])

library(knitr)
centers_df <- round(as.data.frame(kmeans_clustering$centers))
centers_df$cluster <- rownames(centers_df)
centers_df <- select(centers_df, cluster, everything())
kable(centers_df)


library(data.table)
library(gridExtra)
library(factoextra)
library(tidyverse)
subset$cluster <- kmeans_clustering$cluster

subset$cluster <- kmeans_clustering$cluster
kable(arrange(subset, cluster))

# Calculate the distance matrix
distance <- dist(subset[,-1])

#Obtain clusters using the Wards method
hierarchical_clustering <- hclust(distance, method="ward.D")

plot(hierarchical_clustering)
#Cut dendrogram at the 3 clusters level and obtain cluster membership
hierarchical_clustering_3_clusters = cutree(hierarchical_clustering,k=3)

subset$cluster <- hierarchical_clustering_3_clusters

subset[,-1] %>%
  group_by(cluster) %>% 
  summarise_all(funs(mean)) %>% 
  round() %>% 
  kable()


######Cincinnati Zoo###########

Food <- read.csv('food.csv',sep=",")
Food <- Food[, -1]
# Find out elements that are not equal to 0 or 1 and change them to 1.
Others <- which(!(as.matrix(Food) ==1 | as.matrix(Food) ==0), arr.ind=T )
Food[Others] <- 1
Food <- as(as.matrix(Food), "transactions")

library(data.table)
library(tidyverse)
library(knitr)
library(arules)
library(arulesViz)

summary(Food)

itemFrequencyPlot(Food, support = 0.1, cex.names=0.8)

x = Food[size(Food) > 13]
inspect(x)
library(arules);
search() 
unloadNamespace("arules") 
unloadNamespace("arulesViz") 
update.packages("arules") 
library(arulesViz)
library(arules)
# Run the apriori algorithm
basket_rules <- apriori(Food,parameter = list(sup = 0.003, conf = 0.9,target="rules"))
summary(basket_rules)
inspect(basket_rules)
inspect(subset(basket_rules, size(basket_rules)>3))
inspect(subset(basket_rules, lift>8))
French.Fries.BasketFood.rhs <- subset(basket_rules, subset = rhs %in% "French.Fries.BasketFood" & lift>8)

inspect(French.Fries.BasketFood.rhs)
plot(basket_rules)
plot(basket_rules, method="grouped")
plot(head(sort(basket_rules, by="lift"), 8), method = "graph")
