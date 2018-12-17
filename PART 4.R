library(cluster)
library(ggplot2)
library(dplyr)
library(radarchart)
library(tidyr)

features <- data.frame(drake_songs_similar[1:9])
# Calculate the avg silhouette for k = 2,... k = 10
avg.sil <- sapply(2:10, function(x) pam(features, x)$silinfo$avg.width)
avg.sil.df <- data.frame(k = 2:10, silhouette = avg.sil)

ggplot(avg.sil.df, aes(x = k, y = silhouette)) +
  geom_point() +
  geom_line() +
  ggtitle("Average Silhouette for k=2, ..., k=10")

# Gap Statistic
c <- clusGap(features, pam, 10, B = 100, verbose = interactive())
plot(c, main = "Gap statistic for k=2, ..., k=10")

main_clustering <- pam(features, 3, trace.lev = 1)
# Cluster information
print(main_clustering$clusinfo)
# Average silhouette per cluster
print(main_clustering$silinfo$avg.width)

main_clustering_silhouette <- silhouette(main_clustering)
clusplot(main_clustering, shade=TRUE, color=TRUE)
plot(main_clustering_silhouette, col='black', border='gray',
     main='Silhouette plot of the cluster')

#The items with negative silhouette are songs that are most probably assigned to the wrong cluster.
main_clustering$clustering
clustering_with_name <- data.frame(clustering = main_clustering$clustering, name = drake_songs_similar$name)

head(clustering_with_name )
clustering_with_name
clustering_with_name$name <- unlist(clustering_with_name$name)
clustering_with_name$name
head(clustering_with_name[clustering_with_name$clustering == 3,])

c1 <- clustering_with_name %>% filter(clustering == 1)
c2 <- clustering_with_name %>% filter(clustering == 2)
c3 <- clustering_with_name %>% filter(clustering == 3)
c1
c2
c3


names(main_clustering)
main_clustering$medoids

head(drake_songs_similar)
clustplot <- data.frame(drake_songs_similar[1:9],main_clustering$clustering)
head(clustplot)
clustplot
write.csv(clustplot, file="main_cluster.csv", row.names = FALSE)


#Outliers in HIP/HOP 1 Happier,Taki Taki,Eastside
#Outliers in RNB 2 no tears left to cry
#Outliers in TRAP 3 god is a woman, Woman Like Me
#---------------------------------------------------------------------
#Elbow Method for finding the optimal number of clusters
set.seed(123)

k.max <- 15
dataelbow <- scale.features2
get <- sapply(1:k.max, 
              function(k){kmeans(dataelbow, k, nstart=50,iter.max = 15 )$tot.withinss})
get
plot(1:k.max, get,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

features2 <- data.frame(drake_songs_similar[1:9])
scale.features2 <- scale(features2)
library(ggfortify)
library(ggthemes)
set.seed(5000)
rownames(scale.features2) <- drake_songs_similar$name
k_means <- kmeans(scale.features2, 4)

kmeans_plot <- autoplot(k_means, 
                        main = "K-means Clustering", 
                        data = scale.features2,
                        loadings = TRUE, 
                        loadings.colour = "#CC0000",
                        loadings.label.colour = "#CC0000", 
                        loadings.label = TRUE, 
                        loadings.label.size = 2.2,  
                        loadings.label.repel = T,
                        label.size = 2.2, 
                        label.repel = T) + 
  scale_fill_manual(values = c("#000066", "#9999CC", "#66CC99", "#FB7201")) + 
  scale_color_manual(values = c("#000066", "#9999CC", "#66CC99", "#FB7201")) + 
  theme(plot.title=element_text(size=18, face="bold"))

kmeans_plot

k_means$cluster

totalfeatures <- cbind(drake_songs_similar[12], apply(features2,2,scale))

cluster_centers <- as.data.frame(k_means$centers)
cluster <- c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4")
cluster_centers <- cbind(cluster, cluster_centers)
cluster_centers$cluster <- NULL
radarDF_4 <- gather(cluster_centers, key=Attribute, value=Score, -cluster) %>%
  spread(key=cluster, value=Score)


chartJSRadar(scores = radarDF_4, scaleStartValue = -4, maxScale =1.5, showToolTipLabel = TRUE, colMatrix = NULL)


kclusters <- data.frame(k_means$cluster)
kclusters$names <- rownames(kclusters)

kc1 <- kclusters %>% filter(k_means.cluster == 1)
kc2 <- kclusters %>% filter(k_means.cluster == 2)
kc3 <- kclusters %>% filter(k_means.cluster == 3)
kc4 <- kclusters %>% filter(k_means.cluster == 4)



