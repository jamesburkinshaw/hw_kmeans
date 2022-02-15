hw_kmeans_ani <- function(x, k, colx, coly) {
  
  #Assign points to cluster at random
  cluster <- sample(1:k,nrow(x),replace = TRUE)
  x$cluster <- cluster
  splitx <- split(x, x$cluster)
  
  #(DEPRECATED) pick a point from each cluster to be the initial centroid
  #for (d in splitx){
    #centroids <- rbind(centroids,d[sample(nrow(d), 1),])
  #}
  
  #set initial centroids as means of clusters
  dimensions <- ncol(x)-1
  centroids <- x[0,]
  columnNames <- colnames(centroids)
  for (c in 1:k) {
    clusterCentroids <- c()
    for (d in 1:dimensions) {
      cluster <- data.frame(splitx[c])
      clusterCentroids <- append(clusterCentroids,mean(cluster[,d]))
    }
    clusterCentroids <- append(clusterCentroids,c)
    centroids <-rbind(centroids,clusterCentroids)
  }
  colnames(centroids) <- columnNames
  
  #plot intial clusters and centroids
  plot(x[c(colx,coly)],col=x$cluster)
  points(centroids[c(colx,coly)],col=1:k,pch=4, cex=3)
  
}


#iris
newiris <- iris
newiris$Species <- NULL
#newiris

first="Sepal.Length"
second="Sepal.Width"

hw_kmeans_ani(newiris,3, first, second)