hw_kmeans_ani <- function(x, k, colx, coly) {
  
  #Assign points to cluster at random
  cluster <- sample(1:k,nrow(x),replace = TRUE)
  x$cluster <- cluster
  splitx <- split(x, x$cluster)
  
  #pick a point from each cluster to be the initial centroid and
  centroids <- x[0,]
  for (d in splitx){
    centroids <- rbind(centroids,d[sample(nrow(d), 1),])
  }
  
  #plot intial clusters and centroids
  plot(x[c(colx,coly)],col=x$cluster)
  points(centroids[c(colx,coly)],col=1:k,pch=4, cex=3)
  
  #update initial clusters to be means of main clusters
  dimensions <- ncol(centroids)-1
  for (c in 1:k) {
    for (d in 1:dimensions) {
      #centroids[c,d] <- mean(splitx[c][d])
      cluster <- data.frame(splitx[c])
      print(cluster[,d])
    }
  }
}


#iris
newiris <- iris
newiris$Species <- NULL
#newiris

first="Sepal.Length"
second="Sepal.Width"

hw_kmeans_ani(newiris,3, first, second)








kc <- kmeans(newiris, 3)
kc
table(iris$Species, kc$cluster)

plot(newiris[c(first, second)], col=kc$cluster)
points(kc$centers[,c(first, second)], col=1:3, pch=8, cex=2)