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
  points(centroids[c(colx,coly)],col=centroids$cluster,pch=4, cex=3)
  
  convergence <- FALSE
  #loop through points
  for (r in 1:nrow(x)) {
    p <- x[r,]
    points(p[c(colx,coly)],col=p$cluster,pch=19, cex=1)
    Sys.sleep(0.1)
    
    #check which cluster to reassign to if any
    
      #calculate sum of Squares of current cluster without row
      
      #calculate sum of squares of other clusters with row
    
    #recalculate means
    
    #do centroids move? if not then convergence
    
    #move centroids
    
  }
  
  #TODO: Or Randomly Sample Points?
  #point <- x[0,]
  #point <- rbind(point,x[sample(nrow(x), 1),])
  #points(point[c(colx,coly)],col=point$cluster,pch=19, cex=1)
  
}


#iris
newiris <- iris
newiris$Species <- NULL
#newiris

first="Sepal.Length"
second="Sepal.Width"

hw_kmeans_ani(newiris,3, first, second)