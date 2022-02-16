hw_kmeans_ani <- function(x, k, colx, coly) {
  
  get_centroids <- function(x, k){
    #split data into clusters
    splitx <- split(x, x$cluster)
    
    #find dimensions of data, initialise centroids & get column names of data
    dimensions <- ncol(x)-1
    centroids <- x[0,]
    columnNames <- colnames(centroids)
    
    #loop through clusters
    for (c in 1:k) {
      clusterCentroids <- c()
      cluster <- data.frame(splitx[c])
      #loop through dimensions
      for (d in 1:dimensions) {
        #get mean of the dimension for the cluster
        clusterCentroids <- append(clusterCentroids,mean(cluster[,d]))
      }
      #add the means of all the dimensions for each cluster
      clusterCentroids <- append(clusterCentroids,c)
      centroids <-rbind(centroids,clusterCentroids)
    }
    #add the column names
    colnames(centroids) <- columnNames
    return(centroids)
  }
  
  get_sum_of_squares <- function (x,k) {
    error <- 0
    
    #split data into clusters
    splitx <- split(x, x$cluster)
    dimensions <- ncol(x)-1
    #loop through clusters
    for (c in 1:k) {
      cluster <- data.frame(splitx[c])
      #loop through dimensions
      for (d in 1:dimensions) {
        ssd <- 0
        for (m in cluster[,d]) {
          ssd <- ssd + ((m-mean(cluster[,d]))^2)
        }
        ssd < ssd/(nrow(cluster[,d])-1)
        #add dimension sse to total
        error <- error + ssd
      }
    }
    print(error)
    return(error)
  }
  
  #Assign points to cluster at random
  cluster <- sample(1:k,nrow(x),replace = TRUE)
  x$cluster <- cluster
  
  #set initial centroids as means of clusters
  centroids <- get_centroids(x,k)
  
  #plot intial clusters and centroids
  plot(x[c(colx,coly)],col=x$cluster)
  points(centroids[c(colx,coly)],col=centroids$cluster,pch=4, cex=3)
  
  #Sys.sleep(2)
  
  while (TRUE) {
    convergence <- TRUE
    #loop through points
    for (r in 1:nrow(x)) {
      p <- x[r,]
      #plot data with point being tested
      plot(x[c(colx,coly)],col=x$cluster)
      points(p[c(colx,coly)],col=p$cluster,pch=19, cex=1)
      
      #get initial cluster to check for convergence 
      initialCluster <- x[r,]$cluster
      testError <- Inf
      
      #check which cluster to reassign to if any
      #loop through clusters
      for (c in 1:k) {
        cat('Testing Row ',r,' in cluster ',c,'\n')
    
        #set cluster of row being tested
        x[r,]$cluster <- c
        
        #calculate centroids for cluster including new point
        testCentroids <- get_centroids(x,k)
        
        #calculate sum of squares for cluster with row
        currentError <- get_sum_of_squares(x,k)
        
        if(currentError < testError) {
          x[r,]$cluster <- c
          newCluster <- c
          testError <- currentError
          centroids <- testCentroids
        }
      }
      
      if (initialCluster != newCluster) {
        convergence = FALSE
      }
    }
    
    if(convergence){
      break
    }
  }
  
  plot(x[c(colx,coly)],col=x$cluster)
  points(centroids[c(colx,coly)],col=centroids$cluster,pch=4, cex=3)
  
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