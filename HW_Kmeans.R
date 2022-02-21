hw_kmeans <- function(x, k, colx, coly, msg=FALSE, doPlot=FALSE, animation=FALSE, interval=0.07, nstart=5) {
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
  
  get_sum_of_squares <- function (x,k,centroids) {
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
        #loop through rows in dimension
        for (m in cluster[,d]) {
          ssd <- ssd + ((m-centroids[c,d])^2)
        }
        ssd < ssd/(nrow(cluster[,d])-1)
        #add dimension error to total
        error <- error + ssd
      }
    }
    return(error)
  }
  
  referenceError<-Inf
  
  for (s in 1:nstart) {
    
    cat('Start ', s, '\n')
    
    #Assign points to cluster at random
    cluster <- sample(1:k,nrow(x),replace = TRUE)
    x$cluster <- cluster
    
    #index for sorting
    x$index <- as.numeric(row.names(x))
    
    #set initial centroids as means of clusters
    centroids <- get_centroids(x,k)
    
    if(doPlot){
      #plot intial clusters and centroids
      plot(x[c(colx,coly)],col=x$cluster,main='Initial Clusters/Centroids',sub=paste('Start',s,sep=' '))
      points(centroids[c(colx,coly)],col=centroids$cluster,pch=4, cex=3)
    }
    
    #interation for plots
    i <- 1
    while (TRUE) {
      convergence <- TRUE
      #shuffle data
      x <- x[sample(nrow(x)),]
      #loop through points
      for (r in 1:nrow(x)) {
        #add a delay for animation
        if (animation) {
          Sys.sleep(interval)
        }
        p <- x[r,]
        #get initial cluster to check for convergence 
        initialCluster <- x[r,]$cluster
        newCluster <- 0
        testError <- Inf
        
        #check which cluster to reassign to if any
        #loop through clusters
        for (c in 1:k) {
          if(msg){
            cat('Testing row ',r,' in cluster ',c,'\n')
          }
      
          #set cluster of row being tested
          x[r,]$cluster <- c
          
          #calculate centroids for cluster including new point
          testCentroids <- get_centroids(x,k)
          
          #calculate sum of squares for cluster with row
          currentError <- get_sum_of_squares(x,k,testCentroids)
          
          if(currentError < testError) {
            newCluster <- c
            testError <- currentError
            centroids <- testCentroids
            if (msg) {
              cat('Cluster for row ',r,' is ',newCluster,'\n')
            }
          }
        }
        
        x[r,]$cluster <- newCluster
        
        #plot data with point being tested
        if (animation){
          plot(x[c(colx,coly)],col=x$cluster,main='Clustering in Progress',sub=paste('Iteration', i, 'Point', r, 'Start', s, sep=' '))
          points(p[c(colx,coly)],col=p$cluster,pch=19, cex=1)
          points(centroids[c(colx,coly)],col=centroids$cluster,pch=4, cex=3) 
        }
        #if a cluster has moved loop again
        if (initialCluster != newCluster) {
          convergence = FALSE
        }
      }
      #if we have reached convergence stop iterating
      if(convergence){
        break
      }
      i<-i+1
    }
    
    #calculate sum of squares of clustering start
    currentError <- get_sum_of_squares(x,k,centroids)
    cat('Error of current cluster ', currentError, '\n')
    
    #if it is the lowest save it to return
    if (currentError < referenceError) {
      cat('Iteration ', s, ' is the new champion. Current Error=', currentError, '\n')
      startReturn <- s
      clusterReturn <- x
      centroidsReturn <- centroids
      referenceError <- currentError
    } 
  }
  
  if(doPlot){
    #plot final clusters
    plot(clusterReturn[c(colx,coly)],col=clusterReturn$cluster,main='Final Clusters', sub=paste('Start', startReturn, sep=' '))
    points(centroidsReturn[c(colx,coly)],col=centroidsReturn$cluster,pch=4, cex=3)
  }
  
  #reorder data to return
  toReturn <- list(clusterReturn[order(clusterReturn$index), ], centroidsReturn)
  class(toReturn) <- 'Cluster'
  return(toReturn)
}


#iris
newiris <- iris
newiris$Species <- NULL
#newiris

first="Sepal.Length"
second="Sepal.Width"

#kc <- kmeans(newiris, 3)
kc <- hw_kmeans(newiris,3, first, second, doPlot = TRUE, )

kc
table(iris$Species, kc[1]$cluster)

#plot(newiris[c(first, second)], col=kc$cluster)
#points(kc$centers[,c(first, second)], col=1:3, pch=8, cex=2)