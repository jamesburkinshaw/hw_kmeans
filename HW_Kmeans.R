hw_kmeans <- function(x, k, colx, coly, msg=FALSE, doPlot=FALSE, animation=FALSE, interval=0.07, nstart=5, random=FALSE, save = FALSE) {
  get_centroids <- function(x, k, randomFlag){
    #split data into clusters
    splitx <- split(x, x$cluster)
    #find dimensions of data, initialise centroids & get column names of data
    dimensions <- ncol(x)-1
    centroids <- x[0,]
    columnNames <- colnames(centroids)
    #if random option is set to false calcluate means as centroids
    if (!randomFlag){
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
    } else {
      #random option is selected so select a random point from each cluster as centroid
      for (d in splitx){
      centroids <- rbind(centroids,d[sample(nrow(d), 1),])
      }
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
  
  #counter for plots
  plotIdx<-1
  
  #Initialise reference error to check which start is optimal
  referenceError<-Inf
  for (s in 1:nstart) {
    if (msg) {
      cat('Start ', s, '\n')
    }
    
    #Assign points to cluster at random
    cluster <- sample(1:k,nrow(x),replace = TRUE)
    x$cluster <- cluster
    
    #x$changes <- 0
    
    #index for sorting
    x$index <- as.numeric(row.names(x))
    
    #set initial centroids as means of clusters
    centroids <- get_centroids(x,k,random)
    
    if(doPlot | animation){
      #plot intial clusters and centroids
      plot(x[c(colx,coly)],col=x$cluster,main='Initial Clusters/Centroids',sub=paste('Start',s,sep=' '))
      points(centroids[c(colx,coly)],col=centroids$cluster,pch=4, cex=3)
    }
    
    if(save){
      #save initial clusters + centroids plot 
      file <- paste('plots/hw_plot_',plotIdx,'.png',sep='')
      png(filename=file, width=500,height =500)
      plot(x[c(colx,coly)],col=x$cluster,main='Initial Clusters/Centroids',sub=paste('Start',s,sep=' '))
      points(centroids[c(colx,coly)],col=centroids$cluster,pch=4, cex=3)
      dev.off()
      plotIdx<-plotIdx+1
    }
    
    
    #iterate until
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
          testCentroids <- get_centroids(x,k,FALSE)
          
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
        
        #if(x[r,]$cluster != newCluster) {
          #x[r,]$changes <- x[r,]$changes + 1
        #}
        
        x[r,]$cluster <- newCluster
        
        if (animation){
          #plot data with point being tested
          plot(x[c(colx,coly)],col=x$cluster,main='Clustering in Progress',sub=paste('Iteration', i, 'Point', r, 'Start', s, sep=' '))
          points(p[c(colx,coly)],col=p$cluster,pch=19, cex=1)
          points(centroids[c(colx,coly)],col=centroids$cluster,pch=4, cex=3)
        }
        if (save){
          #save animation plots
          file <- paste('plots/hw_plot_',plotIdx,'.png',sep='')
          png(filename=file, width=500,height =500)
          plot(x[c(colx,coly)],col=x$cluster,main='Clustering in Progress',sub=paste('Iteration', i, 'Point', r, 'Start', s, sep=' '))
          points(p[c(colx,coly)],col=p$cluster,pch=19, cex=1)
          points(centroids[c(colx,coly)],col=centroids$cluster,pch=4, cex=3)
          dev.off()
          plotIdx<-plotIdx+1
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
    
    if (msg) {
      cat('Error of current cluster ', currentError, '\n')
    }
    
    #if it is the lowest save it to return
    if (currentError < referenceError) {
      if(msg){
        cat('Iteration ', s, ' is the new champion. Current Error=', currentError, '\n')
      }
      startReturn <- s
      clusterReturn <- x
      centroidsReturn <- centroids
      referenceError <- currentError
    } 
  }
  
  if(doPlot | animation){
    #plot final clusters
    plot(clusterReturn[c(colx,coly)],col=clusterReturn$cluster,main='Final Clusters', sub=paste('Start', startReturn, sep=' '))
    points(centroidsReturn[c(colx,coly)],col=centroidsReturn$cluster,pch=4, cex=3)
  }
  if(save){
    #save final clusters plot
    file <- paste('plots/hw_plot_',plotIdx,'.png',sep='')
    png(filename=file, width=500,height =500)
    plot(clusterReturn[c(colx,coly)],col=clusterReturn$cluster,main='Final Clusters', sub=paste('Start', startReturn, sep=' '))
    points(centroidsReturn[c(colx,coly)],col=centroidsReturn$cluster,pch=4, cex=3)
    dev.off()
    plotIdx<-plotIdx+1
  }
  
  #normalise centroids by ordering on column
  centroidsReturn <- centroidsReturn[order(centroidsReturn[,colx]), ]
  centroidsReturn <- cbind(newCluster = 1:nrow(centroidsReturn), centroidsReturn) 
  
  for (r in 1:nrow(clusterReturn)) {
    for (c in 1:nrow(centroidsReturn))
      if (clusterReturn[r,]$cluster == centroidsReturn[c,]$cluster){
        #cat('row: ', r, ' old cluster: ', clusterReturn[r,]$cluster, ' new cluster: ', centroidsReturn[c,]$newCluster, '\n')
        newRowCluster <- centroidsReturn[c,]$newCluster
      }
    clusterReturn[r,]$cluster <- newRowCluster
  }

  centroidsReturn$cluster <- centroidsReturn$newCluster
  centroidsReturn <- within(centroidsReturn, rm(newCluster)) 
  
  #reorder data to return
  toReturn <- list(clusterReturn[order(clusterReturn$index), ], centroidsReturn[order(centroidsReturn$index), ])
  return(toReturn)
}

#iris
newiris <- iris
newiris$Species <- NULL
#newiris

first="Sepal.Length"
second="Sepal.Width"

#centroidList<-c()
#kc <- kmeans(newiris, 3)
#for (i in 1:3) {
  kc <- hw_kmeans(newiris,3, first, second, nstart = 1)
  kc
#  centroidList <- append(centroidList, kc[2])
#}
#centroidDf <- data.frame(kc[2])
#centroidDf$Sepal.Length
#centroidDf
#for (row in centroidDf){
#  print(row[1])
#}


#centroidList

#table(iris$Species, kc[1]$cluster)

#plot(newiris[c(first, second)], col=kc$cluster)
#points(kc$centers[,c(first, second)], col=1:3, pch=8, cex=2)