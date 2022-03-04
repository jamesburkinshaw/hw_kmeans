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
      dimensions <- ncol(x)-2
      #loop through clusters
      for (c in 1:k) {
        cluster <- as.data.frame(splitx[c])
        #print(cluster)
        #loop through dimensions
        for (d in 1:dimensions) {
          ssd <- 0
          #loop through rows in dimension
          for (m in cluster[,d]) {
            ssd <- ssd + ((m-centroids[c,d])^2)
            #cat('Value: ', m, ' Mean: ', centroids[c,d], ' SSD: ', ssd, '\n')
          }
          ssd <- ssd/(nrow(cluster)-1)
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
      
      #index for sorting
      x$index <- as.numeric(row.names(x))
      
      #set initial centroids as means of clusters
      centroids <- get_centroids(x,k,random)
      
      if(doPlot|| animation){
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
    
    if(save){
      #save final clusters plot
      file <- paste('plots/hw_plot_',plotIdx,'.png',sep='')
      png(filename=file, width=500,height =500)
      plot(clusterReturn[c(colx,coly)],col=clusterReturn$cluster,main='Final Clusters', sub=paste('Start', startReturn, sep=' '))
      points(centroidsReturn[c(colx,coly)],col=centroidsReturn$cluster,pch=4, cex=3)
      dev.off()
      plotIdx<-plotIdx+1
    }
    
    if(doPlot | animation){
      #plot final clusters
      plot(clusterReturn[c(colx,coly)],col=clusterReturn$cluster,main='Final Clusters', sub=paste('Start', startReturn, sep=' '))
      points(centroidsReturn[c(colx,coly)],col=centroidsReturn$cluster,pch=4, cex=3)
    }
    
    #reorder data to return
    toReturn <- list(clusterReturn[order(clusterReturn$index), ], centroidsReturn[order(centroidsReturn$index), ])
    return(toReturn)
  }
  
  newiris <- iris
  newiris$Species <- NULL
  plotIdx <- 1
  
  first="Sepal.Length"
  second="Sepal.Width"
  
  #kc <- data.frame(hw_kmeans(newiris,3, first, second, nstart = 1,doPlot=TRUE)[1])
  
  #initialise clusters for iris
  irisClusterUpdates <- newiris
  irisClusterUpdates$cluster <- 0
  irisClusterUpdates$updates <- -1
  irisClusterUpdates
  
  #loop to see how often points change
  for (i in 1:100) {
    #initialise clusters to test
    irisTest <- newiris
    irisTestKM <- kmeans(irisTest, 3)
    irisTest$cluster <- irisTestKM$cluster
    
    #initialise centers
    testCenters <- irisTestKM$centers
    testCenters <- cbind(originalCluster = 1:nrow(testCenters), testCenters)
    
    #normalise centroids by ordering on column
    testCenters <- testCenters[order(testCenters[,'Sepal.Length']), ]
    
    #add new clusters after ordering
    testCenters <- cbind(newCluster = 1:nrow(testCenters), testCenters) 
    testCenters <- as.data.frame(testCenters)
    
    #for each row in test clustering update the cluster to match the normalised one
    for (r in 1:nrow(irisTest)) {
      for (c in 1:nrow(testCenters)) {
        if (irisTest[r,]$cluster == testCenters[c,]$originalCluster){
          #cat('Old Center: ', irisTest[r,]$cluster ,' New Cluster: ', testCenters[c,]$newCluster, '\n')
          newCluster <- testCenters[c,]$newCluster
        }
      }
      irisTest[r,]$cluster <- newCluster
    }
    
    #Sys.sleep(0.5)
    #file <- paste('plots/r_plots_',plotIdx,'.png',sep='')
    #png(filename=file, width=500,height =500)
    #plot(irisTest[c(first, second)], col=irisTest$cluster, main=paste('Iteration', plotIdx, sep = ' '))
    #points(testCenters[,c(first, second)], col=1:3, pch=8, cex=2)
    #dev.off()
    #plotIdx<-plotIdx+1
    
    #Increment number of updates if the cluster has changed
    for (r in 1:nrow(irisClusterUpdates)){
      if (irisClusterUpdates[r,]$cluster != irisTest[r,]$cluster) {
        #cluster has updated
        irisClusterUpdates[r,]$updates <- irisClusterUpdates[r,]$updates + 1
        irisClusterUpdates[r,]$cluster <- irisTest[r,]$cluster
      }
    }
  }
  
  irisClusterUpdates
  irisClusterUpdates$clusterCat <- factor(irisClusterUpdates$cluster)
  
  #install.packages("ggplot2")
  
  library(ggplot2)
  ggplot(irisClusterUpdates, aes(x=Sepal.Length, y=Sepal.Width, size = updates, color=clusterCat)) + 
    geom_point(alpha=0.7) +
    labs(size='Cluster Changes', color='Final Cluster') +
    ggtitle('Iris Clustering Changes')