kc <- kmeans(newiris, 3)
kc
table(iris$Species, kc$cluster)

plot(newiris[c(first, second)], col=kc$cluster)
points(kc$centers[,c(first, second)], col=1:3, pch=8, cex=2)

#(DEPRECATED) pick a point from each cluster to be the initial centroid
#for (d in splitx){
#centroids <- rbind(centroids,d[sample(nrow(d), 1),])
#}



centroidList<-c()
#kc <- kmeans(newiris, 3)
#for (i in 1:3) {
kc <- hw_kmeans(newiris,3, first, second, nstart = 1)
#  centroidList <- append(centroidList, kc[2])
#}
centroidDf <- data.frame(kc[2])
centroidDf$Sepal.Length
centroidDf
for (row in centroidDf){
  print(row[1])
}
#for (cl in centroidList) {
#  cldf <- data.frame(cl)
#  for (row in cldf)
#    if (round(row$Sepal.Length,2)==5.90 && round(row$Sepal.Width,2)==2.75 && round(row$Petal.Length,2)==4.39 && round(row$Petal.Width,2)==1.43){
#      cldf$normCluster <- 'A'
#    }
# cldf
#}



#TODO: Run repeatedly, track times points change cluster, visualise
for (i in 1:3) {
  kc <- data.frame(hw_kmeans(newiris,3, first, second, nstart = 1)[2])
  #TODO:add cluster as new column
  #TODO:for row: if new cluster != current cluster change = change +1
  for (r in 1:nrow(kc)) {
    if (kc[r,]$cluster != kcUpdates[r,]$cluster){
      kcUpdates[,r]$clusterUpdates <- kcUpdates[r,]$clusterUpdates + 1
    }
    kcUpdates[,r]$cluster <- kc$cluster
  }
  #TODO: current cluster = new cluster
}

kcUpdates


#Initialise dataframe to track changes by running function for current cluster + add change column as 0
kcUpdates <- data.frame(hw_kmeans(newiris,3, first, second, nstart = 1)[1])
kcUpdates$clusterUpdates <- 0

#TODO: Run repeatedly, track times points change cluster, visualise
for (i in 1:50) {
  kc <- data.frame(hw_kmeans(newiris,3, first, second, nstart = 1)[1])
  #TODO:add cluster as new column
  #TODO:for row: if new cluster != current cluster change = change +1
  for (r in 1:nrow(kc)) {
    if (kc[r,]$cluster != kcUpdates[r,]$cluster){
      kcUpdates[r,]$clusterUpdates <- kcUpdates[r,]$clusterUpdates + 1
    }
    kcUpdates$cluster <- kc$cluster
  }
  #TODO: current cluster = new cluster
}

kcUpdates


#TODO: Run repeatedly, track times points change cluster, visualise
for (i in 1:3) {
  kc <- data.frame(hw_kmeans(newiris,3, first, second, nstart = 1, doPlot=TRUE)[1])
  #TODO:add cluster as new column
  print(kc)
  #TODO:for row: if new cluster != current cluster change = change +1
  for (r in 1:nrow(kc)) {
    #cat('Iteration: ', i,  ' row: ', r, ' Current Cluster: ', kcUpdates[r,]$cluster, ' New Cluster: ', kc[r,]$cluster, '\n' )
    if (kc[r,]$cluster != kcUpdates[r,]$cluster){
      print('Cluster Changed')
      kcUpdates[r,]$clusterUpdates <- kcUpdates[r,]$clusterUpdates + 1
    } 
    kcUpdates$cluster <- kc$cluster
  }
  #TODO: current cluster = new cluster
}

#TODO: visualise with number of changes as intensity ggplot2

#centroidList <- append(centroidList, kc[2])
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


#Initialise dataframe to track changes by running function for current cluster + add change column as 0
kcUpdates <- data.frame(hw_kmeans(newiris,3, first, second, nstart = 1, doPlot=TRUE)[1])
kcUpdates$clusterUpdates <- 0


#TODO:add cluster as new column
print(kc)
#TODO:for row: if new cluster != current cluster change = change +1
for (r in 1:nrow(kc)) {
  #cat('Iteration: ', i,  ' row: ', r, ' Current Cluster: ', kcUpdates[r,]$cluster, ' New Cluster: ', kc[r,]$cluster, '\n' )
  if (kc[r,]$cluster != kcUpdates[r,]$cluster){
    print('Cluster Changed')
    kcUpdates[r,]$clusterUpdates <- kcUpdates[r,]$clusterUpdates + 1
  } 
  kcUpdates$cluster <- kc$cluster
}
#TODO: current cluster = new cluster






#Initialise dataframe to track changes by running function for current cluster + add change column as 0
kcUpdates <- newiris
kc <- kmeans(newiris, 3)
kcUpdates$cluster <- kc$cluster
kcUpdates$clusterUpdates <- 0










#TODO: Run repeatedly, track times points change cluster, visualise
for (i in 1:3) {
  testiris <- newiris
  kcTest <- kmeans(testiris, 3)
  kcCenters <- kcTest$centers
  testiris$cluster <- kcTest$cluster
  
  #normalise centroids by ordering on column
  kcCenters <- cbind(originalCluster = 1:nrow(kcCenters), kcCenters) 
  kcCenters <- kcCenters[order(kcCenters[,'Sepal.Length']), ]
  kcCenters <- cbind(newCluster = 1:nrow(kcCenters), kcCenters) 
  kcCenters <- data.frame(kcCenters)
  print(testiris)
  print(kcCenters)
  
  for (r in 1:nrow(testiris)) {
    for (c in 1:nrow(kcCenters)) {
      if (testiris[r,]$cluster == kcCenters[c,]$originalCluster){
        newRowCluster <- kcCenters[c,]$newCluster
      }
    }
    testiris[r,]$cluster <- newRowCluster
  }
  print(testiris)
}







#normalise centroids by ordering on column
centroidsReturn <- centroidsReturn[order(centroidsReturn[,colx]), ]
centroidsReturn <- cbind(newCluster = 1:nrow(centroidsReturn), centroidsReturn) 

for (r in 1:nrow(clusterReturn)) {
  for (c in 1:nrow(centroidsReturn)){
    if (clusterReturn[r,]$cluster == centroidsReturn[c,]$cluster){
      #cat('row: ', r, ' old cluster: ', clusterReturn[r,]$cluster, ' new cluster: ', centroidsReturn[c,]$newCluster, '\n')
      newRowCluster <- centroidsReturn[c,]$newCluster
    }
    clusterReturn[r,]$cluster <- newRowCluster
  }
}

centroidsReturn$cluster <- centroidsReturn$newCluster
centroidsReturn <- within(centroidsReturn, rm(newCluster)) 



