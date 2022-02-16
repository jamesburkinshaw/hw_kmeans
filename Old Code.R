kc <- kmeans(newiris, 3)
kc
table(iris$Species, kc$cluster)

plot(newiris[c(first, second)], col=kc$cluster)
points(kc$centers[,c(first, second)], col=1:3, pch=8, cex=2)

#(DEPRECATED) pick a point from each cluster to be the initial centroid
#for (d in splitx){
#centroids <- rbind(centroids,d[sample(nrow(d), 1),])
#}