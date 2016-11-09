###clustering
##clustering data with hierarchical clustering

#load data from customer.csv 
customer=read.csv('customer.csv',header=TRUE)
head(customer)
#examine the dataset structure
str(customer)
#normalize the customer data into the same scale
customer=scale(customer[,-1])
#use agglomerative hierarchical clustering to cluster 
hc=hclust(dist(customer,method="euclidean"),method="ward.D2")
hc
#Plot the dendrogram
plot(hc,hang=-0.01,cex=0.7)
#use the single method to perform hierarchical clustering
hc2=hclust(dist(customer),method="single")
plot(hc2,hang=-0.01,cex=0.7)

##Cutting trees into clusters
#categorize the data into four groups
fit=cutree(hc,k=4)
fit
table(fit)
plot(hc)
rect.hclust(hc,k=4,border="red")

##clustering data with the k-means method
set.seed(22)
fit=kmeans(customer,4)
fit
#inspect the center of each cluster
barplot(t(fit$centers),beside = TRUE,xlab="cluster",ylab="value")
#draw a scatter plot of the data 
plot(customer,col=fit$cluster)

##drawing a bivariate cluster plot
install.packages("cluster")
library(cluster)
clusplot(customer,fit$cluster,color=TRUE,shade = TRUE)
#zoom into the bivariate cluster plot
par(mfrow=c(1,2))
clusplot(customer,fit$cluster,color=TRUE,shade=TRUE)
rect(-0.7,-1.7,2.2,-1.2,border="orange",lwd=2)
clusplot(customer,fit$cluster,color=TRUE,xlim=c(-0.7,2.2),ylim=c(-1.7,-1.2))
#use cmdscale to reduce the dimensions
mds=cmdscale(dist(customer),k=2)
plot(mds,col=fit$cluster)

##comparing clustering methods
install.packages("fpc")
library(fpc)
single_c=hclust(dist(customer),method="single")
hc_single=cutree(single_c,k=4)
#use the complete method to cluster customer data
complete_c=hclust(dist(customer),method="complete")
hc_complete=cutree(complete_c,k=4)
#use k-means clustering
set.seed(22)
km=kmeans(customer,4)
#retrieve the cluster validation statistics of either clustering method
cs=cluster.stats(dist(customer),km$cluster)
#using within.cluster.ss and avg.silwidth to validate the clustering method
cs[c("within.cluster.ss","avg.silwidth")]
#generate the cluster statics of each clustering method and list them in a table
sapply(list(kmeans=km$cluster,hc_single=hc_single,hc_complete=hc_complete),function(c) cluster.stats(dist(customer),c)[c("within.cluster.ss","avg.silwidth")])

##Extracting silhouette information from clustering
set.seed(22)
km=kmeans(customer,4)
#compute the sihouette information
kms=silhouette(km$cluster,dist(customer))
summary(kms)
plot(kms)
##obtaining the optimum number of clusters for k-means
#calculate the within sum of squares of different numbers of cluster
nk=2:10
set.seed(22)
WSS=sapply(nk,function(k){
  kmeans(customer,centers=k)$tot.withinss
})
WSS
#use a line plot
plot(nk,SW,type="l",xlab="number of k",ylab="within sum of squares")
#Retrieve the maximum number of clusters
nk[which.max(SW)]

##Clustering data with the density-based method
install.packages("mlbench")
library(mlbench)
#use the mlbench to draw a Cassini problem graph
set.seed(2)
p=mlbench.cassini(500)
plot(p$x)
#cluster data with regard to its density measurement
ds=dbscan(dist(p$x),0.2,2,countmode = NULL,method="dist")
ds
plot(ds,p$x)
#use dbscan to predict which cluster the data point belongs to
y=matrix(0,nrow=3,ncol=2)
y[1,]=c(0,0)
y[2,]=c(0,-1.5)
y[3,]=c(1,1)
y
predict(ds,p$x,y)

##clustering data with the model-based method
install.packages("mclust")
library(mclust)
#perform model-based clustering 
mb=Mclust(customer)
plot(mb)
summary(mb)

##Visualizing a dissimilarity matrix
install.packages("seriation")
library(seriation)
dissplot(dist(customer),labels=km$cluster,options=list(main="Kmeans Clustering With k=4"))

complete_c=hclust(dist(customer),method="complete")
hc_complete=cutree(complete_c,k=4)
dissplot(dist(customer),labels=hc_complete,options=list(main="Hierarchical Clustering"))

##using dist and image functions to visualize a distance matrix
image(as.matrix(dist(customer)))
##plot both a dendrogram and heat map to show how data is cluster
cd=dist(customer)
hc=hclust(cd)
cdt=dist(t(customer))
hcc=hclust(cdt)
heatmap(customer,Rowv=as.dendrogram(hc),Colv=as.dendrogram(hcc))

##Validating clusters externally
install.packages("png")
library(png)
#read images from handwriting.png
img2=readPNG("handwriting.png",TRUE)
img3 = img2[,nrow(img2):1]
b = cbind(as.integer(which(img3 < -1) %% 28), which(img3 < -1) /28)
plot(b, xlim=c(1,28), ylim=c(1,28))
#perform a k-means clustering method on the handwriting digits
set.seed(18)
fit = kmeans(b, 2)
plot(b,col=fit$cluster)
plot(b,col=fit$cluster,xlim=c(1,28),ylim=c(1,28))
#perform the dbscan clustering method on the handwriting digits
ds=dbscan(b,2)
ds
plot(ds,b,xlim=c(1,28),ylim=c(1,28))

