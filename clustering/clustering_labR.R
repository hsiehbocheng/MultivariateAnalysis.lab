##############################################

olive <- read.table("olive.txt",header = T)
newolive <- olive[,3:10]
#agnes
library(cluster)
x <- daisy(newolive, stand=T) 
agn <- agnes(x, metric="euclidean", method="single")
plot(agn,which.plots=2)
plot(agn,which.plots=1)
agn$ac
olive[,1][agn$order]-olive[,1]
olive[,2][agn$order]-olive[,2]
#try complete linkage
agn <- agnes(x, metric="euclidean", method = "complete")
plot(agn,which.plots=2)
agn$ac
#ward linkage
agn <- agnes(x, metric="euclidean", method = "ward")
plot(agn,which.plots=2)
agn$ac

#Partitioning Method
#k-means
km <- kmeans(newolive,3,20)
pca.newolive <- princomp(scale(newolive,scale=TRUE,center=TRUE),cor=FALSE)
pcs.newolive <- predict(pca.newolive)
plot(pcs.newolive[,1:2], type = 'n')
text(pcs.newolive,as.character(km$cluster),col=km$cluster,cex=1)
plot(pcs.newolive[,1:2],type="n",xlab='1st PC',ylab='2nd PC')
text(pcs.newolive[,1:2],as.character(olive$Region),col=olive$Region,cex=1)