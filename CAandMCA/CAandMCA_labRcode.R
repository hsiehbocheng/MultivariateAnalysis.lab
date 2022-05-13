setwd("D:/NCCUSTAT/碩一下/多變量分析/data")
getwd

#fisher----
fisher<-read.table("fisher.txt") 

library(ca)
#Let us request a 2-D solution by choosing nd = 2 and check out the summary:
fisher.ca <- ca(fisher, nd=2)
fisher

plot(fisher.ca)

#mammals----
mammals<-read.table("mammals.txt", h=T) 
#Then apply MCA to the data by starting with a 2-D solution.
mammals.mca<-mjca(mammals, nd=2, lambda="indicator")
mammals.mca
mammals.mca<-mjca(mammals, nd=2, lambda="Burt") 
mammals.mca
plot(mammals.mca)
plot(mammals.mca, what = c("all", "all"), col=c("blue","red"))
mammals.mca<-mjca(mammals, nd=2, lambda="adjusted")
mammals.mca
mammals.jca<-mjca(mammals, nd=2, lambda="JCA") 
summary(mammals.jca)
plot(mammals.jca)
plot(mammals.jca, what = c("all", "all"), col=c("blue","red"))
