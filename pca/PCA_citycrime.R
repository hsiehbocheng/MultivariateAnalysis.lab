setwd("D:/NCCUSTAT/碩一下/多變量分析")
getwd()
crime<-read.table("citycrime.txt") 
pairs(crime)
cor(crime)
library(stats)
pca.crime<-princomp(crime,cor=TRUE) 
summary(pca.crime)
loadings(pca.crime) ##看係數 空的是接近0
pcs.crime<-predict(pca.crime)  #Calculate the PC scores.
eigen<-eigen(cor(crime))
plot(eigen$values,type="h") 
#Plot the first 2 PCs.
plot(pcs.crime[,1:2],type="n",xlab='1st PC',ylab='2nd PC')
text(pcs.crime[,1:2],row.names(crime)) 

#Plot also the biplot. 
biplot(pca.crime,scale=1)#scale = 1 則X=GH的H變成B*sqrt(eigenvalue) -> corrlation matrix(R內設)

#permutaion test code
sign.pc<-function(x,R=1000,m=length(x), cor=T,...){
  # run PCA
  pc.out<-princomp(x,cor=cor,...)
  # the proportion of variance of each PC
  pve=(pc.out$sdev^2/m)[1:m]
  # a matrix with R rows and m columns that contains
  # the proportion of variance explained by each pc
  # for each randomization replicate.
  pve.perm<-matrix(NA,ncol=m,nrow=R)
  
  for(i in 1:R){
    # permutation each column針對col作亂排
    x.perm<-apply(x,2,sample)
    # run PCA 
    pc.perm.out<-princomp(x.perm,cor=cor,...)
    # the proportion of variance of each PC.perm
    pve.perm[i,]=(pc.perm.out$sdev^2/m)[1:m]
  }
  # calcalute the p-values
  pval<-apply(t(pve.perm)>pve,1,sum)/R
  return(list(pve=pve,pval=pval))
}
install.packages('RCurl')
library(RCurl)
sign.pc(crime,cor=T)
#只有第一個pvalue 有通過 即只有comp1具有解釋性 #但一維度難以解釋 所以加入comp2



x <- c(-5:5)
plot(sin(x))


