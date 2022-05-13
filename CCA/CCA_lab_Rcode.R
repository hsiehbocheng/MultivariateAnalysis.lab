setwd("D:/NCCUSTAT/碩一下/多變量分析/data")
getwd()

col2000<-read.table("new_col2000.txt",h=T)
col<-scale(col2000)
x<-cbind(col[,1],col[,2],col[,6])
y<-cbind(col[,3],col[,4],col[,5])
#--Standardize all variables and apply CCA to the two groups using the R function "cancor".
cxy<-cancor(x,y)
cxy

library(robustbase);library(lme4);library(MVN) 

#--(1) Mardia's Test
mvn(col,mvnTest = c("mardia"), desc = FALSE, multivariatePlot = "qq",
    multivariateOutlierMethod = "adj", showOutliers = TRUE)
#--(2) Henze-Zirkler’s Tes
mvn(col,mvnTest = c("hz"), desc = FALSE, multivariatePlot = "qq",
    multivariateOutlierMethod = "adj", showOutliers = TRUE)
#--(3) Royston’s Test)
mvn(col,mvnTest = c("royston"), desc = FALSE, multivariatePlot = "qq",
    multivariateOutlierMethod = "adj", showOutliers = TRUE)
#--(4) Dornik-Haansen’s Test
mvn(col,mvnTest = c("dh"), desc = FALSE, multivariateOutlierMethod =
      "adj", showOutliers = TRUE)

# The above 4 tests all deny the assumption of multivariate normality!! (what shall we do?)

# Let us first remove the 14 outliers identified by the above tests (1), (2) and
# (4) (although you may not be happy about that) and run the tests again:

result<-mvn(col,mvnTest = c("dh"), desc = FALSE,
            multivariateOutlierMethod = "adj", showOutliers = TRUE, showNewData =
              TRUE)
newcol<-result$newData
mvn(newcol,mvnTest = c("mardia"))
# The new data now agree with the assumption of multivariate normality based on the tests (1), (2) and (3).

#However, the Dornik-Haansen’s Test still rejects the multivariate normal assumption:
mvn(newcol,mvnTest = c("dh"))

library(CCP)
newx<-cbind(newcol[,1], newcol[,2], newcol[,6])
newy<-cbind(newcol[,3], newcol[,4], newcol[,5])
newcxy<-cancor(scale(newx, scale=T, center=T),scale(newy, scale=T,
                                                    center=T))
rho<-newcxy$cor
p.asym(rho,28,3,3,tstat="Wilks")



newcxy
xx<-scale(newx,scale=T,center=T)
 yy<-scale(newy,scale=T,center=T)
 scorex<-xx%*%newcxy$xcoef[,1]
 scorey<-yy%*%newcxy$ycoef[,1]
 plot(scorex,scorey,type="n")
 text(scorex,scorey,row.names(newcol),cex=.6)
 