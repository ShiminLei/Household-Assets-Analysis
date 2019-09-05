


#******************************************************
#
#				Additional Plots for
#				Principal Components
#
#******************************************************


loadingsplot <- function(pca.obj, j=1,  k = 2, ...)  
{
  par(mfrow = c(k-j+1,1))
  if(nrow(pca.obj$loadings) > 8){cn <- 8/nrow(pca.obj$loadings)} else {cn <- 1}
  for(i in j:k)
  {barplot(pca.obj$loadings[,i], main = paste("Component ", i), cex.names = cn , ...)}
  par(mfrow = c(1,1))
  
}

cumplot <- function(pca.obj, ...)
{
  xc <- cumsum(pca.obj$sdev^2)/sum(pca.obj$sdev^2)
  barplot(xc, ylim = c(0,1), main = "Proportion of Variance", ylab = "Proportion", names.arg = 1:length(pca.obj$sdev), xlab = "Components", ...)
  
}