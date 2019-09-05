

#***********************************************************
#
#			Factor Plots
#
#***********************************************************

# Basic factor plots
# start is the column or variable number to start plotting
# stop is the column or variable number to stop plotting
# data is the data set
# res is the response variable
# rname is the name of the response variable

plot.factor <- function(start, stop, data, res, rname = "Response")
{
  num <- stop - start + 1
  cols <- min(num, 3)
  rows <- ceiling(num/3)
  par(mfrow = c(rows, cols))
  for(i in start:stop)
    plot(data[,i]~as.factor(res), ylab = paste("Variable ", colnames(data)[i]), xlab = rname, main = colnames(data)[i])
  par(mfrow = c(1,1))
}


# Lplot.factor performs a log transform of the variables 
# and displays the results as factor plots


Lplot.factor <- function(start, stop, data, res, rname = "Response")
{
  sel <- which(apply(data[,start:stop],2, min) == 0)
  nd <- data
  nd[,names(sel)] <- log(data[,names(sel) ] + .01)
  nd[,setdiff(colnames(data[,start:stop]), names(sel))] <- log(nd[,setdiff(colnames(data[,start:stop]), names(sel))])
  num <- stop - start + 1
  cols <- min(num, 3)
  rows <- ceiling(num/3)
  par(mfrow = c(rows, cols))
  for(i in start:stop)
    plot(nd[,i]~as.factor(res), ylab = paste("Variable ", colnames(data)[i]), xlab = rname, main = colnames(data)[i])
  par(mfrow = c(1,1))
}

# Biplot factor takes a princomp object, a response variable,
# and the two components to be plotted.

biplot.fact <- function(pc.obj, res, comp = c(1,2), pch1 = 19, pch2 = 18)
{
  plot(pc.obj$scores[res == 0,comp[1]], pc.obj$scores[res == 0,comp[2]], col = "blue", xlab = paste("Comp", comp[1]), ylab = paste("Comp", comp[2]), ylim = c(min(pc.obj$scores[,comp[2]]), max(pc.obj$scores[,comp[2]])),
       xlim = c(min(pc.obj$scores[,comp[1]]), max(pc.obj$scores[,comp[1]])), pch = pch1)
  points(pc.obj$scores[res == 1,comp[1]], pc.obj$scores[res == 1,comp[2]], col = "red", pch = pch2)
  
}

biplot.fact2 <- function(pc.obj, res)
{
  plot(pc.obj$scores[res == 1,1], pc.obj$scores[res == 1,2], col = "blue", xlab = "Comp 1", ylab = "Comp 2", ylim = c(min(pc.obj$scores[,2]), max(pc.obj$scores[,2])),
       xlim = c(min(pc.obj$scores[,1]), max(pc.obj$scores[,1])), pch = 3)
  points(pc.obj$scores[res == 0,1], pc.obj$scores[res == 0,2], col = "red", pch = 5)
  
}

biplot.fact3 <- function(pc.obj, res)
{
  plot(pc.obj$scores[res == 0,1], pc.obj$scores[res == 0,2], col = "blue", xlab = "Comp 1", ylab = "Comp 2", ylim = c(min(pc.obj$scores[,2]), max(pc.obj$scores[,2])),
       xlim = c(min(pc.obj$scores[,1]), max(pc.obj$scores[,1])), pch = 3)
  points(pc.obj$scores[res == 1,1], pc.obj$scores[res == 1,2], col = "red", pch = 5)
  
}



# Factor plot with vector input for variable numbers

plot.factorV <- function(V, data, res, rname = "Response")
{
  num <- length(V)
  cols <- min(num, 3)
  rows <- ceiling(num/3)
  par(mfrow = c(rows, cols))
  for(i in V)
    plot(data[,i]~as.factor(res), ylab = paste("Variable ", i), xlab = rname)
  par(mfrow = c(1,1))
}

Lplot.factorV <- function(V, data, res, rname = "Response")
{
  sel <- which(apply(data[,V],2, min) == 0)
  nd <- data
  nd[,names(sel)] <- log(data[,names(sel) ] + .01)
  nd[,setdiff(colnames(data[,V]), names(sel))] <- log(nd[,setdiff(colnames(data[,V]), names(sel))])
  num <- length(V)
  cols <- min(num, 3)
  rows <- ceiling(num/3)
  par(mfrow = c(rows, cols))
  for(i in V)
    plot(nd[,i]~as.factor(res), ylab = paste("Variable ", i), xlab = rname)
  par(mfrow = c(1,1))
}