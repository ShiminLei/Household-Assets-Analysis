
#**********************************************************
#
#			Scatter Plot Matrix Functions
#
#*********************************************************




panel.cor <- function(x, y, digits=2, prefix="", cex.cor)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- round(abs(cor(x, y, use = "complete.obs")), 2)
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = 2)
}

panel.hist <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y,
       col="steelblue2", ...)
}


uva.pairs <- function(vars, ...)
{
  args <- list(...)
  
  if(is.matrix(vars) | is.data.frame(vars)){
    if(is.null(args$labels))pairs(vars, lower.panel = panel.smooth, upper.panel = panel.cor, diag.panel = panel.hist, main = args$main)
    else(pairs(vars, lower.panel = panel.smooth, upper.panel = panel.cor, diag.panel = panel.hist, main = args$main, labels = args$labels))
  }
  else(if(is.character(vars)){
    if(is.null(args$labels))pairs(formula(vars), lower.panel = panel.smooth, upper.panel = panel.cor, diag.panel = panel.hist, main = args$main, data = args$data)
    else(pairs(formula(vars), lower.panel = panel.smooth, upper.panel = panel.cor, diag.panel = panel.hist, main = args$main, data = args$data, labels = args$labels))} 
    else(cat("You must enter a matrix, dataframe or formula")))
}