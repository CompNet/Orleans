# Plots (complementary) cumulative distributions.
#
# Version: 1
# Author: Vincent Labatut 06/2013
# Inspired by https://stat.ethz.ch/pipermail/r-help/2010-March/231899.html
#
# setwd("~/eclipse/workspaces/Networks/Orleans/")
# setwd("C:/Eclipse/workspaces/Networks/Orleans/")
# source("PostProcessing/ecdflt.R")
###############################################################################

###############################################################################
# Plot the cumulative distribution of the specified values. The complementary
# distribution can alternatively be processed.
#
# x: data to plot.
# complementary: whether to plot the classic distribution or its complementary.
# points: number of points to be plotted.
###############################################################################
ecdflt <- function(x, complementary=FALSE, points=NA, ...)
{	size <- length(x)
	if(is.na(points) || points>size)
		idx <- 1:size
	else
	{	step <- size %/% points
		idx <- seq(from=1,to=size,by=step)
		if(idx[length(idx)]!=size)
		idx <- c(idx,size)
	}
	
	if(complementary)
	{	sorted <- sort(x, decreasing = TRUE)[idx]
		cdf <- as.vector(sapply(sorted, function(y) sum(x >= y)/length(x)))
	}
	
	else
	{	sorted <- sort(x, decreasing = FALSE)[idx]
		cdf <- as.vector(sapply(sorted, function(y) sum(x <= y)/length(x)))
	}
	
	plot(sorted, cdf, type="l", ylab="Density", ...)
}


###############################################################################
# Test
###############################################################################
#x <- c(0.23, 0.09, 0.05, 0.02, 0.38, 1, 0.04, 0.01, 0.17, 0.04, 0.01, 0.17, 0.5)
#ecdflt(x, xlab="My values", main="my plot", col="RED")

#x <- c(rnorm(8000, 300, 10), rnorm(400, 500, 300) )
#ecdflt(x, xlab="My values", main="my plot", log="y", col="RED")
