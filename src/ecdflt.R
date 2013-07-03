# Plots cumulative distributions.
#
# Version: v1
# Author: Vincent Labatut
# Inspired by https://stat.ethz.ch/pipermail/r-help/2010-March/231899.html
#
# setwd("~/eclipse/workspaces/Networks/Orleans/")
# setwd("C:/Eclipse/workspaces/Networks/Orleans/")
# source("src/ecdflt.R")
###############################################################################

ecdflt <- function(x, inverse=FALSE, ...)
{	if(inverse)
	{	sorted <- sort(x, decreasing = TRUE)
		cdf <- as.vector(sapply(sorted, function(y) sum(x >= y)/length(x)))
	}
	
	else
	{	sorted <- sort(x, decreasing = FALSE)
		cdf <- as.vector(sapply(sorted, function(y) sum(x <= y)/length(x)))
	}
	
	plot(sorted, cdf, type="l", ylab="P", ...)
}


###############################################################################
# Test
###############################################################################
#x <- c(0.23, 0.09, 0.05, 0.02, 0.38, 1, 0.04, 0.01, 0.17, 0.04, 0.01, 0.17, 0.5)
#ecdflt(x, xlab="My values", main="my plot", col="RED")

#x <- c(rnorm(8000, 300, 10), rnorm(400, 500, 300) )
#ecdflt(x, xlab="My values", main="my plot", log="y", col="RED")
