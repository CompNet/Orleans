# Adapter allowing to apply the classic k-means algoritm
# implemented in R.
# 
# Version: 1
# Author: Vincent Labatut 07/2014
#
# setwd("~/eclipse/workspaces/Networks/Orleans/")
# setwd("C:/Eclipse/workspaces/Networks/Orleans/")
# source("CommunityDetection/kmeans.R")
###############################################################################

###############################################################################
# Applies the R version of the k-means clustering algorithm.
#
# folder.data: folder containing all input and output files.
# role.meas: type of role measures.
# clust.algo: cluster analysis method.
# comdet.algo: community detection algorithm.
###############################################################################
apply.kmeans <- function(folder.data, role.meas, clust.algo, comdet.algo)
{	# load the normalized data
	in.file <- get.rolemeas.filename(folder.data,role.meas,norm=TRUE,comdet.algo)
	data <- as.matrix(read.table(in.file))
	
	# apply k-means	
	kmeans(x=data, centers, iter.max = 10, nstart = 1,
			algorithm = c("Hartigan-Wong", "Lloyd", "Forgy",
					"MacQueen"), trace=FALSE)
	
	membership <- temp$cluster
	
	# record result
	out.file <- get.cluster.filename(folder.data,role.meas,0,clust.algo,comdet.algo)
	write.table(x=membership, file=out.file, row.names=FALSE, col.names=FALSE)
}
