# Adapter allowing to apply the version of the Louvain
# community algorithm implemented in the igraph library.
# 
# Version: 1
# Author: Vincent Labatut 07/2014
#
# setwd("~/eclipse/workspaces/Networks/Orleans/")
# setwd("C:/Eclipse/workspaces/Networks/Orleans/")
# source("CommunityDetection/igraph-louvain.R")
###############################################################################
library("igraph")

###############################################################################
# Applies the igraph version of the Louvain community detection algorithm.
#
# folder.data: folder containing the network file, and will contain the community file.
# comdet.algo: community detection method.
###############################################################################
apply.igraph.louvain <- function(folder.data, comdet.algo)
{	# load the edgelist
	net.file <- get.network.filename(folder.data)
	el <- as.matrix(read.table(net.file))
	el <- el + 1 # number from 1
		
	# build graph from edgelist
	g <- graph.edgelist(el, directed=FALSE)
	
	# apply louvain
	temp <- multilevel.community(graph=g, weights=NULL)
	membership <- temp$membership - 1	# to get numbers starting from 0
	
	# record result
	com.file <- get.communities.filename(folder.data, comdet.algo)
	write.table(x=membership, file=com.file, row.names=FALSE, col.names=FALSE)
}