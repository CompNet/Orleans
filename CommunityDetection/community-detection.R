# Processes various calculations related to the
# network and its community structure.
# 
# Version: 2
# Author: Vincent Labatut 06/2013,07/2014
#
# setwd("~/eclipse/workspaces/Networks/Orleans/")
# setwd("C:/Eclipse/workspaces/Networks/Orleans/")
# source("GenerateData/community-detection.R")
###############################################################################
library("igraph")

source("CommunityDetection/igraph-louvain.R")
source("CommunityDetection/directed-louvain.R")


###############################################################################
# Returns the standard network filename.
###############################################################################
get.network.filename <- function()
{	return("network.edgelist")
}

###############################################################################
# Returns the standard degree filename.
###############################################################################
get.degrees.filename <- function()
{	return("degrees.txt")
}

###############################################################################
# Returns the list of names for the degree.
###############################################################################
get.degrees.names <- function()
{	return(c("In","Out","All"))
}

###############################################################################
# Returns the standard community structure filename.
#
# algo: community detection method.
###############################################################################
get.communities.filename <- function(algo)
{	resultat <- paste("communities.",algo,".txt",sep="")
	return(resultat)
}

###############################################################################
# Applies the specified community detection algorithm
# to the network contained in the specified folder.
#
# folder.data: folder containing the network file, and will contain the community file.
# algo: community detection method.
###############################################################################
detect.communities <- function(folder.data, algo)
{	if(algo=="LV")
		apply.igraph.louvain(folder.data, algo)
	else if(algo=="LVdir")
		apply.directed.louvain(folder.data, algo)
}
