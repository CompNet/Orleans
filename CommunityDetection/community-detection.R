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
#
# folder.data: folder containing the network file, and will contain the community file.
###############################################################################
get.network.filename <- function(folder.data)
{	result <- paste(folder.data,"network.edgelist",sep="")
}

###############################################################################
# Returns the standard degree filename.
#
# folder.data: folder containing the network file, and will contain the community file.
###############################################################################
get.degrees.filename <- function(folder.data)
{	result <- paste(folder.data,"degrees.txt",sep="")
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
# folder.data: folder containing the network file, and will contain the community file.
# comdet.algo: community detection method.
###############################################################################
get.communities.filename <- function(folder.data, comdet.algo)
{	result <- paste(folder.data,"comdet=",comdet.algo,sep="")
	resultat <- paste(resultat,".communities.txt",sep="")
	return(resultat)
}

###############################################################################
# Applies the specified community detection algorithm
# to the network contained in the specified folder.
#
# folder.data: folder containing the network file, and will contain the community file.
# algo: community detection method.
###############################################################################
detect.communities <- function(folder.data, comdet.algo)
{	if(comdet.algo=="LV")
		apply.igraph.louvain(folder.data, comdet.algo)
	else if(algo=="LVdir")
		apply.directed.louvain(folder.data, comdet.algo)
}
