# Processes various calculations related to the
# network and its community structure.
# 
# Version: 2
# Author: Vincent Labatut 06/2013, 07/2014
#
# setwd("~/eclipse/workspaces/Networks/Orleans/")
# setwd("C:/Eclipse/workspaces/Networks/Orleans/")
# source("GenerateData/community-detection.R")
###############################################################################
library("igraph")

source("CommunityDetection/igraph-louvain.R")
source("CommunityDetection/directed-louvain.R")


###############################################################################
# Returns the standard network filename for the original network.
#
# folder.data: folder containing the network file.
###############################################################################
get.network.filename <- function(folder.data)
{	result <- paste(folder.data,"network.edgelist",sep="")
}

###############################################################################
# Returns the standard network filename for the clean network (renumbered).
#
# folder.data: folder containing the network file.
###############################################################################
get.network.clean.filename <- function(folder.data)
{	result <- paste(folder.data,"network.clean.edgelist",sep="")
}

###############################################################################
# Returns the standard network filename for the compiled network.
#
# folder.data: folder containing the network file.
###############################################################################
get.network.compiled.filename <- function(folder.data)
{	result <- paste(folder.data,"network.bin",sep="")
}

###############################################################################
# Returns the standard degree filename.
#
# folder.data: folder containing the network file, and which will contain the 
#              community file.
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
# folder.data: folder containing the network file, and which will contain the 
#              community file.
# comdet.algo: community detection method.
###############################################################################
get.communities.filename <- function(folder.data, comdet.algo)
{	result <- paste(folder.data,"comdet=",comdet.algo,sep="")
	result <- paste(result,".communities.txt",sep="")
	return(result)
}

###############################################################################
# Loads a graph as a text file, verifies its numerotation and
# possibly corrects it so that it starts from zero, and 
# records the results as a "clean" text file, which can
# be used directly. Also generates a file matching the original
# node names and the new "clean" ids.
#
# folder.data: folder containing the network file, and will contain the community file.
###############################################################################
clean.graph.file <- function(folder.data)
{	# get the file names
	net.file <- get.network.filename(folder.data)
	clean.file <- get.network.clean.filename(folder.data)
	
	# set up the command, including all required parameters
	cmd <- paste("blablabla ",net.file," ",clean.file,sep="") # TODO complete with appropriate parameters
}

###############################################################################
# Loads a clean graph text file, and compiles it to get a more convenient
# representation. It is then recorded as a binary file, in order to be used
# later for community detection and/or role measure processing.
#
# folder.data: folder containing the network file, and which will contain the 
#			   community file.
###############################################################################
compile.graph.file <- function(folder.data)
{	# renumber the original edgelist, if necessary
	clean.file <- get.network.clean.filename(folder.data)
	if(!file.exists(clean.file))
		clean.graph.file(folder.data)
	
	# set up the command, including all required parameters
	compiled.file <- get.compiled.filename(folder.data)
	cmd <- paste("blablabla ",clean.file," ",compiled.file,sep="") # TODO complete with appropriate parameters
}

###############################################################################
# Applies the specified community detection algorithm
# to the network contained in the specified folder.
#
# folder.data: folder containing the network file, and which will contain the 
#			   community file.
# algo: community detection method.
###############################################################################
detect.communities <- function(folder.data, comdet.algo)
{	start.time <- Sys.time();
	cat("[",format(start.time,"%a %d %b %Y %H:%M:%S"),"] Detecting communities\n",sep="")
	
	# apply the appropriate algorithm
	if(comdet.algo=="LV")
		apply.igraph.louvain(folder.data, comdet.algo)
	else if(comdet.algo=="LV-dir")
		apply.directed.louvain(folder.data, comdet.algo)
	else if(comdet.algo %in% c("OSLOM-undir-p", "OSLOM-dir-p", "OSLOM-undir-o", "OSLOM-dir-o"))
		apply.oslom(floder.data, comdet.algo)
	
	end.time <- Sys.time();
	total.time <- end.time - start.time;
	cat("[",format(end.time,"%a %d %b %Y %H:%M:%S"),"] Communities detected in ",format(total.time),"\n",sep="")
}
