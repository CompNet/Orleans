# Processes various calculations on the social
# capitalists indices.
# 
# Version: 1
# Author: Vincent Labatut 01/2015
#
# setwd("~/eclipse/workspaces/Networks/Orleans/")
# setwd("C:/Eclipse/workspaces/Networks/Orleans/")
# source("SocapIndices/socap-indices.R")
###############################################################################


###############################################################################
# Returns the list of names for the social capitalism indices.
###############################################################################
get.socap.names <- function()
{	result <- c("Overlap","Ratio","k-in")
	return(result)
}

###############################################################################
# Returns the name of the file containing the social capitalism indices.
#
# folder.data: folder containing all input and output files.
###############################################################################
get.socap.filename <- function(folder.data)
{	result <- paste(folder.data,"socap.txt",sep="")
	return(result)
}

###############################################################################
# Returns the list of names for the social capitalism indices.
###############################################################################
get.degree.names <- function()
{	result <- c("In","Out","Total")
	return(result)
}

###############################################################################
# Returns the name of the file containing the node degrees.
#
# folder.data: folder containing all input and output files.
###############################################################################
get.degree.filename <- function(folder.data)
{	result <- paste(folder.data,"degrees.txt",sep="")
	return(result)
}

###############################################################################
# Processes the social capitalism indices, for the specified network and
# community.
#
# folder.data: folder containing all input and output files.
# role.meas: considered role measures.
# force: whether or not to force recalulation.
###############################################################################
retrieve.socap.indices <- function(folder.data, role.meas, force=FALSE)
{	socap.file <- get.socap.filename(folder.data)
	
	# indices previously processed >> we load them
	if(file.exists(socap.file) && !force)
		indices <- as.matrix(read.table(socap.file,header=TRUE))
	
	# otherwise, process and record them
	else
	{	# load the original graph (non the re-numbered one) TODO: should load the cleaned one
		net.file <- get.network.clean.filename(folder.data)
		g <- read.graph(file=net.file, format="edgelist", directed=is.directed.rolemeas(role.meas))
		
		# get the degrees (process or load them) 
		degrees <- retrieve.degrees(folder.data, role.meas, force)
		
		# process neighborhoods
		neigh.in <- neighborhood(graph=g, order=1, nodes=V(g), mode="in")
		neigh.out <- neighborhood(graph=g, order=1, nodes=V(g), mode="out")

		# process overlap
		overlap <- sapply(1:vcount(g), function(u)
		{	# Note: the "-1"s compensate the fact igraph puts the node of interest in its own neighborhood
			denominator <- length(intersect(neigh.in[[u]],neigh.out[[u]])) - 1
			numerator <- min(length(neigh.in[[u]]),length(neigh.out[[u]])) - 1
			if(numerator==0)
				result <- 0
			else
				result <- denominator/numerator
			return(result)
		})

		# process ratio
		ratio <- sapply(1:vcount(g), function(u)
		{	# Note: the "-1"s compensate the fact igraph puts the node of interest in its own neighborhood
			denominator <- length(neigh.in[[u]]) - 1
			numerator <- length(neigh.out[[u]]) - 1
			if(numerator==0)
				result <- 0
			else
				result <- denominator/numerator
			return(result)
		})
		
		# build result matrix 
		indices <- cbind(
			overlap,
			ratio,
			degrees[,1]
		)
		colnames(indices) <- get.socap.names()
			
		# record as table
		write.table(x=indices,file=socap.file,row.names=FALSE,col.names=TRUE)
	}
	
	return(indices)
}

###############################################################################
# Processes the degrees (in, out, all) for the network located in the specified
# folder.
#
# folder.data: folder containing all input and output files.
# role.meas: considered role measures.
# force: whether or not to force recalulation.
###############################################################################
retrieve.degrees <- function(folder.data, role.meas, force=FALSE)
{	degree.file <- get.degree.filename(folder.data)
	
	# degree previously processed >> we load it
	if(file.exists(degree.file) && !force)
		degrees <- as.matrix(read.table(degree.file,header=TRUE))
	
	# otherwise, process and record it
	else
	{	# load the original graph (non the re-numbered one) TODO: should load the cleaned one
		net.file <- get.network.clean.filename(folder.data)
		g <- read.graph(file=net.file, format="edgelist", directed=is.directed.rolemeas(role.meas))
		
		# process degree
		degrees <- cbind(
			degree(g,mode="in"),
			degree(g,mode="out"),
			degree(g,mode="all")
		)
		colnames(degrees) <- get.degree.names()
		
		# record as table
		write.table(x=degrees,file=degree.file,row.names=FALSE,col.names=TRUE)
	}
	
	return(degrees)
}

###############################################################################
# Processes the social capitalism indices, for the specified network and
# community.
#
# folder.data: folder containing all input and output files.
# role.meas: considered role measures.
# force: whether or not to force recalculation.
###############################################################################
process.socap.indices <- function(folder.data, role.meas, force=FALSE)
{	
	# temporary: process the indices with igraph
	retrieve.socap.indices(folder.data, role.meas, force)
	
	# TODO could also use the C++ implementation of N&A, instead
	# get file paths
#	net.file <- get.network.filename(folder.data)
#	clean.file <- get.network.clean.filename(folder.data) #TODO one or the other?
#	compiled.file <- get.network.compiled.filename(folder.data) #TODO one or the other?
#	com.file <- get.communities.filename(folder.data,comdet.algo)
#	out.file <- get.rolemeas.filename(folder.data,role.meas,norm=FALSE,comdet.algo)
}
