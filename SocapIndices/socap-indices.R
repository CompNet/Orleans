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
# Returns the list of names for the types of users.
###############################################################################
get.socap.types <- function()
{	result <- c("User","IFYFM","FMIFY")
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
# force: whether or not to force recalulation of social capitalism indices when the file already exists.
###############################################################################
retrieve.socap.indices <- function(folder.data, role.meas, force=FALSE)
{	socap.file <- get.socap.filename(folder.data)
	
	# indices previously processed >> we load them
	if(file.exists(socap.file) && !force)
		indices <- as.matrix(read.table(socap.file,header=TRUE))
	
	# otherwise, process and record them
	else
	{	# load the clean graph
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
			numerator <- length(intersect(neigh.in[[u]],neigh.out[[u]])) - 1
			denominator<- min(length(neigh.in[[u]]),length(neigh.out[[u]])) - 1
			if(denominator==0)
				result <- 0
			else
				result <- numerator/denominator
			return(result)
		})
#for(u in 1:vcount(g)){print(neigh.in[[u]]);print(neigh.out[[u]]);print(overlap[u]);print("-----")}

		# process ratio
		ratio <- sapply(1:vcount(g), function(u)
		{	# Note: the "-1"s compensate the fact igraph puts the node of interest in its own neighborhood
			numerator <- length(neigh.in[[u]]) - 1
			denominator <- length(neigh.out[[u]]) - 1
			if(denominator==0)
				result <- numerator
			else
				result <- numerator/denominator
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
# force: whether or not to force recalulation of degrees when the file already exists.
###############################################################################
retrieve.degrees <- function(folder.data, role.meas, force=FALSE)
{	degree.file <- get.degree.filename(folder.data)
	
	# degree previously processed >> we load it
	if(file.exists(degree.file) && !force)
		degrees <- as.matrix(read.table(degree.file,header=TRUE))
	
	# otherwise, process and record it
	else
	{	# load the clean graph
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
# force: whether or not to force recalculation of social capitalism indices 
#        when the file already exists.
###############################################################################
process.socap.indices <- function(folder.data, role.meas, force=FALSE)
{	
	# TODO @Nico j'ai utilise une implementation en R
	# en attendant que votre implementation en C++ soit clean
	
	# temporary: process the indices with igraph
	retrieve.socap.indices(folder.data, role.meas, force)
	
	# TODO could also use the C++ implementation of N&A, instead
	# get file paths
#	net.file <- get.network.filename(folder.data)
#	clean.file <- get.network.clean.filename(folder.data) #TODO @Nico one or the other?
#	compiled.file <- get.network.compiled.filename(folder.data) #TODO @Nico one or the other?
#	com.file <- get.communities.filename(folder.data,comdet.algo)
#	out.file <- get.rolemeas.filename(folder.data,role.meas,norm=FALSE,comdet.algo)
}

###############################################################################
# Identifies social capitalists depending on the social capitalism indices.
#
# values: social capitalism indices. "Overlap","Ratio","k-in"
# overlap.threshold: threshold used for the overlap index (determines if a 
#				     user is a social capitalist or not).
###############################################################################
identify.social.capitalists <- function(values, overlap.threshold)
{	start.time <- Sys.time()
	cat("[",format(start.time,"%a %d %b %Y %H:%M:%S"),"] Detecting social capitalists\n",sep="")
		# by default, or users are regular
		soccap.status <- rep(x=1,times=length(values))
		
		# overlap threshold for being a social caps
		cap.idx <- which(values[,"Overlap"]>overlap.threshold)
		# ratio threshold to distinguish the types of social caps
		temp.idx <- which(values[cap.idx,"Ratio"]>1)
print(values[,"Overlap"])		
		
		# set IFYFM social caps
		ifyfm.idx <- cap.idx[temp.idx]
		soccap.status[ifyfm.idx] <- 2
		
		# set FMIFY social caps
		fmify.idx <- cap.idx[-temp.idx]
		soccap.status[fmify.idx] <- 3
	end.time <- Sys.time()
	total.time <- end.time - start.time
	cat("[",format(end.time,"%a %d %b %Y %H:%M:%S"),"] Process completed in ",format(total.time),"\n",sep="")
	
	return(soccap.status)
}