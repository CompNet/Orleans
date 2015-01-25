# Processes the community role measures, as an alternative to the C++ implementation. 
# 
# Version: 1
# Author: Vincent Labatut 01/2015
#
# setwd("~/eclipse/workspaces/Networks/Orleans/")
# setwd("C:/Eclipse/workspaces/Networks/Orleans/")
# source("RoleMeasures/process-rolemeas.R")
###############################################################################


###############################################################################
# Processes the community role measures, as an alternative to the C++ implementation. 
#
# folder.data: folder containing all input and output files.
# role.meas: considered role measures.
# comdet.algo: algorithm used for community detection (needed to infer the file name)
# force: whether or not to force recalulation.
###############################################################################
retrieve.role.measures <- function(folder.data, role.meas, comdet.algo, force=FALSE)
{	rolemeas.file <- get.rolemeas.filename(folder.data,role.meas,norm=FALSE,comdet.algo)
	
	# indices previously processed >> we load them
	if(file.exists(rolemeas.file) && !force)
		values <- as.matrix(read.table(rolemeas.file,header=TRUE))
	
	# otherwise, process and record them
	else
	{	# load the cleaned graph
		net.file <- get.network.clean.filename(folder.data)
		g <- read.graph(file=net.file, format="edgelist", directed=is.directed.rolemeas(role.meas))
		
		# load the community membership
		com.file <- get.communities.filename(folder.data,comdet.algo)
		membership <- as.vector(as.matrix(read.table(com.file)))
		
		# process neighborhoods
		neigh.all <- neighborhood(graph=g, order=1, nodes=V(g), mode="all")
		neigh.in <- neighborhood(graph=g, order=1, nodes=V(g), mode="in")
		neigh.out <- neighborhood(graph=g, order=1, nodes=V(g), mode="out")
		
		# process the associated communities
		com.all <- list()
		com.in <- list()
		com.out <- list()
		for(u in 1:vcount(g))
		{	nall <- neigh.all[[u]][neigh.all[[u]]!=u]
			com.all[[u]] <- membership[nall]
			nin <- neigh.in[[u]][neigh.in[[u]]!=u]
			com.in[[u]] <- membership[nin]
			nout <- neigh.out[[u]][neigh.out[[u]]!=u]
			com.out[[u]] <- membership[nout]
		}
		
		# process the role measures
		values <- process.individual.rolemeas(role.meas, membership, com.all, com.in, com.out) 

		# record as table
		write.table(x=values,file=rolemeas.file,row.names=FALSE,col.names=TRUE)
	}
	
	return(values)
}

###############################################################################
# Processes the community z-score, i.e. the z-score of a given value for each
# node, but processed relatively to the node community (by opposition to the
# whole network.
#
# values: the values to consider.
# membership: communities of the nodes.
###############################################################################
process.community.zscore <- function(values, membership)
{	result <- rep(NA,length(values))
	coms <- sort(unique(membership))
	
	for(i in 1:length(coms))
	{	com <- coms[i]
		idx <- which(membership==com)
		result[idx] <- scale(values[idx])
	}
	
	return(result)
}

###############################################################################
# Processes the specified role measures, taking the specified communities
# into account. The communities for the total, incoming and outgoing neighborhoods
# must have been pre-processed.
#
# role.meas: considered role measures.
# membership: communities of the nodes.
# com.all: communities of the total neighborhoods.
# com.in: communities of the incoming neighborhoods.
# com.out: communities of the outgoing neighborhoods.
###############################################################################
process.individual.rolemeas <- function(role.meas, membership, com.all, com.in, com.out) 
{	names <- get.rolemeas.names(role.meas)
	result <- matrix(nrow=length(com.all),ncol=length(names))
	colnames(result) <- names
	
	# Guimera-Amaral
	if(role.meas=="GA")
	{	# P
		result[,"P"] <- sapply(com.all,function(coms)
		{	if(length(coms)==0)
				res <- 0
			else
			{	numerator <- (table(coms))^2
				denominator <- rep((length(coms))^2,length(numerator))
				res <- 1 - sum(numerator/denominator)
			}
			return(res)
		})

		# z
		k.int <- sapply(1:length(com.all),function(u)
		{	coms <- com.all[[u]]
			own.com <- membership[u]
			res <- length(which(coms==own.com))
			return(res)
		})
		result[,"z"] <- process.community.zscore(values=k.int,membership)
	}
	
	# directed Guimera-Amaral
	else if(role.meas=="GA-dir")
	{	# P-in
		result[,"P-in"] <- sapply(com.in,function(coms)
		{	if(length(coms)==0)
				res <- 0
			else
			{	numerator <- (table(coms))^2
				denominator <- rep((length(coms))^2,length(numerator))
				res <- 1 - sum(numerator/denominator)
			}
			return(res)
		})

		# P-out
		result[,"P-out"] <- sapply(com.out,function(coms)
		{	if(length(coms)==0)
				res <- 0
			else
			{	numerator <- (table(coms))^2
				denominator <- rep((length(coms))^2,length(numerator))
				res <- 1 - sum(numerator/denominator)
			}
			return(res)
		})

		# z-in
		k.int <- sapply(1:length(com.in),function(u)
		{	coms <- com.in[[u]]
			own.com <- membership[u]
			res <- length(which(coms==own.com))
			return(res)
		})
		result[,"z-in"] <- process.community.zscore(values=k.int,membership)
		
		# z-out
		k.int <- sapply(1:length(com.out),function(u)
		{	coms <- com.out[[u]]
			own.com <- membership[u]
			res <- length(which(coms==own.com))
			return(res)
		})
		result[,"z-out"] <- process.community.zscore(values=k.int,membership)
	}
	
	# Dugue-Labatut-Perez
	else if(role.meas=="DLP-p")
	{	# I-int-in
		k.int <- sapply(1:length(com.in),function(u)
		{	coms <- com.in[[u]]
			own.com <- membership[u]
			res <- length(which(coms==own.com))
			return(res)
		})
		result[,"I-int-in"] <- process.community.zscore(values=k.int,membership)
		
		# I-int-out
		k.int <- sapply(1:length(com.out),function(u)
		{	coms <- com.out[[u]]
			own.com <- membership[u]
			res <- length(which(coms==own.com))
			return(res)
		})
		result[,"I-int-out"] <- process.community.zscore(values=k.int,membership)
		
		# I-ext-in
		k.ext <- sapply(1:length(com.in),function(u)
		{	coms <- com.in[[u]]
			own.com <- membership[u]
			res <- length(which(coms!=own.com))
			return(res)
		})
		result[,"I-ext-in"] <- process.community.zscore(values=k.ext,membership)
		
		# I-ext-out
		k.ext <- sapply(1:length(com.out),function(u)
		{	coms <- com.out[[u]]
			own.com <- membership[u]
			res <- length(which(coms!=own.com))
			return(res)
		})
		result[,"I-ext-out"] <- process.community.zscore(values=k.ext,membership)
		
		# H-in
		sd.ext <- sapply(1:length(com.in),function(u)
		{	coms <- com.in[[u]]
			if(length(coms)==0)
				res <- 0
			else
			{	own.com <- membership[u]
				idx <- which(coms!=own.com)
				t <- table(coms[idx])
				res <- sd(t)
				if(is.na(res))
					res <- 0
			}
			return(res)
		})
		result[,"H-in"] <- process.community.zscore(values=sd.ext,membership)
		
		# H-out
		sd.ext <- sapply(1:length(com.out),function(u)
		{	coms <- com.out[[u]]
			if(length(coms)==0)
				res <- 0
			else
			{	own.com <- membership[u]
				idx <- which(coms!=own.com)
				t <- table(coms[idx])
				res <- sd(t)
				if(is.na(res))
					res <- 0
			}
			return(res)
		})
		result[,"H-in"] <- process.community.zscore(values=sd.ext,membership)
		
		# D-in
		n.ext <- sapply(1:length(com.in),function(u)
		{	if(length(coms)==0)
				res <- 0
			else
			{	coms <- com.in[[u]]
				own.com <- membership[u]
				idx <- which(coms!=own.com)
				res <- length(unique(coms[idx]))
			}
			return(res)
		})
		result[,"D-in"] <- process.community.zscore(values=n.ext,membership)
		
		# D-out
		n.ext <- sapply(1:length(com.out),function(u)
		{	coms <- com.out[[u]]
			if(length(coms)==0)
				res <- 0
			else
			{	own.com <- membership[u]
				idx <- which(coms!=own.com)
				res <- length(unique(coms[idx]))
			}
			return(res)
		})
		result[,"D-in"] <- process.community.zscore(values=n.ext,membership)
	}
	
	# overlap Dugue-Labatut-Perez
	else if(role.meas=="DLP-o")
	{	# I-int-in
		#TODO
		
		# I-int-out
		#TODO
		
		# H-int-in
		#TODO
		
		# H-int-out
		#TODO
		
		# D-int-in
		#TODO
		
		# D-int-out
		#TODO
		
		# I-ext-in
		#TODO
		
		# I-ext-out
		#TODO
		
		# H-ext-in
		#TODO
		
		# H-ext-out
		#TODO
		
		# D-ext-in
		#TODO
	
		# D-ext-out
		#TODO
	}
	
	return(result)
}