# Processes statistics regarding the estimated clusters
# using the non-normalized data: average, standard deviation
# and size.
#
# version: 1
# Author: Vincent Labatut 06/2013
#
# setwd("~/eclipse/workspaces/Networks/Orleans/")
# setwd("C:/Eclipse/workspaces/Networks/Orleans/")
# source("PostProcessing/cluster-stats.R")
###############################################################################


###############################################################################
# Returns the standard filename for the cluster means.
#
# folder.data: folder containing all input and output files.
# role.meas: type of role measures.
# n.clust: number of detected clusters (optional).
# clust.algo: cluster analysis algorithm.
# comdet.algo: community detection algorithm.
###############################################################################
get.cluster.means.filename <- function(folder.data, role.meas, clust.algo, comdet.algo, n.clust=0)
{	result <- paste(folder.data,"comdet=",comdet.algo,sep="")
	result <- paste(result,".rolemeas=",role.meas,sep="")
	result <- paste(result,".clust=",clust.algo,sep="")
	if(n.clust>0)
		result <- paste(result,".k=",n.clust,sep="")
	result <- paste(result,".clusters.means.txt",sep="")
	return(result)
}

###############################################################################
# Returns the standard filename for the cluster standard-deviations.
#
# folder.data: folder containing all input and output files.
# role.meas: type of role measures.
# n.clust: number of detected clusters (optional).
# clust.algo: cluster analysis algorithm.
# comdet.algo: community detection algorithm.
###############################################################################
get.cluster.stdevs.filename <- function(folder.data, role.meas, clust.algo, comdet.algo, n.clust=0)
{	result <- paste(folder.data,"comdet=",comdet.algo,sep="")
	result <- paste(result,".rolemeas=",role.meas,sep="")
	result <- paste(result,".clust=",clust.algo,sep="")
	if(n.clust>0)
		result <- paste(result,".k=",n.clust,sep="")
	result <- paste(result,".clusters.stdev.txt",sep="")
	return(result)
}

###############################################################################
# Returns the standard filename for the cluster sizes.
#
# folder.data: folder containing all input and output files.
# role.meas: type of role measures.
# n.clust: number of detected clusters (optional).
# clust.algo: cluster analysis algorithm.
# comdet.algo: community detection algorithm.
###############################################################################
get.cluster.sizes.filename <- function(folder.data, role.meas, clust.algo, comdet.algo, n.clust=0)
{	result <- paste(folder.data,"comdet=",comdet.algo,sep="")
	result <- paste(result,".rolemeas=",role.meas,sep="")
	result <- paste(result,".clust=",clust.algo,sep="")
	if(n.clust>0)
		result <- paste(result,".k=",n.clust,sep="")
	result <- paste(result,".clusters.sizes.txt",sep="")
	return(result)
}

###############################################################################
# Processes the means, standard-deviations and sizes of the detected clusters.
#
# folder.data: folder containing all input and output files.
# role.meas: type of role measures.
# clust.algo: cluster analysis method.
# comdet.algo: community detection algorithm.
###############################################################################
process.cluster.stats <- function(folder.data, role.meas, clust.algo, comdet.algo)
{	# load membership vector
	start.time <- Sys.time();
	cat("[",format(start.time,"%a %d %b %Y %H:%M:%S"),"] Loading membership vector\n",sep="")
		membership.file <- get.cluster.filename(folder.data, role.meas, clust.algo, comdet.algo)
		membership <- as.vector(as.matrix(read.table(membership.file)))
	end.time <- Sys.time();
	total.time <- end.time - start.time;
	cat("[",format(end.time,"%a %d %b %Y %H:%M:%S"),"] Load completed in ",format(total.time),"\n",sep="")
	
	# load raw data (role measures)
	start.time <- Sys.time();
	cat("[",format(start.time,"%a %d %b %Y %H:%M:%S"),"] Loading raw data\n",sep="")
		file.data <- get.rolemeas.filename(folder.data, role.meas, norm=FALSE, comdet.algo)
		data <- as.matrix(read.table(file.data,header=TRUE))
		rolemeas.names <- get.rolemeas.names(role.meas)
	end.time <- Sys.time();
	total.time <- end.time - start.time;
	cat("[",format(end.time,"%a %d %b %Y %H:%M:%S"),"] Load completed in ",format(total.time),"\n",sep="")

	# process means and standard deviations
	cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] Process means, standard deviations and sizes\n",sep="")
		#print(membership)	
		num.clusters <- sort(unique(membership))
		nbr.clusters <- length(num.clusters)
		means <- matrix(nrow=nbr.clusters,ncol=ncol(data))
		rownames(means) <- num.clusters
		colnames(means) <- rolemeas.names
		stdevs <- matrix(nrow=nbr.clusters,ncol=ncol(data))
		rownames(stdevs) <- num.clusters
		colnames(stdevs) <- rolemeas.names
		sizes <- matrix(nrow=nbr.clusters,ncol=1)
		rownames(sizes) <- num.clusters
		colnames(sizes) <- "Size"
		for(i in 1:nbr.clusters)
		{	idx <- which(membership==num.clusters[i])
			sizes[i] <- length(idx)
			cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] ..Size of cluster ",num.clusters[i],": ",sizes[i],"\n",sep="")
			for(j in 1:ncol(data))
			{	means[i,j] <- mean(data[idx,j])
				cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] ....Average role measure ",rolemeas.names[j]," in cluster ",num.clusters[i],": ",means[i,j],"\n",sep="")
				stdevs[i,j] <- sd(data[idx,j])
				cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] ....Standard deviation for role measure ",rolemeas.names[j]," in cluster ",num.clusters[i],": ",stdevs[i,j],"\n",sep="")
			}
		}

	# record data
	cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] Record data\n",sep="")
		mean.file <- get.cluster.means.filename(folder.data, role.meas, clust.algo, comdet.algo)
		write.table(means,mean.file,row.names=TRUE,col.names=TRUE)
		stdev.file <- get.cluster.stdevs.filename(folder.data, role.meas, clust.algo, comdet.algo)
		write.table(stdevs,stdev.file,row.names=TRUE,col.names=TRUE)
		size.file <- get.cluster.sizes.filename(folder.data, role.meas, clust.algo, comdet.algo)
		write.table(sizes,size.file,row.names=TRUE,col.names=TRUE)
}
