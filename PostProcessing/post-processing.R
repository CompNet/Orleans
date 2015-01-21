# Processes various statistics and generates some
# plots once the clusters have been detected.
# 
# Version: 2
# Author: Vincent Labatut 06/2013,01/2015
#
# setwd("~/eclipse/workspaces/Networks/Orleans/")
# setwd("~/src/Orleans/")
# setwd("C:/Eclipse/workspaces/Networks/Orleans/")
# source("PostProcess/post-process.R")
###############################################################################
source("ClusterAnalysis/cluster-analysis.R")
source("PostProcessing/cluster-stats.R")
source("RoleMeasures/role-measures.R")

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
# Performs various calculations related to the clusters and/or communities
# and/or measures.
#
# folder.data: folder containing all input and output files.
# role.meas: code representing the used role measures (cf. RoleMeasures/role-measures.R)
# clust.algo: cluster analysis method.
# comdet.algo: community detection algorithm.
###############################################################################
post.process <- function(folder.data, role.meas, clust.algo, comdet.algo)
{	# process cluster stats
	process.cluster.stats(folder.data, role.meas, clust.algo, comdet.algo)
	
	# 
}
