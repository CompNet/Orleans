# Processes various calculations related to 
# cluster analysis.
# 
# Version: 2
# Author: Vincent Labatut 06/2013, 07/2014
#
# setwd("~/eclipse/workspaces/Networks/Orleans/")
# setwd("~/src/Orleans/")
# setwd("C:/Eclipse/workspaces/Networks/Orleans/")
# source("ClusterAnalysis/cluster-analysis.R")
###############################################################################
source("ClusterAnalysis/clara.R")
source("ClusterAnalysis/gkmeans.R")
source("ClusterAnalysis/hclust.R")
source("ClusterAnalysis/kmeans.R")
source("ClusterAnalysis/pkmeans.R")
source("ClusterAnalysis/xmeans.R")


###############################################################################
# Returns the standard cluster filename, i.e. the name of the file
# containing the cluster membership associated to each node.
# Note clusters are numbered starting from 0.
#
# folder.data: folder containing all input and output files.
# role.meas: type of role measures.
# clust.algo: cluster analysis algorithm.
# comdet.algo: community detection algorithm.
# n.clust: number of detected clusters (optional).
###############################################################################
get.cluster.filename <- function(folder.data, role.meas, clust.algo, comdet.algo, n.clust=0)
{	result <- paste(folder.data,"comdet=",comdet.algo,sep="")
	result <- paste(result,".rolemeas=",role.meas,sep="")
	result <- paste(result,".clust=",clust.algo,sep="")
	if(n.clust>0)
		result <- paste(result,".k=",n.clust,sep="")
	result <- paste(result,".clusters.txt",sep="")
	return(result)
}

###############################################################################
# Returns the standard filename for the performances of a clustering algorithm.
#
# folder.data: folder containing all input and output files.
# role.meas: type of role measures.
# clust.algo: cluster analysis algorithm.
# comdet.algo: community detection algorithm.
# perf.meas: performance measure used to assess the clusters.
# plot: wheter the file should be a PDF (TRUE) or just text (FALSE)
###############################################################################
get.cluster.perf.filename <- function(folder.data, role.meas, clust.algo, comdet.algo, perf.meas, plot=FALSE)
{	result <- paste(folder.data,"comdet=",comdet.algo,sep="")
	result <- paste(result,".rolemeas=",role.meas,sep="")
	result <- paste(result,".clust=",clust.algo,sep="")
	result <- paste(result,".perf=",perf.meas,sep="")
	
	if(plot)
		ext <- "pdf"
	else
		ext <- "txt"
	result <- paste(result,".",ext,sep="")
	
	return(result)
}

###############################################################################
# Applies the specified clustering algorithm
# to the data contained in the specified folder.
#
# folder.data: folder containing all input and output files.
# role.meas: code representing the used role measures (cf. RoleMeasures/role-measures.R)
# clust.algo: cluster analysis method.
# comdet.algo: community detection algorithm.
###############################################################################
detect.clusters <- function(folder.data, role.meas, clust.algo, comdet.algo)
{	# possibly normalize data
	normalize.data(folder.data, role.meas, clust.algo, comdet.algo)
		
	# apply clustering method
	if(clust.algo=="clara")
		result <- apply.clara(folder.data, role.meas, clust.algo, comdet.algo)
	else if(clust.algo=="hclust")
		result <- apply.hclust(folder.data, role.meas, clust.algo, comdet.algo)
	else if(clust.algo=="kmeans")
		result <- apply.kmeans(folder.data, role.meas, clust.algo, comdet.algo)
	else if(clust.algo=="pkmeans")
		result <- apply.pkmeans(folder.data, role.meas, clust.algo, comdet.algo)
	else if(clust.algo=="xmeans")
		result <- apply.xmeans(folder.data, role.meas, clust.algo, comdet.algo)
	else if(clust.algo=="gkmeans")
		result <- apply.gkmeans(folder.data, role.meas, clust.algo, comdet.algo)
	else if(clust.algo=="fgkmeans")
		result <- apply.gkmeans(folder.data, role.meas, clust.algo, comdet.algo)
	else if(clust.algo=="gpkmeans")
		result <- apply.gkmeans(folder.data, role.meas, clust.algo, comdet.algo)
	else if(clust.algo=="fgpkmeans")
		result <- apply.gkmeans(folder.data, role.meas, clust.algo, comdet.algo)
}
	
###############################################################################
# Normalizes the previously processed role measures, in order
# to get better results when doing the cluster analysis.
# We center/reduce the data.
#
# Note: we don't re-process the data if the normalized file
# already exists.
#
# folder.data: folder containing all input and output files.
# role.meas: code representing the used role measures (cf. RoleMeasures/role-measures.R)
# clust.algo: cluster analysis method.
# comdet.algo: community detection algorithm.
###############################################################################
normalize.data <- function(folder.data, role.meas, clust.algo, comdet.algo)
{	in.file <- get.rolemeas.filename(folder.data, role.meas, norm=FALSE, comdet.algo)
	out.file <- get.rolemeas.filename(folder.data, role.meas, norm=TRUE, comdet.algo)
	
	# we normalize only if the file doesn't aleady exist
	if(!file.exists(out.file))
	{	# load the data
		start.time <- Sys.time()
		cat("[",format(start.time,"%a %d %b %Y %H:%M:%S"),"] Loading raw data...\n",sep="")
			x <- as.matrix(read.table(in.file,header=TRUE))
		end.time <- Sys.time()
		total.time <- end.time - start.time
		cat("[",format(end.time,"%a %d %b %Y %H:%M:%S"),"] Load completed in ",total.time,"\n",sep="")
		
		# normalize
		start.time <- Sys.time()
		cat("[",format(start.time,"%a %d %b %Y %H:%M:%S"),"] Normalizing data...\n",sep="")
			for(c in 1:ncol(x))
			{	cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] ..Processing col.",c,"\n",sep="")
				average <- mean(x[,c])
				#print(x)
				stdev <- sd(x[,c])
				cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] ....Normalizing col.",c,": avg=",average," stdev=",stdev,"\n",sep="")
				x[,c] <- (x[,c] - average) / stdev
			}
		end.time <- Sys.time()
		total.time <- end.time - start.time
		cat("[",format(end.time,"%a %d %b %Y %H:%M:%S"),"] Normalization completed in ",total.time,"\n",sep="")
		
		# record
		start.time <- Sys.time()
		cat("[",format(start.time,"%a %d %b %Y %H:%M:%S"),"] Recording normalized data\n",sep="")
			write.table(x=x,file=out.file,row.names=FALSE,col.names=FALSE)
		end.time <- Sys.time()
		total.time <- end.time - start.time
		cat("[",format(end.time,"%a %d %b %Y %H:%M:%S"),"] Recording completed in ",total.time,"\n",sep="")
	}
}

# Test
#detect.clusters(folder.data="data/socap/", role.meas="dummy", clust.algo="fgpkmeans", comdet.algo="dummy")
#detect.clusters(folder.data="data/", role.meas="dummy", clust.algo="fgpkmeans", comdet.algo="dummy")
