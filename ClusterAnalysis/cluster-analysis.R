# Processes various calculations related to 
# cluster analysis.
# 
# Version: 2
# Author: Vincent Labatut 06/2013,07/2014
#
# setwd("~/eclipse/workspaces/Networks/Orleans/")
# setwd("C:/Eclipse/workspaces/Networks/Orleans/")
# source("GenerateData/cluster-analysis.R")
###############################################################################
source("RoleMeasures/role-measures.R")

###############################################################################
# Returns the standard cluster filename.
#
# folder.data: folder containing all input and output files.
# role.meas: type of role measures.
# n.clust: number of detected clusters (optional).
# clust.algo: cluster analysis algorithm.
# comdet.algo: community detection algorithm.
###############################################################################
get.cluster.filename <- function(folder.data, role.meas, n.clust=0, clust.algo, comdet.algo)
{	result <- paste(folder.data,"comdet=",comdet.algo,sep="")
	result <- paste(result,".rolemeas=",role.meas,sep="")
	result <- paste(result,".clusters=",clust.algo,sep="")
	if(k>0)
		result <- paste(result,".k",n.clust,sep="")
	result <- paste(result,"clusters.txt",sep="")
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
	if(algo=="kmeans")
		apply.kmeans(folder.data, algo, role.meas)
	else if(algo=="dkmeans")
		apply.dkmeans(folder.data, algo, role.meas)
	else if(algo=="xmeans")
		apply.xmeans(folder.data, algo, role.meas)
	else if(algo=="gkmeans")
		apply.gkmeans(folder.data, algo, role.meas)
	else if(algo=="fgkmeans")
		apply.fgkmeans(folder.data, algo, role.meas)
}

###############################################################################
# Normalizes the previously processed role measures, in order
# to get better results when doing the cluster analysis.
# We center/reduce the data.
#
# Note: we don't re-process the data of the normalized file
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
	if(!file.exists(path.input))
	{	# load the data
		start.time <- Sys.time();
		cat("[",format(start.time,"%a %d %b %Y %H:%M:%S"),"] Loading raw data...\n",sep="")
			x <- as.matrix(read.table(in.file))
		end.time <- Sys.time();
		total.time <- end.time - start.time;
		cat("[",format(end.time,"%a %d %b %Y %H:%M:%S"),"] Load completed in ",total.time,"\n",sep="")
		
		# normalize
		start.time <- Sys.time();
		cat("[",format(start.time,"%a %d %b %Y %H:%M:%S"),"] Normalizing data...\n",sep="")
			for(c in 1:ncol(x))
			{	cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] ..Processing col.",c,"\n",sep="")
				average <- mean(x[,c])
				stdev <- sd(x[,c])
				cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] ....Normalizing col.",c,": avg=",average," stdev=",stdev,"\n",sep="")
				x[,c] <- (x[,c] - average) / stdev
			}
		end.time <- Sys.time();
		total.time <- end.time - start.time;
		cat("[",format(end.time,"%a %d %b %Y %H:%M:%S"),"] Normalization completed in ",total.time,"\n",sep="")
		
		# record
		start.time <- Sys.time();
		cat("[",format(start.time,"%a %d %b %Y %H:%M:%S"),"] Recording normalized data\n",sep="")
			write.table(x=x,file=out.file,row.names=FALSE,col.names=FALSE)
		end.time <- Sys.time();
		total.time <- end.time - start.time;
		cat("[",format(end.time,"%a %d %b %Y %H:%M:%S"),"] Recording completed in ",total.time,"\n",sep="")
	}
}
