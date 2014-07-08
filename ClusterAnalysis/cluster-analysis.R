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
###############################################################################
get.cluster.filename <- function(algo, n.clust)
{	resultat <- paste("clusters.",algo,".k",n.clust,".txt",sep="")
	return(resultat)
}

###############################################################################
# Applies the specified clustering algorithm
# to the data contained in the specified folder.
#
# folder.data: folder containing all input and output files.
# algo: cluster analysis method.
# role.meas: code representing the used role measures (cf. RoleMeasures/role-measures.R)
###############################################################################
detect.clusters <- function(folder.data, algo, role.meas)
{	# possibly normalize data
	normalize.data(folder.data, role.meas)
		
	# apply clustering method
	if(algo=="kmeans")
		apply.kmeans(folder.data, algo, role.meas)
	else if(algo=="xmeans")
		apply.gkmeans(folder.data, algo, role.meas)
	else if(algo=="xmeans")
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
###############################################################################
normalize.data <- function(folder.data, role.meas)
{	file.input <- get.rolemeas.filename(role.meas,norm=FALSE)
	path.input <- paste(folder.data,file.input,sep="")
	
	# we normalize only if the file doesn't aleady exist
	if(!file.exists(path.input))
	{	# load the data
		start.time <- Sys.time();
		cat("[",format(start.time,"%a %d %b %Y %H:%M:%S"),"] Loading raw data...\n",sep="")
			x <- as.matrix(read.table(path.input))
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
			file.output <- get.rolemeas.filename(role.meas,norm=TRUE)
			path.output <- paste(folder.data,file.output,sep="")
			write.table(x=x,file=path.output,row.names=FALSE,col.names=FALSE)
		end.time <- Sys.time();
		total.time <- end.time - start.time;
		cat("[",format(end.time,"%a %d %b %Y %H:%M:%S"),"] Recording completed in ",total.time,"\n",sep="")
	}
}
