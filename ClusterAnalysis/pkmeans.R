# Adapter allowing to apply the classic distributd k-means algoritm
# implemented by Wei-keng Liao.
# http://users.eecs.northwestern.edu/~wkliao/Kmeans/
# 
# Version: 1
# Author: Vincent Labatut 07/2014
#
# setwd("~/eclipse/workspaces/Networks/Orleans/")
# setwd("C:/Eclipse/workspaces/Networks/Orleans/")
# source("CommunityDetection/pkmeans.R")
###############################################################################
library("clusterSim")	# Davies-Bouldin index
library("cluster")		# Silhouette index

###############################################################################
# Applies an external implementation of the distributed k-means clustering algorithm.
#
# folder.data: folder containing all input and output files.
# role.meas: type of role measures.
# clust.algo: cluster analysis method.
# comdet.algo: community detection algorithm.
###############################################################################
apply.pkmeans <- function(folder.data, role.meas, clust.algo, comdet.algo)
{	# load the normalized data
	in.file <- get.rolemeas.filename(folder.data,role.meas,norm=TRUE,comdet.algo)
	temp.file <- paste(in.file,".membership",sep="")
	data <- as.matrix(read.table(in.file))
	
	temp <- iterative.pkmeans(data, ik=2, iter.max=15, pr.proc=TRUE, ignore.covar=TRUE, merge.cls=FALSE)
	membership <- temp$cluster
	
	# record result
	out.file <- get.cluster.filename(folder.data,role.meas,0,clust.algo,comdet.algo)
	write.table(x=best.result, file=out.file, row.names=FALSE, col.names=FALSE)
}

###############################################################################
# We apply iteratively the algorithm for a range of k, and keep the clusters
# leading to the best criterion (average Silhouette width or Davies-Bouldin index).
#
# data: data to process.
# ks: range of k to process.
# criterion: criterion used to select the best number of clusters:
#				- ASW: average Silhouette width
#				- DB: Davies-Bouldin
# trace: if TRUE, logs the process
###############################################################################
iterative.pkmeans(data, ks=c(2:15), criterion="ASW", trace=FALSE)
{	# init
	best.quality <- 0
	best.result <- NA
	
	# apply distributed k-means	
	file.kmeans <- "ClusterAnalysis/pkmeans/omp_main"	# location of the parallel k-means software
	quality <- matrix(NA,ncol=2,nrow=length(ks))
	quality[,1] <- ks
	for(i in 1:length(ks))
	{	# apply k-means
		k <- ks[i]
		if(trace) cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] ..Processing k=",k,"\n",sep="")
		
		start.time <- Sys.time();
		if(trace) cat("[",format(start.time,"%a %d %b %Y %H:%M:%S"),"] ....Applying k-means for k=",k,"\n",sep="")
		# define command
		kmeans.command <- paste(file.kmeans,
				" -i ", getwd(), "/", file.data,
				" -n ", k,
				sep="")
		# perform clustering
		if(trace) cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] ....Executing command ",kmeans.command,"\n",sep="")
		system(command=kmeans.command)
		# move produced membership file (not the others, we don't care)
		file.new <- get.cluster.filename(folder.data,role.meas,n.clust=k,clust.elgo,comdet.algo)
		if(trace) cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] ......Moving file ",temp.file," to ",file.new,"\n",sep="")
		if(file.exists(file.new))
			file.remove(file.new)
		file.rename(from=temp.file,to=file.new)
		end.time <- Sys.time();
		total.time <- end.time - start.time;
		if(trace) cat("[",format(end.time,"%a %d %b %Y %H:%M:%S"),"] ....Process completed in ",format(total.time),"\n",sep="")
		
		# load membership vector
		start.time <- Sys.time();
		if(trace) cat("[",format(start.time,"%a %d %b %Y %H:%M:%S"),"] ....Load membership vector (",file.new,")\n",sep="")
		membership <- as.matrix(read.table(file.new))[,2] + 1	# the clusters are numbered from zero 
		end.time <- Sys.time();
		total.time <- end.time - start.time;
		if(trace) cat("[",format(end.time,"%a %d %b %Y %H:%M:%S"),"] ....Load completed in ",format(total.time),"\n",sep="")
		
		# process quality measure
		start.time <- Sys.time();
		if(trace) cat("[",format(start.time,"%a %d %b %Y %H:%M:%S"),"] ....Process Davies-Bouldin measure for k=",k,"\n",sep="")
		if(criterion=="DB")
		{	qual.value <- index.DB(x=data, cl=membership, centrotypes="centroids")$DB
			if(is.na(best.quality) | qual.value<best.quality)
			{	best.quality <- qual.value
				best.result <- min.res
			}
		}
		else if(criterion=="ASW")
		{	qual.value <- summary(silhouette(x=membership, dist(x)))$avg.width
			if(is.na(best.quality) | qual.value>best.quality)
			{	best.quality <- qual.value
				best.result <- min.res
			}
		}
		qual.value <- index.DB(x=data, cl=membership, centrotypes="centroids")$DB
		quality[i,2] <- qual.value
		end.time <- Sys.time();
		total.time <- end.time - start.time;
		if(trace) cat("[",format(end.time,"%a %d %b %Y %H:%M:%S"),"] ....Processing completed in ",format(total.time),", DB(",k,")=",qual.value,"\n",sep="")
		
		gc()
		if(trace) cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] ..Process completed for k=",k,"\n",sep="")
		if(trace) print(quality)
	}
	
	return(best.result)
}
