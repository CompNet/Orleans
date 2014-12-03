# Adapter allowing to apply the classic k-means algorithm
# implemented in R.
# 
# Version: 1
# Author: Vincent Labatut 07/2014
#
# setwd("~/eclipse/workspaces/Networks/Orleans/")
# setwd("C:/Eclipse/workspaces/Networks/Orleans/")
# source("CommunityDetection/kmeans.R")
###############################################################################

###############################################################################
# Applies the R version of the k-means clustering algorithm.
#
# folder.data: folder containing all input and output files.
# role.meas: type of role measures.
# clust.algo: cluster analysis method.
# comdet.algo: community detection algorithm.
###############################################################################
apply.kmeans <- function(folder.data, role.meas, clust.algo, comdet.algo)
{	# load the normalized data
	in.file <- get.rolemeas.filename(folder.data,role.meas,norm=TRUE,comdet.algo)
	data <- as.matrix(read.table(in.file))
	
	# apply k-means	
	membership <- iterative.kmeans(data, ks=c(2:15), criterion="ASW", trace=TRUE)
	membership <- membership - 1 # number from 0
	
	# record result
	out.file <- get.cluster.filename(folder.data,role.meas,0,clust.algo,comdet.algo)
	write.table(x=membership, file=out.file, row.names=FALSE, col.names=FALSE)
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
iterative.kmeans <- function(data, ks=c(2:15), criterion="ASW", trace=FALSE)
{	# init
	best.quality <- 0
	best.result <- NA
	if(criterion=="ASW")
	{	start.time <- Sys.time();
		if(trace) cat("[",format(start.time,"%a %d %b %Y %H:%M:%S"),"] ....Processing distances\n",sep="")
			distances <- dist(data)
		end.time <- Sys.time();
		total.time <- end.time - start.time;
		if(trace) cat("[",format(end.time,"%a %d %b %Y %H:%M:%S"),"] ....Process completed in ",format(total.time),"\n",sep="")
	}
	
	# apply distributed k-means	
	quality <- matrix(NA,ncol=2,nrow=length(ks))
	quality[,1] <- ks
	for(i in 1:length(ks))
	{	# apply k-means
		k <- ks[i]
		if(trace) cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] ..Processing k=",k,"\n",sep="")
		
		start.time <- Sys.time();
		if(trace) cat("[",format(start.time,"%a %d %b %Y %H:%M:%S"),"] ....Applying k-means for k=",k,"\n",sep="")
			# perform clustering
			membership <- kmeans(x=data, centers=k, trace=trace)$cluster
		end.time <- Sys.time();
		total.time <- end.time - start.time;
		if(trace) cat("[",format(end.time,"%a %d %b %Y %H:%M:%S"),"] ....Process completed in ",format(total.time),"\n",sep="")
		
		# process quality measure
		start.time <- Sys.time();
		if(trace) cat("[",format(start.time,"%a %d %b %Y %H:%M:%S"),"] ....Process ",criterion," measure for k=",k,"\n",sep="")
			if(criterion=="DB")
			{	qual.value <- index.DB(x=data, cl=membership, centrotypes="centroids")$DB
				if(is.na(best.quality) | qual.value<best.quality)
				{	best.quality <- qual.value
					best.result <- membership
				}
			}
			else if(criterion=="ASW")
			{	qual.value <- summary(silhouette(x=membership, distances))$avg.width
				if(is.na(best.quality) | qual.value>best.quality)
				{	best.quality <- qual.value
					best.result <- membership
				}
			}
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
