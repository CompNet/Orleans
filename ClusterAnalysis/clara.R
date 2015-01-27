# Adapter allowing to apply the clara algorithm
# implemented in R.
# 
# Version: 1
# Author: Vincent Labatut 12/2014
#
# setwd("~/eclipse/workspaces/Networks/Orleans/")
# setwd("C:/Eclipse/workspaces/Networks/Orleans/")
# source("ClusterAnalysis/clara.R")
###############################################################################
library("cluster")		# clara algorithm


###############################################################################
# Applies the R version of the clara clustering algorithm.
#
# folder.data: folder containing all input and output files.
# role.meas: type of role measures.
# clust.algo: cluster analysis method.
# comdet.algo: community detection algorithm.
###############################################################################
apply.clara <- function(folder.data, role.meas, clust.algo, comdet.algo)
{	# load the normalized data
	in.file <- get.rolemeas.filename(folder.data,role.meas,norm=TRUE,comdet.algo)
	data <- as.matrix(read.table(in.file))
	
	# apply k-means	
	membership <- iterative.clara(data, ks=c(2:15), criterion="DB", folder.data, role.meas, clust.algo, comdet.algo, trace=TRUE)
	membership <- membership - 1 # number from 0
	
	# record result
	out.file <- get.cluster.filename(folder.data,role.meas,clust.algo,comdet.algo)
	write.table(x=membership, file=out.file, row.names=FALSE, col.names=FALSE)
	
	return(membership)
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
# folder.data: folder containing all input and output files.
# role.meas: type of role measures.
# clust.algo: cluster analysis method.
# comdet.algo: community detection algorithm.
# trace: if TRUE, logs the process
###############################################################################
iterative.clara <- function(data, ks=c(2:15), criterion="ASW", 
		folder.data, role.meas, clust.algo, comdet.algo, trace=FALSE)
{	# init
	best.quality <- NA
	best.result <- NA
	if(criterion=="ASW")
	{	start.time <- Sys.time()
		if(trace) cat("[",format(start.time,"%a %d %b %Y %H:%M:%S"),"] ....Processing distances\n",sep="")
			distances <- dist(data)
		end.time <- Sys.time()
		total.time <- end.time - start.time
		if(trace) cat("[",format(end.time,"%a %d %b %Y %H:%M:%S"),"] ....Process completed in ",format(total.time),"\n",sep="")
	}
	
	# iterate over k	
	quality <- matrix(NA,ncol=2,nrow=length(ks))
	colnames(quality) <- c("k",criterion)
	quality[,1] <- ks
	for(i in 1:length(ks))
	{	# apply clara
		k <- ks[i]
		if(trace) cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] ..Processing k=",k,"\n",sep="")
		
		start.time <- Sys.time()
		if(trace) cat("[",format(start.time,"%a %d %b %Y %H:%M:%S"),"] ....Applying clara for k=",k,"\n",sep="")
			# perform clustering
			membership <- clara(x=data, k=k, metric="euclidean", stand=TRUE,
					# recommended value
					samples=50,
					# NOTE sample size : 2 orders of magnitude smaller than data size, with bounds inf=100 and sup=100000 
					sampsize=max(10^2,min(10^5,nrow(data)/100)), 
					medoids.x=FALSE, keep.data=FALSE, trace=trace, pamLike=TRUE)$clustering
		end.time <- Sys.time()
		total.time <- end.time - start.time
		if(trace) cat("[",format(end.time,"%a %d %b %Y %H:%M:%S"),"] ....Process completed in ",format(total.time),"\n",sep="")
		
		# process quality measure
		start.time <- Sys.time()
		if(trace) cat("[",format(start.time,"%a %d %b %Y %H:%M:%S"),"] ....Process ",criterion," measure for k=",k,"\n",sep="")
			if(criterion=="DB")
			{	qual.value <- index.DB(x=data, cl=membership, centrotypes="centroids")$DB
				#cat("qual.value=",qual.value,"\n",sep="")
				if(is.na(best.quality) | qual.value<best.quality)
				{	best.quality <- qual.value
					best.result <- membership
				}
				#print(best.result)
			}
			else if(criterion=="ASW")
			{	qual.value <- summary(silhouette(x=membership, distances))$avg.width
				if(is.na(best.quality) | qual.value>best.quality)
				{	best.quality <- qual.value
					best.result <- membership
				}
			}
			quality[i,2] <- qual.value
		end.time <- Sys.time()
		total.time <- end.time - start.time
		if(trace) cat("[",format(end.time,"%a %d %b %Y %H:%M:%S"),"] ....Processing completed in ",format(total.time),", ",criterion,"(",k,")=",qual.value,"\n",sep="")
		
		gc()
		if(trace) cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] ..Process completed for k=",k,"\n",sep="")
		if(trace) print(quality)
	}
	
	# record performance
	if(trace) cat("[",format(start.time,"%a %d %b %Y %H:%M:%S"),"] ..Recording the performances\n",sep="")
	perf.file <- get.cluster.perf.filename(folder.data,role.meas,clust.algo,comdet.algo,perf.meas=criterion,plot=FALSE)
	write.table(x=quality,file=perf.file,row.names=FALSE,col.names=TRUE)
	perf.file <- get.cluster.perf.filename(folder.data,role.meas,clust.algo,comdet.algo,perf.meas=criterion,plot=TRUE)
	pdf(file=perf.file,bg="white")
	plot(x=quality,type="o",col="RED")
	dev.off()
	
	return(best.result)
}
