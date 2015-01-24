# Adapter allowing to apply the hclust algorithm
# implemented in the 'fastcluster' R package
# (not the default one from the 'stats' package).
# 
# Version: 1
# Author: Vincent Labatut 12/2014
#
# setwd("~/eclipse/workspaces/Networks/Orleans/")
# setwd("C:/Eclipse/workspaces/Networks/Orleans/")
# source("ClusterAnalysis/hclust.R")
###############################################################################
library("fastcluster")		# (improved) hclust algorithm


###############################################################################
# Applies the R version of the clara clustering algorithm.
#
# folder.data: folder containing all input and output files.
# role.meas: type of role measures.
# clust.algo: cluster analysis method.
# comdet.algo: community detection algorithm.
###############################################################################
apply.hclust <- function(folder.data, role.meas, clust.algo, comdet.algo)
{	# load the normalized data
	in.file <- get.rolemeas.filename(folder.data,role.meas,norm=TRUE,comdet.algo)
	data <- as.matrix(read.table(in.file))
	
	# apply k-means	
	membership <- iterative.hclust(data, ks=c(2:15), criterion="DB", folder.data, role.meas, clust.algo, comdet.algo, trace=TRUE)
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
# criterion: criterion used to select the best number of clusters:
#				- ASW: average Silhouette width
#				- DB: Davies-Bouldin
# folder.data: folder containing all input and output files.
# role.meas: type of role measures.
# clust.algo: cluster analysis method.
# comdet.algo: community detection algorithm.
# trace: if TRUE, logs the process
###############################################################################
iterative.hclust <- function(data, ks=c(2:15), criterion="ASW", 
		folder.data, role.meas, clust.algo, comdet.algo, trace=FALSE)
{	# init
	best.quality <- NA
	best.result <- NA
	if(criterion=="ASW")
	{	start.time <- Sys.time();
		if(trace) cat("[",format(start.time,"%a %d %b %Y %H:%M:%S"),"] ....Processing distances\n",sep="")
			distances <- dist(data)
		end.time <- Sys.time();
		total.time <- end.time - start.time;
		if(trace) cat("[",format(end.time,"%a %d %b %Y %H:%M:%S"),"] ....Process completed in ",format(total.time),"\n",sep="")
	}
	
	# apply hclust
	quality <- matrix(NA,ncol=2,nrow=length(ks))
	colnames(quality) <- c("k",criterion)
	quality[,1] <- ks
	
	# perform clustering
	start.time <- Sys.time();
	if(trace) cat("[",format(start.time,"%a %d %b %Y %H:%M:%S"),"] ....Applying hclust\n",sep="")
		dendrogram <- hclust.vector(X=data, method="centroid", metric="euclidean")
cat("dendrogram height:",dendrogram$height,"\n",sep="")
	end.time <- Sys.time();
	total.time <- end.time - start.time;
	if(trace) cat("[",format(end.time,"%a %d %b %Y %H:%M:%S"),"] ....Process completed in ",format(total.time),"\n",sep="")
	
	# process quality measure
	for(i in 1:length(ks))
	{	start.time <- Sys.time();
		k <- ks[i]
		if(trace) cat("[",format(start.time,"%a %d %b %Y %H:%M:%S"),"] ....Process ",criterion," measure for k=",k,"\n",sep="")
			membership <- cutree(dendrogram,k)
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
