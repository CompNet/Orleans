# Applies the global k-means algorithm, which
# is deterministic and allows estimating stable clusters. 
# It can rely on any k-means implementation, be it the 
# distributed one directly used in p06-clustering.dkm.R, 
# or some R standard library.
#
# Global k-means algorithm described in:
# http://www.cs.uoi.gr/~arly/papers/PR2003.pdf
# Both the regular and fast versions are implemented here,
# but not the k-d tree variant. Note the regular
# version might be very slow, depending on the number of 
# instances.
#
# version: 1
# Author: Vincent Labatut 02/2014
#
# setwd("~/eclipse/workspaces/Networks/Orleans/")
# setwd("C:/Eclipse/workspaces/Networks/Orleans/")
# source("Clustering/gkmeans.R")
###############################################################################
library("clusterSim")	# Davies-Bouldin index
library("cluster")		# Silhouette index


# x:			data points
# fast:			use regular (FALSE) or fast (TRUE) global k-means
# k.bounds:		inf and sup bounds for the tested numbers of clusters
# criterion:	criterion used to select the best number of clusters. Either ASW (average Silhouette width) or DB (Davies-Bouldin)
# trace:		if TRUE, logs the process
gkmeans <- function(x, fast=TRUE, k.bounds=c(2,15), criterion="ASW", trace=FALSE)
{	# process the initial center (means of data attributes)
	centers <- matrix(ncol=ncol(x),nrow=1)
	centers[1,] <- apply(x, 2, mean)
	min.clusters <- rep(1,nrow(x))
	best.quality <- 0
	best.result <- NA
	
	# iteratively process the clusters using the global k-means principle
	for(k in k.bounds[1]:k.bounds[2])
	{	if(trace) cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] ..Processing k=",k,"\n",sep="")
		
		# init candidate centers for the new cluster
		if(fast)
		{	start.time <- Sys.time();
				if(trace) cat("[",format(start.time,"%a %d %b %Y %H:%M:%S"),"] ....Fast version: estimating the best instance to be used as a cluster center\n",sep="")
				
				# squared distances to centers from the previous iteration (k-1)
				if(trace) cat("[",format(start.time,"%a %d %b %Y %H:%M:%S"),"] ......Processing distances with previous centers\n",sep="")
				prev.centers <- matrix(nrow=nrow(x),ncol=ncol(x))
				prev.centers[1:nrow(x),] <- centers[min.clusters,]
				prev.distances <- matrix(nrow=nrow(x),ncol=1)
				prev.distances[1:nrow(x),] <- apply((x-prev.centers)^2, 1 ,sum)
				
				# compare with the distances to each instance
				if(trace) cat("[",format(start.time,"%a %d %b %Y %H:%M:%S"),"] ......Comparing with each instance\n",sep="")
				max.b <- -1
				max.i <- -1
				for(i in 1:nrow(x))
				{	# squared distances to centers from the considered instance
					inst.distances <- matrix(nrow=nrow(x),ncol=1)
					inst.distances[1:nrow(x),] <- apply(scale(x, center=x[i,], scale=FALSE)^2, 1 ,sum)
					# process equation (2) from the paper
					val <- prev.distances - inst.distances
					val[val<0] <- 0
					b <- sum(val)
					if(b>max.b)
					{	max.b <- b
						max.i <- i
					}
				}
				
				# keep the instance with maximal b (largest guaranteed reduction in error)
				if(trace) cat("[",format(start.time,"%a %d %b %Y %H:%M:%S"),"] ......Selected instance: ",max.i,"\n",sep="")
				candidates <- matrix(ncol=ncol(x),nrow=1)
				candidates[1,] <- x[max.i,]
			
			end.time <- Sys.time();
			total.time <- end.time - start.time;
			if(trace) cat("[",format(end.time,"%a %d %b %Y %H:%M:%S"),"] ....Process completed in ",format(total.time),"\n",sep="")
		} else
			candidates <- x
		
		# init min centers
		min.centers <- matrix(ncol=ncol(x),nrow=k)
		min.error <- .Machine$double.xmax
		min.res <- NA
		
		# apply k-means using each candidate instance as a potential center for the next cluster
		for(i in 1:nrow(candidates))
		{	start.time <- Sys.time();
				if(trace) cat("[",format(start.time,"%a %d %b %Y %H:%M:%S"),"] ....Applying k-means for k=",k," and candidate ",i,"/",nrow(candidates),"\n",sep="")
				if(any(apply(centers, 1, function(x, want) isTRUE(all.equal(x, want)), candidates[i,])))
				{	if(trace) cat("[",format(start.time,"%a %d %b %Y %H:%M:%S"),"] ......This instance is already a center, so we skip it\n",sep="")
				}
				else
				{	# set up centers
					c <- rbind(centers[1:(k-1),],candidates[i,])
					# apply k-means
					res <- kmeans(x=x, centers=c, iter.max=10, algorithm=c("Hartigan-Wong"))
					ss <- res$tot.withinss
					# possibly update min variables
					if(ss<min.error)
					{	min.error <- ss
						min.centers <- res$centers
						min.clusters <- res$cluster
						min.res <- res
					}
				}
			end.time <- Sys.time();
			total.time <- end.time - start.time;
			if(trace) cat("[",format(end.time,"%a %d %b %Y %H:%M:%S"),"] ....Process completed in ",format(total.time)," ss=",ss,"\n",sep="")
		}
		
		# updates centers using the min ones found during the last iteration
		centers <- min.centers
		if(trace) cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] ..New centers for k=",k,"\n",sep="")
		if(trace) print(centers)
		
		# process quality measure
		start.time <- Sys.time();
			if(trace) cat("[",format(start.time,"%a %d %b %Y %H:%M:%S"),"] ....Process quality measure for k=",k,"\n",sep="")
			if(criterion=="DB")
			{	qual.value <- index.DB(x=data, cl=min.clusters, centrotypes="centroids")$DB
				if(qual.value<best.quality)
				{	best.quality <- qual.value
					best.result <- min.res
				}
			}
			else if(criterion=="ASW")
			{	qual.value <- summary(silhouette(x=min.clusters, dist(x)))$avg.width
				if(qual.value>best.quality)
				{	best.quality <- qual.value
					best.result <- min.res
				}
			}
		end.time <- Sys.time();
		total.time <- end.time - start.time;
		if(trace) cat("[",format(end.time,"%a %d %b %Y %H:%M:%S"),"] ....Processing completed in ",format(total.time),", quality(",k,")=",qual.value,"\n",sep="")
		
		if(trace) cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] ..Process completed for k=",k,"\n",sep="")
	}
	
	# set up the final result
	return(best.result)
}
