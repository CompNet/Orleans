# Applies the Global k-means algorithm, which
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
# source("ClusterAnalysis/gkmeans.R")
###############################################################################
library("clusterSim")	# Davies-Bouldin index
library("cluster")		# Silhouette index


###############################################################################
# Adapter function allowing to use global k-means from our own scripts.
#
# folder.data: folder containing all input and output files.
# role.meas: type of role measures.
# clust.algo: cluster analysis method.
# comdet.algo: community detection algorithm.
###############################################################################
apply.gkmeans <- function(folder.data, role.meas, clust.algo, comdet.algo)
{	# load the normalized data
	in.file <- get.rolemeas.filename(folder.data,role.meas,norm=TRUE,comdet.algo)
	data <- as.matrix(read.table(in.file))
	
	# apply global k-means
	if(clust.algo=="fgkmeans" || clust.algo=="fgpkmeans")
		fast = TRUE
	else
		fast = FALSE
	if(clust.algo=="gpkmeans" || clust.algo=="fgpkmeans")
		parallel = TRUE
	else
		parallel = FALSE
	temp <- gkmeans(x=data, fast=fast, parallel=parallel, k.bounds=c(2,15), criterion="DB", 
			folder.data, file.data=in.file, role.meas, clust.algo, comdet.algo, trace=TRUE)
	membership <- temp$cluster - 1 # number from 0
	
	# record result
	out.file <- get.cluster.filename(folder.data,role.meas,clust.algo,comdet.algo)
	write.table(x=membership, file=out.file, row.names=FALSE, col.names=FALSE)
	
	return(membership)
}

###############################################################################
# Applies the global k-means clustering method.
#
# x: data points
# fast: use regular (FALSE) or fast (TRUE) global k-means
# parallel: use regular R (FALSE) or parallel (TRUE) version of k-means.
# k.bounds: lower and upper bounds for the tested numbers of clusters
# criterion: criterion used to select the best number of clusters:
#				- ASW: average Silhouette width
#				- DB: Davies-Bouldin
# folder.data: main folder (needed for external programs)
# file.data: input file (already loaded, but path needed for external programs)
# role.meas: name of the role measure (needed for external programs)
# clust.ago: name of the clustering algorithm (needed for external programs)
# comdet.ago: name of the community detection algorithm (needed for external programs)
# trace: if TRUE, logs the process
###############################################################################
gkmeans <- function(x, fast=TRUE, parallel=FALSE, k.bounds=c(2,15), criterion="ASW", 
		folder.data, file.data, role.meas, clust.algo, comdet.algo, trace=FALSE)
{	ks <- k.bounds[1]:k.bounds[2]
	quality <- matrix(NA,ncol=2,nrow=length(ks))
	colnames(quality) <- c("k",criterion)
	quality[,1] <- ks
	
	# process the initial center (means of data attributes)
	centers <- matrix(ncol=ncol(x),nrow=1)
	centers[1,] <- apply(x, 2, mean)
	min.clusters <- rep(1,nrow(x))
	best.quality <- NA
	best.result <- NA
	if(criterion=="ASW")
	{	start.time <- Sys.time();
		if(trace) cat("[",format(start.time,"%a %d %b %Y %H:%M:%S"),"] ....Processing distances\n",sep="")
			distances <- dist(x)
		end.time <- Sys.time();
		total.time <- end.time - start.time;
		if(trace) cat("[",format(end.time,"%a %d %b %Y %H:%M:%S"),"] ....Process completed in ",format(total.time),"\n",sep="")
	}
	
	# iteratively process the clusters using the global k-means principle
	for(k in ks)
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
				if(trace
#						& i %% 1000 == 0
				) cat("[",format(start.time,"%a %d %b %Y %H:%M:%S"),"] ....Applying k-means for k=",k," and candidate ",i,"/",nrow(candidates),"\n",sep="")
				if(any(apply(centers, 1, function(x, want) isTRUE(all.equal(x, want)), candidates[i,])))
				{	if(trace) cat("[",format(start.time,"%a %d %b %Y %H:%M:%S"),"] ......This instance (",i,")is already a center, so we skip it\n",sep="")
				}
				else
				{	# set up centers
					ctrs <- rbind(centers[1:(k-1),],candidates[i,])
					# apply k-means
					res <- inner.apply.kmeans(data=x, centers=ctrs, folder.data, file.data, role.meas, clust.algo, comdet.algo, trace)
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
			if(trace 
#					& i %% 1000 == 0
			) cat("[",format(end.time,"%a %d %b %Y %H:%M:%S"),"] ....Process completed in ",format(total.time)," ss=",ss,"\n",sep="")
		}
		
		# updates centers using the min ones found during the last iteration
		centers <- min.centers
		if(trace) cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] ..New centers for k=",k,"\n",sep="")
		if(trace) print(centers)
		
		# process quality measure
		start.time <- Sys.time();
		if(trace) cat("[",format(start.time,"%a %d %b %Y %H:%M:%S"),"] ....Process ",criterion," measure for k=",k,"\n",sep="")
			if(criterion=="DB")
			{	qual.value <- index.DB(x=x, cl=min.clusters, centrotypes="centroids")$DB
				if(is.na(best.quality) | qual.value<best.quality)
				{	best.quality <- qual.value
					best.result <- min.res
				}
			}
			else if(criterion=="ASW")
			{	qual.value <- summary(silhouette(x=min.clusters, distances))$avg.width
				if(is.na(best.quality) | qual.value>best.quality)
				{	best.quality <- qual.value
					best.result <- min.res
				}
			}
			quality[k-1,2] <- qual.value
		end.time <- Sys.time();
		total.time <- end.time - start.time;
		if(trace) cat("[",format(end.time,"%a %d %b %Y %H:%M:%S"),"] ....Processing completed in ",format(total.time),", ",criterion,"(",k,")=",qual.value,"\n",sep="")
		
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
	
	# set up the final result
	return(best.result)
}

###############################################################################
# Applies the basic k-means clustering method in the context of global
# k-means.
#
# data: objects to cluster (already loaded)
# centers: initial centers (coordinates)
# folder.data: main folder (needed for external programs)
# file.data: input file (already loaded, but path needed for external programs)
# role.meas: name of the role measure (needed for external programs)
# clust.ago: name of the clustering algorithm (needed for external programs)
# comdet.ago: name of the community detection algorithm (needed for external programs)
# trace: if TRUE, logs the process
###############################################################################
inner.apply.kmeans <- function(data, centers, folder.data, file.data, role.meas, clust.algo, comdet.algo, trace)
{	# regular k-means
	if(clust.algo=="fgkmeans" || clust.algo=="gkmeans")
		result <- kmeans(x=data, centers=centers, iter.max=10, algorithm=c("Hartigan-Wong"))
	
	# parallel k-means
	else
	{	start.time <- Sys.time();
			k <- nrow(centers)
			if(trace) cat("[",format(start.time,"%a %d %b %Y %H:%M:%S"),"] ........Applying Parallel k-means for k=",k,"\n",sep="")
			
			# record the centers in a specific file (to be removed later)
			file.centers <- paste(file.data,".centers",sep="")
			write.table(x=centers,file=file.centers,row.names=FALSE,col.names=FALSE)
			
			# location of the parallel k-means software
			file.kmeans <- "ClusterAnalysis/pkmeans/omp_main"	
			
			# define command
			kmeans.command <- paste(file.kmeans,
					" -i ", getwd(), "/", file.data,
					" -z ", getwd(), "/", file.centers,
					" -n ", k,
					sep="")
			
			# perform clustering
			if(trace) cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] ..........Executing command ",kmeans.command,"\n",sep="")
			system(command=kmeans.command)
	
			# move produced cluster file
			temp.cluster.file <- paste(file.data,".membership",sep="")
			file.new <- get.cluster.filename(folder.data,role.meas,clust.algo,comdet.algo,n.clust=k)
			if(trace) cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] ..........Moving file ",temp.cluster.file," to ",file.new,"\n",sep="")
			if(file.exists(file.new))
				file.remove(file.new)
			file.rename(from=temp.cluster.file,to=file.new)
		end.time <- Sys.time();
		total.time <- end.time - start.time;
		if(trace) cat("[",format(end.time,"%a %d %b %Y %H:%M:%S"),"] ........Process completed in ",format(total.time),"\n",sep="")
		
		# load then remove centers file
		start.time <- Sys.time();
			temp.center.file <- paste(file.data,".cluster_centres",sep="")
			if(trace) cat("[",format(start.time,"%a %d %b %Y %H:%M:%S"),"] ........Load/remove centers file (",temp.center.file,")\n",sep="")
			centers2 <- as.matrix(read.table(file=temp.center.file))
			file.remove(temp.center.file)
		end.time <- Sys.time();
		total.time <- end.time - start.time;
		if(trace) cat("[",format(end.time,"%a %d %b %Y %H:%M:%S"),"] ........Load completed in ",format(total.time),"\n",sep="")
		
		# load membership vector
		start.time <- Sys.time();
			if(trace) cat("[",format(start.time,"%a %d %b %Y %H:%M:%S"),"] ........Load membership vector (",file.new,")\n",sep="")
			membership <- as.matrix(read.table(file.new))[,2] + 1	# the clusters are numbered from zero 
		end.time <- Sys.time();
		total.time <- end.time - start.time;
		if(trace) cat("[",format(end.time,"%a %d %b %Y %H:%M:%S"),"] ........Load completed in ",format(total.time),"\n",sep="")
		
		# process within-cluster sum of squares
		start.time <- Sys.time();
			if(trace) cat("[",format(start.time,"%a %d %b %Y %H:%M:%S"),"] ........Process within-cluster sum of squares\n",sep="")
			temp <- sapply(1:k, function(num)
			{	idx <- which(membership==num)
				if(length(idx)>1)
				{	#print(idx)					
					mean.coord <- apply(data[idx,], 2, mean)
					wcs <- apply(data[idx,], 1, function(x) (x - mean.coord)^2)
					total <- sum(wcs)
				}
				else
					total <- 0
				return(total)
			})
			wcss <- sum(temp)
		end.time <- Sys.time();
		total.time <- end.time - start.time;
		if(trace) cat("[",format(end.time,"%a %d %b %Y %H:%M:%S"),"] ........Process completed in ",format(total.time)," (",wcss,")\n",sep="")
		
		result = list(cluster=membership,centers=centers2,tot.withinss=wcss)
	}
	
	return(result)
}
