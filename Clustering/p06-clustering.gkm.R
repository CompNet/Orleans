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
# source("src/p06-clustering.gkm.R")
###############################################################################
library("clusterSim")	# Davies-Bouldin index
library("cluster")		# Silhouette index


###############################################################################
# setup files
###############################################################################
folder.data <- "Data/"	
file.kmeans <- "~/Downloads/Simple_Kmeans/omp_main"		# TODO k-means executable file, adapt to your own case
file.kmeans <- NA										# TODO or NA to use the standard k-means implementation from R
ks <- c(2:15)											# TODO values of k to be tried, you can change that
fast <- TRUE											# TODO whether the regular or fast global k-means version is applied


###############################################################################
# load normalized data
###############################################################################
start.time <- Sys.time();
cat("[",format(start.time,"%a %d %b %Y %H:%M:%S"),"] Loading normalized data\n",sep="")
	file.data <- paste(folder.data,"rolemeasures.normalized.txt",sep="")
	data <- as.matrix(read.table(file.data))
end.time <- Sys.time();
total.time <- end.time - start.time;
cat("[",format(end.time,"%a %d %b %Y %H:%M:%S"),"] Load completed in ",total.time,"\n",sep="")


###############################################################################
# apply algorithm for each specified k
###############################################################################
all.centers <- list()
quality <- c()
centers <- matrix(ncol=ncol(data),nrow=length(ks)+1)
# process initial center (means of data attributes)
initial.center <- apply(data, 2, mean)
centers[1,] <- initial.center
all.centers[[1]] <- initial.center
min.clusters <- rep(1,nrow(data))
# iteratively process the clusters using the global k-means principle
for(k in ks)
{	cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] ..Processing k=",k,"\n",sep="")

	# init candidate centers for the new cluster
	if(fast)
	{	start.time <- Sys.time();
		cat("[",format(start.time,"%a %d %b %Y %H:%M:%S"),"] ....Fast version: estimating the best instance to be used as a cluster center\n",sep="")
		
			# squared distances to centers from the previous iteration (k-1)
			cat("[",format(start.time,"%a %d %b %Y %H:%M:%S"),"] ......Processing distances with previous centers\n",sep="")
			prev.centers <- matrix(nrow=nrow(data),ncol=ncol(data))
			prev.centers[1:nrow(data),] <- centers[min.clusters,]
			prev.distances <- matrix(nrow=nrow(data),ncol=1)
			prev.distances[1:nrow(data),] <- apply((data-prev.centers)^2, 1 ,sum)
			
			# compare with the distances to each instance
			cat("[",format(start.time,"%a %d %b %Y %H:%M:%S"),"] ......Comparing with each instance\n",sep="")
			max.b <- -1
			max.i <- -1
			for(i in 1:nrow(data))
			{	# squared distances to centers from the considered instance
				inst.distances <- matrix(nrow=nrow(data),ncol=1)
				inst.distances[1:nrow(data),] <- apply(scale(data, center=data[i,], scale=FALSE)^2, 1 ,sum)
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
			cat("[",format(start.time,"%a %d %b %Y %H:%M:%S"),"] ......Selected instance: ",max.i,"\n",sep="")
			candidates <- matrix(ncol=ncol(data),nrow=1)
			candidates[1,] <- data[max.i,]
			
		end.time <- Sys.time();
		total.time <- end.time - start.time;
		cat("[",format(end.time,"%a %d %b %Y %H:%M:%S"),"] ....Process completed in ",total.time,"\n",sep="")
	} else
		candidates <- data
	
	# init min centers
	min.centers <- matrix(ncol=ncol(data),nrow=k)
	min.error <- .Machine$double.xmax

	# apply k-means using each instance as a potential center for the next cluster
	for(i in 1:nrow(candidates))
	{	start.time <- Sys.time();
		cat("[",format(start.time,"%a %d %b %Y %H:%M:%S"),"] ....Applying k-means for k=",k," and candidate ",i,"/",nrow(candidates),"\n",sep="")
			# set up centers
			c <- rbind(centers[1:(k-1),],candidates[i,])
			# apply k-means
			res <- kmeans(x=data, centers=c, iter.max=10, algorithm=c("Hartigan-Wong"))
			ss <- res$tot.withinss
			# possibly update min variables
			if(ss<min.error)
			{	min.error <- ss
				min.centers <- res$centers
				min.clusters <- res$cluster
			}
		end.time <- Sys.time();
		total.time <- end.time - start.time;
		cat("[",format(end.time,"%a %d %b %Y %H:%M:%S"),"] ....Process completed in ",total.time," ss=",ss,"\n",sep="")
	}
	
	# updates centers using the min ones found during the last iteration
	centers[1:k,] <- min.centers
	all.centers[[k]] <- min.centers
	cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] ..New centers for k=",k,"\n",sep="")
	print(centers)
	
	# process quality measure
	start.time <- Sys.time();
	cat("[",format(start.time,"%a %d %b %Y %H:%M:%S"),"] ....Process quality measure for k=",k,"\n",sep="")
#		qual.value <- index.DB(x=data, cl=min.clusters, centrotypes="centroids")$DB
		qual.value <- summary(silhouette(x=min.clusters, dist(data)))$avg.width
		quality[k] <- qual.value
	end.time <- Sys.time();
	total.time <- end.time - start.time;
	cat("[",format(end.time,"%a %d %b %Y %H:%M:%S"),"] ....Processing completed in ",total.time,", DB(",k,")=",qual.value,"\n",sep="")
	
	cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] ..Process completed for k=",k,"\n",sep="")
}
plot(x=data, col=min.clusters); points(centers, col=1:(length(ks)+1), pch = 8) # TODO


###############################################################################
# record quality values
###############################################################################
values.file <- paste(folder.data,"clusters.quality.txt",sep="")
cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] ..Record all quality values in ",values.file,"\n",sep="")
write.table(quality,file=values.file,row.names=FALSE,col.names=FALSE)


###############################################################################
# plot quality values
###############################################################################
plot.file <- paste(folder.data,"clusters.quality.pdf",sep="")
cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] ..Plot quality values in ",plot.file,"\n",sep="")
pdf(file=plot.file,bg="white")
plot(quality[,1],quality[,2],type="l",xlab="Clusters",ylab="Davies-Bouldin index",col="RED")
dev.off()
