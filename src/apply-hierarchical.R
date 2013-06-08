# Apply a hierarchical method to detect clusters, and select the best
# cut in the resulting dendrogram.
# v1
# 
# Author: Vincent Labatut 06/2013
# source("C:/Eclipse/workspaces/Networks/Orleans/src/apply-hierarchical.R")
###############################################################################
library("cluster")

###############################################################################
# Applies a hierarchical clustering method to the specified data.
#
# dist.matrix: 
#	Distance matrix.
# pca:
#	Two principal components obtained for this data. This parameter
#	is optional: if provided, plots will be generated.
# folder.data:
#	Output folder, used to record new files.
# dist.function:
#	Name of the distance function to use during cluster analysis.
###############################################################################
apply.hierarchical <- function(dist.matrix, pca=NULL, folder.data, dist.function)
{	
	cat("----\n")
	# apply hierarchical clustering algorithm
	start.time <- Sys.time();
	cat("[",format(start.time,"%a %d %b %Y %X"),"] Detecting clusters using ",algo.name,"\n",sep="")
	
	if(algo.name=="AGNES")
		result <- agnes(x=dist.matrix, metric=dist.function, method="average", keep.diss=FALSE, keep.data=FALSE)
	else if(algo.name=="DIANA")
		result <- diana(x=dist.matrix, metric=dist.function, keep.diss=FALSE, keep.data=FALSE)
	
	# select best cut from dendrogram, record it
	res <- select.best.cut(result)
	nbr <- length(unique(res$membership))
	
	# record best membership vector and performance
	cat("[",format(Sys.time(),"%a %d %b %Y %X"),"] Recording results\n",sep="")
	file.membership <- paste(folder.data,algo.name,".txt",sep="")
	write.table(x=res$membership,file=file.membership,row.names=FALSE,col.names=FALSE)
	save.performance(res$silhouette, nbr, algo.name, folder.data)
	
	# plot result
	if(do.plotting)
	{	file.plot <- paste(folder.data,algo.name,".pdf",sep="")
		plot.clusters(data=pca, membership=res$membership, file=file.plot)
	}
	
	end.time <- Sys.time();
	total.time <- end.time - start.time;
	cat("[",format(end.time,"%a %d %b %Y %X"),"] final result: ",nbr," clusters, silhouette=",res$silhouette,"\n",sep="")
	cat("[",format(end.time,"%a %d %b %Y %X"),"] ",algo.name," completed in ",total.time,"\n",sep="")
}



###############################################################################
# Considers all cuts in a dendrogram, process their silhouette, and returns
# the cut corresponding to the maximal silhouette.
#
# dendrogram: 
#	hierarchical clusters (object returned by agnes or diana).
#
# returns:
#	a list containing the maximal silhouette ($silhouette)
#	and the corresponding membership vector ($membership), i.e.
#	a vector whose each value represents the cluster of the 
#	corresponding instance.
###############################################################################
select.best.cut <- function(dendrogram)
{	start.time <- Sys.time();
	cat("[",format(start.time,"%a %d %b %Y %X"),"] Selecting best cut\n",sep="")
	
	k.max <- length(dendrogram$height)
	asw <- rep(0,k.max)
	max.asw <- 0
	max.membership <- c()
	
	# select best cut
	for(k in 2:k.max)
	{	#print(k)		
		# retrieve membership vector
		membership <- cutree(result,k)
		
		# process average silhouette width (ASW)
		sil <- silhouette(x=membership,dist=dist.matrix)
		asw[k] <- summary(sil)$avg.width
		
		# update current max
		if(asw[k]>max.asw)
		{	max.asw <- asw[k]
			max.membership <- membership
		}
	}
	
	end.time <- Sys.time();
	total.time <- end.time - start.time;
	cat("[",format(end.time,"%a %d %b %Y %X"),"] best cut selection completed in ",total.time,"\n",sep="")
	
	result <- list(silhouette=max.asw, membership=max.membership)
	return(result)
}
