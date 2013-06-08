# Determines the best cut in a dendrogram, in terms of Silhouette.
# v1.1
# 
# Author: Vincent Labatut 06/2013
# source("C:/Eclipse/workspaces/Networks/Orleans/src/hierarchical-clusters.R")
###############################################################################
library("cluster")

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

###############################################################################
# test
###############################################################################
