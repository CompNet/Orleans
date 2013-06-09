# Apply the k-means method to detect clusters.
# v1
# 
# Author: Vincent Labatut 06/2013
# source("C:/Eclipse/workspaces/Networks/Orleans/src/apply-kmeans.R")
###############################################################################

###############################################################################
# Applies the k-means clustering method to the specified data.
# Data has supposedly been normalized before calling this function.
#
# data: 
#	Normalized data (instances on the rows).
# nbr.instances:
#	Total number of instances (i.e. rows).
# dist.matrix: 
#	Distance matrix.
# pca:
#	Two principal components obtained for this data. This parameter
#	is optional: if provided, plots will be generated.
# folder.data:
#	Output folder, used to record new files.
###############################################################################
apply.kmeans <- function(data, nbr.instances, dist.matrix, pca=NULL, folder.data)
{	algo.name <- "KMEANS"
	cat("----\n")
	start.time <- Sys.time();
	cat("[",format(start.time,"%a %d %b %Y %X"),"] Detecting clusters using ",algo.name,"\n",sep="")
	
		# init
		k.max <- nbr.instances - 1
		asw <- rep(0,k.max)
		max.asw <- 0
		max.membership <- c()
		
		# note: rule of thumb k = sqrt(n/2)
		for(k in 2:k.max)
		{	# apply k-means
			if((k %% 1000) == 0)
				cat("[",format(Sys.time(),"%a %d %b %Y %X"),"] ..with k=",k,"\n",sep="")
			result <- kmeans(x=data, centers=k, iter.max=10, nstart=1)
			membership <- result$cluster
			
			# process average silhouette width (ASW)
			sil <- silhouette(x=membership,dist=dist.matrix)
			asw[k] <- summary(sil)$avg.width
			
			# update current max
			if(asw[k]>max.asw)
			{	max.asw <- asw[k]
				max.membership <- membership
			}
		}
		
		nbr <- length(unique(max.membership))
		
		# record best membership vector and performance
		cat("[",format(Sys.time(),"%a %d %b %Y %X"),"] Recording results\n",sep="")
		file.membership <- paste(folder.data,algo.name,".txt",sep="")
		write.table(x=max.membership,file=file.membership,row.names=FALSE,col.names=FALSE)
		save.performance(max.asw, nbr, algo.name, folder.data)
		
		# plot result
		if(length(pca)>1)
		{	file.plot <- paste(folder.data,algo.name,".pdf",sep="")
			plot.clusters(data=pca, membership=max.membership, file=file.plot)
		}
		
	end.time <- Sys.time();
	total.time <- end.time - start.time;
	cat("[",format(end.time,"%a %d %b %Y %X"),"] final result: ",nbr," clusters, silhouette=",max.asw,"\n",sep="")
	cat("[",format(end.time,"%a %d %b %Y %X"),"] ",algo.name," completed in ",total.time,"\n",sep="")
}
