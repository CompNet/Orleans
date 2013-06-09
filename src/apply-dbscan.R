# Apply the DBscan method to detect clusters.
# v1
# 
# Author: Vincent Labatut 06/2013
# source("C:/Eclipse/workspaces/Networks/Orleans/src/apply-dbscan.R")
###############################################################################
library("cluster")
library("fpc")

###############################################################################
# Applies the DBscan clustering method to the specified distance matrix.
#
# dist.matrix: 
#	Distance matrix.
# nbr.instances:
#	Total number of instances (i.e. rows).
# pca:
#	Two principal components obtained for this data. This parameter
#	is optional: if provided, plots will be generated.
# folder:
#	Output folder, used to record new files.
# force.process:
#	Whether or not we want to use cached results.
###############################################################################
apply.dbscan <- function(dist.matrix, nbr.instances, pca=NULL, folder.data, force.process)
{	algo.name <- "DBSCAN"
	file.membership <- paste(folder.data,algo.name,".txt",sep="")
	cat("----\n")
	
	# membership file already exists for this algorithm
	if(file.exists(file.membership) && !force.process)
	{	cat("[",format(Sys.time(),"%a %d %b %Y %X"),"] Algorithm ",algo.name," has already been applied before.\n",sep="")
	}
	
	# apply hierarchical clustering algorithm
	else
	{	start.time <- Sys.time();
		cat("[",format(start.time,"%a %d %b %Y %X"),"] Detecting clusters using ",algo.name,"\n",sep="")
		
			# define function used in grid search
			# takes only the parameters as inputs
			# and outputs only the performance
			foo <- function(eps,MinPts)
			{	result <- dbscan(data=dist.matrix, eps=eps, MinPts=MinPts, seeds=FALSE)
				membership <- result$cluster
				sil <- silhouette(x=membership,dist=dist.matrix)
				if(length(sil)==1)
					sil.value <- 0
				else
					sil.value <- summary(sil)$avg.width
				return(sil.value)
			}
			
			# define parameter ranges
			avg.dist <- mean(dist.matrix)
			parameters <- list(eps=seq(0.05,avg.dist,avg.dist/10),							# radius
					MinPts=as.integer(seq(2,nbr.instances%/%10,max(nbr.instances%/%100,1))))	# neighborhood
			
			# apply grid search method
			optimal.parameters <- iterative.grid.search(foo=foo, parameter.list=parameters, iterations=3)
#			print("optimal.parameters");print(optimal.parameters)
			
			# apply dbscan again, with optimal parameters
			optimal.parameters$data=dist.matrix
			optimal.parameters$seeds=FALSE
			result <- do.call(dbscan, optimal.parameters)
			
			# process corresponding silhouette
			membership <- result$cluster
			nbr <- length(unique(membership))
			sil <- silhouette(x=membership,dist=dist.matrix)
			asw <- summary(sil)$avg.width
			
			# record best membership vector and performance
			cat("[",format(Sys.time(),"%a %d %b %Y %X"),"] Recording results\n",sep="")
			write.table(x=membership,file=file.membership,row.names=FALSE,col.names=FALSE)
			save.performance(asw, nbr, algo.name, folder.data)
			
			# plot result
			if(do.plotting)
			{	file.plot <- paste(folder.data,algo.name,".pdf",sep="")
				plot.clusters(data=pca, membership=membership, file=file.plot)
			}
		
		end.time <- Sys.time();
		total.time <- end.time - start.time;
		cat("[",format(end.time,"%a %d %b %Y %X"),"] final result: ",nbr," clusters, silhouette=",asw,"\n",sep="")
		cat("[",format(end.time,"%a %d %b %Y %X"),"] ",algo.name," completed in ",total.time,"\n",sep="")
	}
}
