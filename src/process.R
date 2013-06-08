# Process the measures and detect clusters.
# v1.1
# 
# Author: Vincent Labatut 06/2013
# source("C:/Eclipse/workspaces/Networks/Orleans/src/process.R")
###############################################################################
library("cluster")
library("fpc")

# source folder
folder.source <- "C:/Eclipse/workspaces/Networks/Orleans/src/"
source(paste(folder.source,"grid-search.R",sep=""))
source(paste(folder.source,"hierarchical-clusters.R",sep=""))
source(paste(folder.source,"plot-clusters.R",sep=""))
source(paste(folder.source,"save-perf.R",sep=""))

# force ignoring cached files
force.process <- TRUE
# plot cluster results
do.plotting <- TRUE
# data folder
folder.data <- "C:/Eclipse/workspaces/Networks/Orleans/data/"
# clustering algos to apply
clust.algos <- c(									
#	"KMEANS",
#	"AGNES",
#	"DIANA",
	"DBSCAN"
)
# standard distance function
dist.function <- "euclidean"



# load data
start.time <- Sys.time();
cat("[",format(start.time,"%a %d %b %Y %X"),"] Loading raw data...\n",sep="")
file.data <- paste(folder.data,"data.txt",sep="")
	m <- as.matrix(read.table(file.data))
	nbr.instances <- nrow(m)
end.time <- Sys.time();
total.time <- end.time - start.time;
cat("[",format(end.time,"%a %d %b %Y %X"),"] Load completed in ",total.time,"\n",sep="")



# process PCA for plotting
if(do.plotting)
{	file.pca <- paste(folder.data,"pca.txt",sep="")
	if(file.exists(file.pca) && !force.process)
	{	start.time <- Sys.time();
		cat("[",format(start.time,"%a %d %b %Y %X"),"] Loading PCA results...\n",sep="")
		file.data <- paste(folder.data,"data.txt",sep="")
			pca <- as.matrix(read.table(file.pca))
		end.time <- Sys.time();
		total.time <- end.time - start.time;
		cat("[",format(end.time,"%a %d %b %Y %X"),"] Load completed in ",total.time,"\n",sep="")
	}else
	{	start.time <- Sys.time();
		cat("[",format(start.time,"%a %d %b %Y %X"),"] Processing PCA...\n",sep="")
		file.data <- paste(folder.data,"data.txt",sep="")
			pca.obj <- princomp(m)
			pca <- pca.obj$scores[,1:2]
			write.table(x=pca,file=file.pca,row.names=FALSE,col.names=FALSE)
		end.time <- Sys.time();
		total.time <- end.time - start.time;
		cat("[",format(end.time,"%a %d %b %Y %X"),"] Process completed in ",total.time,"\n",sep="")
	}
}



# preprocess data
file.dist <- paste(folder.data,"distances.bin",sep="")
if(file.exists(file.dist) && !force.process)
{	start.time <- Sys.time()
	cat("[",format(start.time,"%a %d %b %Y %X"),"] Loading previously processed distances...\n",sep="")
		load(file=file.dist)
	end.time <- Sys.time();
	total.time <- end.time - start.time;
	cat("[",format(end.time,"%a %d %b %Y %X"),"] Load completed in ",total.time,"\n",sep="")
}else
{	# normalize (centered-reduced)
	start.time <- Sys.time();
	cat("[",format(start.time,"%a %d %b %Y %X"),"] Normalizing data...\n",sep="")
		for(c in 1:ncol(m))
		{	average <- mean(m[,c])
			stdev <- sd(m[,c])
			m[,c] <- (m[,c] - average) / stdev
		}
	end.time <- Sys.time();
	total.time <- end.time - start.time;
	cat("[",format(end.time,"%a %d %b %Y %X"),"] Normalization completed in ",total.time,"\n",sep="")
	
	# process distances
	start.time <- Sys.time();
	cat("[",format(start.time,"%a %d %b %Y %X"),"] Processing distances...\n",sep="")
		dist.matrix <- dist(m, method=dist.function)
	end.time <- Sys.time();
	total.time <- end.time - start.time;
	cat("[",format(end.time,"%a %d %b %Y %X"),"] Distance processing completed in ",total.time,"\n",sep="")
	
	# cache to disk
	start.time <- Sys.time();
	cat("[",format(start.time,"%a %d %b %Y %X"),"] Recording distances for later use...\n",sep="")
		save(dist.matrix,file=file.dist)
	end.time <- Sys.time();
	total.time <- end.time - start.time;
	cat("[",format(end.time,"%a %d %b %Y %X"),"] Distance recording completed in ",total.time,"\n",sep="")
}



# detect clusters with both agglomerative approaches
for(algo.name in c("AGNES","DIANA"))
{	if(any(clust.algos==algo.name))
	{	cat("----\n")
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
				dev.new()
				plot.clusters(data=pca, membership=res$membership, file=file.plot)
			}
		
		end.time <- Sys.time();
		total.time <- end.time - start.time;
		cat("[",format(end.time,"%a %d %b %Y %X"),"] final result: ",nbr," clusters, silhouette=",res$silhouette,"\n",sep="")
		cat("[",format(end.time,"%a %d %b %Y %X"),"] ",algo.name," completed in ",total.time,"\n",sep="")
	}
}



# detect clusters using k-means
if(any(clust.algos=="KMEANS"))
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
			result <- kmeans(x=m, centers=k, iter.max=10, nstart=1)
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
		if(do.plotting)
		{	file.plot <- paste(folder.data,algo.name,".pdf",sep="")
			dev.new()
			plot.clusters(data=pca, membership=max.membership, file=file.plot)
		}
		
	end.time <- Sys.time();
	total.time <- end.time - start.time;
	cat("[",format(end.time,"%a %d %b %Y %X"),"] final result: ",nbr," clusters, silhouette=",max.asw,"\n",sep="")
	cat("[",format(end.time,"%a %d %b %Y %X"),"] ",algo.name," completed in ",total.time,"\n",sep="")
}



# detect clusters using DBscan
if(any(clust.algos=="DBSCAN"))
{	algo.name <- "DBSCAN"
	cat("----\n")
	start.time <- Sys.time();
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
		print("optimal.parameters");print(optimal.parameters)
		
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
		file.membership <- paste(folder.data,algo.name,".txt",sep="")
		write.table(x=membership,file=file.membership,row.names=FALSE,col.names=FALSE)
		save.performance(asw, nbr, algo.name, folder.data)
		
		# plot result
		if(do.plotting)
		{	file.plot <- paste(folder.data,algo.name,".pdf",sep="")
			dev.new()
			plot.clusters(data=pca, membership=membership, file=file.plot)
		}
	
	end.time <- Sys.time();
	total.time <- end.time - start.time;
	cat("[",format(end.time,"%a %d %b %Y %X"),"] final result: ",nbr," clusters, silhouette=",asw,"\n",sep="")
	cat("[",format(end.time,"%a %d %b %Y %X"),"] ",algo.name," completed in ",total.time,"\n",sep="")
}

# TODO ARI
# TODO pb avec limites dans rech grid
# TODO séparer en plusieurs fonctions
# TODO plots plus petits
