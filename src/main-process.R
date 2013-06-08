# Process the measures and detect clusters.
# v1.1
# 
# Author: Vincent Labatut 06/2013
# source("C:/Eclipse/workspaces/Networks/Orleans/src/main-process.R")
###############################################################################
library("cluster")
library("fpc")

###############################################################################
# retrieve secondary functions
###############################################################################
# set source folder				#TODO update depending on local file system
folder.source <- "C:/Eclipse/workspaces/Networks/Orleans/src/"	
# load functions
source(paste(folder.source,"apply-dbscan.R",sep=""))
source(paste(folder.source,"apply-hierarchical.R",sep=""))
source(paste(folder.source,"apply-kmeans.R",sep=""))
source(paste(folder.source,"grid-search.R",sep=""))
source(paste(folder.source,"normalize-data.R",sep=""))
source(paste(folder.source,"perform-pca.R",sep=""))
source(paste(folder.source,"plot-clusters.R",sep=""))
source(paste(folder.source,"process-distances.R",sep=""))
source(paste(folder.source,"save-perf.R",sep=""))

###############################################################################
# init parameters
###############################################################################
# force ignoring cached files	#TODO set to FALSE to avoid caching
force.process <- TRUE	
# plot cluster results			#TODO set to FALSE to avoid plotting
do.plotting <- TRUE		
# data folder 					#TODO update depending on local file system
folder.data <- "C:/Eclipse/workspaces/Networks/Orleans/data/"	
# clustering algos to apply		#TODO comment algorithms you don't want to apply
clust.algos <- c(									
	"KMEANS"
#	"AGNES",
#	"DIANA",
#	"DBSCAN"
)
# standard distance function
dist.function <- "euclidean"



###############################################################################
# preprocess data
###############################################################################
# load and normalize data
data <- normalize.data(folder.data, force.process)
# process distances
dist.matrix <- process.distances(folder.data, dist.function, force.process)
# process PCA for plotting
pca <- NULL
if(do.plotting)
	pca <- perform.pca(data, folder.data, force.process)
nbr.instances <- nrow(data)



###############################################################################
# detect clusters with various methods
###############################################################################
# apply both agglomerative approaches
for(algo.name in c("AGNES","DIANA"))
{	if(any(clust.algos==algo.name))
		apply.hierarchical(dist.matrix, pca, folder.data, dist.function)
}
# apply k-means
if(any(clust.algos=="KMEANS"))
	apply.kmeans(data, nbr.instances, pca, folder.data)
# apply DBscan
if(any(clust.algos=="DBSCAN"))
	apply.dbscan(dist.matrix, nbr.instances, pca, folder.data)



###############################################################################
# compare clusters using ARI
###############################################################################
# TODO
