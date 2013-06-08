# Generates dummy data.
# v1
# 
# Author: Vincent Labatut 06/2013
# source("C:/Eclipse/workspaces/Networks/Orleans/src/main-generation.R")
###############################################################################
# source folder
folder.source <- "C:/Eclipse/workspaces/Networks/Orleans/src/"
source(paste(folder.source,"normalize-data.R",sep=""))
source(paste(folder.source,"perform-pca.R",sep=""))
source(paste(folder.source,"plot-clusters.R",sep=""))


###############################################################################
# init parameters
###############################################################################
# desired number of clusters		# TODO can be changed for testing
n.cluster <- 7			
# desired number of instances		# TODO can be changed for testing
n.instances <- 500		
# we have 2x4 measures (mutually exclusive communities, directed network)
n.fields <- 8
# data folder						#TODO update depending on local file system
folder.data <- "C:/Eclipse/workspaces/Networks/Orleans/data/"
# file name
file.name <- "data"


###############################################################################
# generate clusters
###############################################################################
cat("[",format(Sys.time(),"%a %d %b %Y %X"),"] Generating ",n.instances," instances distributed over ",n.cluster," clusters...\n",sep="")
data <- matrix(ncol=n.fields+1,nrow=n.instances)
cluster.size <- floor(n.instances / n.cluster)
for(c in 1:n.cluster)
{	from <- (c-1)*cluster.size + 1
	nbr <- cluster.size
	if(c==n.cluster)
		nbr <- n.instances - from + 1
	to <- from + nbr - 1
	for(f in 1:n.fields)
		data[from:to,f] <- rnorm(n=nbr,mean=10*runif(1),sd=runif(1))
	data[from:to,n.fields+1] <- c
}
# randomize instance order
data <- data[sample(nrow(data)),]


###############################################################################
# record data, generate plots
###############################################################################
# record raw data
cat("[",format(Sys.time(),"%a %d %b %Y %X"),"] Record to file...\n",sep="")
mbr <- data[,n.fields+1]
data <- data[,-(n.fields+1)]
file.data <- paste(folder.data,file.name,".txt",sep="")
write.table(x=data,file=file.data,row.names=FALSE,col.names=FALSE)
# normalize data
data <- normalize.data(folder.data)
# process and record PCA for later use
pca <- perform-pca(data, folder.data)
# plot clusters, for visual inspection
file.plot <- paste(folder.data,file.name,".pdf",sep="")
plot.clusters(data=pca, membership=mbr, file=file.plot)
