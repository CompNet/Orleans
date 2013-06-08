# Generates dummy data.
# v1
# 
# Author: Vincent Labatut 06/2013
# source("C:/Eclipse/workspaces/Networks/Orleans/src/generation.R")
###############################################################################
# source folder
folder.source <- "C:/Eclipse/workspaces/Networks/Orleans/src/"
source(paste(folder.source,"plot-clusters.R",sep=""))


# desired number of clusters
n.cluster <- 7			
# desired number of instances
n.instances <- 500		
# we have 2x4 measures (mutually exclusive communities, directed network)
n.fields <- 8
# data folder
folder.data <- "C:/Eclipse/workspaces/Networks/Orleans/data/"
# file name
file.name <- "data"


# generate clusters
cat("[",format(Sys.time(),"%a %d %b %Y %X"),"] Generating ",n.instances," instances distributed over ",n.cluster," clusters...\n",sep="")
m <- matrix(ncol=n.fields+1,nrow=n.instances)
cluster.size <- floor(n.instances / n.cluster)
for(c in 1:n.cluster)
{	from <- (c-1)*cluster.size + 1
	nbr <- cluster.size
	if(c==n.cluster)
		nbr <- n.instances - from + 1
	to <- from + nbr - 1
	for(f in 1:n.fields)
		m[from:to,f] <- rnorm(n=nbr,mean=10*runif(1),sd=runif(1))
	m[from:to,n.fields+1] <- c
}


# randomize instance order
m <- m[sample(nrow(m)),]


# record data in a text file (cols=fields, row=instances)
cat("[",format(Sys.time(),"%a %d %b %Y %X"),"] Record to file...\n",sep="")
mbr <- m[,n.fields+1]
m <- m[,-(n.fields+1)]
file.data <- paste(folder.data,file.name,".txt",sep="")
write.table(x=m,file=file.data,row.names=FALSE,col.names=FALSE)


# process and record PCA for later use
pca.obj <- princomp(m)
pca <- pca.obj$scores[,1:2]
file.pca <- paste(folder.data,"pca.txt",sep="")
write.table(x=pca,file=file.pca,row.names=FALSE,col.names=FALSE)


# plot clusters, for visual inspection
file.plot <- paste(folder.data,file.name,".pdf",sep="")
plot.clusters(data=pca, membership=mbr, file=file.plot)

