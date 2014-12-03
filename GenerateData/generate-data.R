# Generates dummy data to play with
# (and to test the rest of the scripts).
#
# version: 2
# Author: Vincent Labatut 06/2013,07/2014
#
# setwd("~/eclipse/workspaces/Networks/Orleans/")
# setwd("C:/Eclipse/workspaces/Networks/Orleans/")
# source("GenerateData/generate-data.R")
###############################################################################
library("igraph")

source("ClusterAnalysis/cluster-analysis.R")
source("CommunityDetection/community-detection.R")
source("RoleMeasures/role-measures.R")

###############################################################################
# Generates and records a fake network,
# as well as its degree sequence.
# 
# folder.data: name of the folder containing all files to be generated.
# directed: whether the generated network should be directed or not.
# n: number of nodes.
###############################################################################
generate.network <- function(folder.data, directed=TRUE, n)
{	# generate network
	cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] Generate network\n",sep="")
	#g <- barabasi.game(n=n.instances, power=3, m=2)
	g <- erdos.renyi.game(n=n, p.or.m=0.1, type="gnp", directed=directed, loops=FALSE)
	
	# record network as edgelist file
	cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] Record network to file\n",sep="")
	net.file <- get.network.filename(folder.data)
	#write.graph(graph=g, file=file.net, format="edgelist")
	el <- get.edgelist(graph=g) - 1 # we want to number nodes starting from 0
	write.table(x=el, file=net.file, row.names=FALSE, col.names=FALSE)
	
	# process degree sequence
	cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] Process degree sequences\n",sep="")
	degrees <- cbind(degree(graph=g,mode="in"),degree(graph=g,mode="out"),degree(graph=g,mode="all"))
	colnames(degrees) <- get.degrees.names()
			
	# record degree sequence
	cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] Record degree sequences to file\n",sep="")
	degree.file <- get.degrees.filename(folder.data)
	write.table(x=degrees, file=degree.file, row.names=FALSE, col.names=TRUE)
}

###############################################################################
# Generates the role measures and clusters.
# 
# folder.data: name of the folder containing all files to be generated.
# role.mes: type of role measures.
# n: desired number of nodes.
# n.clust: desired number of clusters.
# clust.algo: cluster analysis algorithm (fake).
# comdet.algo: community detection algorithm (fake).
###############################################################################
generate.rolemeas <- function(folder.data, role.meas, n, n.clust, clust.algo, comdet.algo)
{	meas.names <- get.rolemeas.names(role.meas)
	n.fields <- length(meas.names)
	
	# generate fake role measures
	cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] Generating ",n," instances distributed over ",n.clust," clusters...\n",sep="")
	x <- matrix(ncol=n.fields+1,nrow=n)
	cluster.size <- floor(n / n.clust)
	for(c in 1:n.clust)
	{	from <- (c-1)*cluster.size + 1
		nbr <- cluster.size
		if(c==n.clust)
			nbr <- n - from + 1
		to <- from + nbr - 1
		for(f in 1:n.fields)
			x[from:to,f] <- rnorm(n=nbr,mean=10*runif(1),sd=runif(1))
		x[from:to,n.fields+1] <- c
	}
	# randomize instance order
	x <- x[sample(nrow(x)),]
	# set names
	colnames(x) <- c(meas.names,"Cluster")
	
	# record cluster membership
	cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] Record membership vector to file\n",sep="")
	file.membership <- get.cluster.filename(folder.data, role.meas, n.clust=0, clust.algo, comdet.algo)
	write.table(x=x[,n.fields+1]-1, file=file.membership, row.names=FALSE, col.names=FALSE)

	# record role measures
	cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] Record role measures to file\n",sep="")
	x <- x[,-(n.fields+1)]
	file.data <- get.rolemeas.filename(folder.data,role.meas,norm=FALSE,comdet.algo)
	write.table(x=x,file=file.data,row.names=FALSE,col.names=TRUE)
}

###############################################################################
# Generates and records fake communities.
#
# folder.data: name of the folder to contain all generated data.
# n: desired number of nodes.
# clust.algo: cluster analysis algorithm (fake).
# n.com: desired number of communities.
# comdet.algo: community detection algorithm (fake).
###############################################################################
generate.communities <- function(folder.data, n, clust.algo, n.com, comdet.algo)
{	cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] Generate communities\n",sep="")
	coms <- floor(runif(n,min=1,max=n.com)) - 1
	
	cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] Record communities to file\n",sep="")
	file.coms <- get.communities.filename(folder.data,comdet.algo)
	write.table(x=coms,file=file.coms,row.names=FALSE,col.names=FALSE)
}

###############################################################################
# Main function for data generation.
# Warning: existing files might be replaced by newly generated ones.
#
# folder.data: name of the folder to contain all generated data.
# role.mes: name of the role measures.
# n: desired number of nodes.
# directed: whether the network should be directed (does not affect the measures)
# n.clust: desired number of clusters.
# clust.algo: clustering algorithm (fake).
# n.com: desired number of communities.
# comdet.algo: community detection algorithm (fake).
###############################################################################
generate.data <- function(folder.data, role.meas, n, directed, n.clust, clust.algo, n.com, comdet.algo)
{	# generate network and degree
	generate.network(folder.data, directed, n)
	
	# generate role measures and clusters
	generate.rolemeas(folder.data, role.meas, n, n.clust, clust.algo, comdet.algo)
		
	# generate communities
	generate.communities(folder.data, n, clust.algo, n.com, comdet.algo)
}
