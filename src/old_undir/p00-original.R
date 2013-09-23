# Process orignal Guimerà & Amaral measures for their
# original data (so that we can compare their heuristic roles
# with roles detected through our cluster-based method).
#
# version: 1
# Author: Vincent Labatut 06/2013
#
# setwd("~/eclipse/workspaces/Networks/Orleans/")
# setwd("C:/Eclipse/workspaces/Networks/Orleans/")
# source("src/old_undir/p00-original.R")
###############################################################################
library("igraph")
library("clusterSim")


###############################################################################
# setup files
###############################################################################
folder.data <- "data/"	
network.names <- c(
	"afu",	# Archaeoglobus fulgidus
	"ape",	# Aeropyrum pernix
	"bsu",	# Bacillus subtilis
	"cel",	# Caenorhabditis elegans
	"eco",	# Escherichia coli
	"hsa",	# Homo sapiens
	"lla",	# Lactococcus lactis
	"pfu",	# Pyrococcus furiosus
	"sce",	# Saccharomyces cerevisiae
	"sso",	# Sulfolobus solfataricus
	"tel"	# Thermosynechococcus elongatus
)


###############################################################################
# load networks
###############################################################################
cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] Load networks\n",sep="")
gs <- list()
for(network.name in network.names)
{	cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] ..Load network ",network.name,"\n",sep="")
	file.net <- paste(folder.data,network.name,".net",sep="")
	g <- read.graph(file=file.net, format="pajek")
	g <- as.undirected(g)
	g <- remove.edge.attribute(graph=g,name="weight")
	gs[[network.name]] <- g
}


###############################################################################
# detect communities
###############################################################################
cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] Detect communities\n",sep="")
# we cannot really get exactly the same communities than in the original article,
# but we use the the closest community detection method to try approaching them.
coms <- list()
for(network.name in network.names)
{	cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] ..Detect communities in network ",network.name,"\n",sep="")
	g <- gs[[network.name]]
	# try to perform an exact optimization
#	coms[[network.name]] <- optimal.community(graph=g)$membership
	# if too long, try an approximation
#	coms[[network.name]] <- edge.betweenness.community(graph=g)$membership
#	coms[[network.name]] <- fastgreedy.community(graph=g)$membership
#	coms[[network.name]] <- infomap.community(graph=g)$membership
#	coms[[network.name]] <- label.propagation.community(graph=g)$membership
#	coms[[network.name]] <- leading.eigenvector.community(graph=g)$membership
	coms[[network.name]] <- multilevel.community(graph=g)$membership
##	coms[[network.name]] <- spinglass.community(graph=g)$membership
#	coms[[network.name]] <- walktrap.community(graph=g)$membership
}	
print(lapply(coms,function(x) length(unique(x))))


###############################################################################
# process both role measures for each network
###############################################################################
cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] Process role measures\n",sep="")
zscores <- list()
participations <- list()
for(network.name in network.names)
{	cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] ..Process role measures for network ",network.name,"\n",sep="")
	
	# get graph and communities
	g <- gs[[network.name]]
	V(g)$name <- 1:vcount(g)
	com <- coms[[network.name]]
	deg <- degree(g,mode="all")
	
	# process average and sd of internal degree for each community
	cs <- sort(unique(com))
	z.mean <- rep(NA,length(cs))
	z.stdev <- rep(NA,length(cs))
	for(c in 1:length(cs))
	{	idx <- which(com==cs[c])
		g.com <- induced.subgraph(graph=g,vids=idx)
		c.degree <- degree(graph=g.com,v=V(g.com),mode="all")
		z.mean[c] <- mean(c.degree)
		z.stdev[c] <- sd(c.degree)
	}
	
	# process community-specific degree for each node and community
	c.degrees <- matrix(0,nrow=vcount(g),ncol=length(cs))
	for(idx1 in 1:vcount(g))
	{	c1 <- com[idx1]
		# get the direct neighbors
		nbhd.idx <- neighborhood(graph=g,order=1,nodes=idx1,mode="all")[[1]]
		nbhd.idx <- nbhd.idx[nbhd.idx!=idx1]
		# check if they are in the same community
		for(idx2 in nbhd.idx)
		{	c2 <- com[idx2]
			c.degrees[idx1,c2] <- c.degrees[idx1,c2] + 1
		}
	}
	
	# process measures for each node
	zscore <- rep(0,vcount(g))
	participation <- rep(0,vcount(g))
	for(idx in 1:vcount(g))
	{	c <- com[idx]
		
		# process z-score
		if(z.stdev[c]==0 || is.na(z.stdev[c]))
			zscore[idx] <- 0
		else
			zscore[idx] <- (c.degrees[idx,c]-z.mean[c])/z.stdev[c]
		
		# process participation coefficient
		if(deg[idx]==0)
			participation[idx] <- 0
		else
		{	part <- 0
			for(c in cs)
			{	part <- part + (c.degrees[idx,c]/deg[idx])^2
			}
			participation[idx] <- 1 - part
		}
	}
	
	zscores[[network.name]] <- zscore
	participations[[network.name]] <- participation
}

###############################################################################
# plot role measures for verification
###############################################################################
cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] Plot role measures\n",sep="")
par(mfrow=c(3,4))
for(network.name in network.names)
{	cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] ..Plotting role measures for network ",network.name,"\n",sep="")
	
	zscore <- zscores[[network.name]] 
	participation <- participations[[network.name]]
	
	plot(participation,zscore,col="red",main=network.name)
}


###############################################################################
# detect clusters in role measure space
###############################################################################
cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] Cluster role measures\n",sep="")
k.max <- 15
quality <- matrix(NA,nrow=length(network.names),ncol=k.max)
rownames(quality) <- network.names
colnames(quality) <- 1:k.max
for(network.name in network.names)
{	cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] ..Clustering role measures for network ",network.name,"\n",sep="")
	
	r <- which(network.names==network.name)
	data <- cbind(participations[[network.name]],zscores[[network.name]])
	
	for(k in 2:k.max)
	{	membership <- kmeans(x=data, centers=k,  nstart=5, algorithm="Hartigan-Wong")$cluster
		db.value <- index.DB(x=data, cl=membership, centrotypes="centroids")$DB
		quality[r,k] <- db.value
	}
}


###############################################################################
# record clusters quality and estimated number of clusters (roles)
###############################################################################
cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] Record results\n",sep="")
# quality associated to the clusters
file.quality <- paste(folder.data,"original.clusters.quality.txt",sep="")
write.table(quality,file=file.quality)
print(quality)
# number of roles for each network
role.nbr <- sapply(1:nrow(quality),function(x) which.min(quality[x,]))
role.nbr <- as.matrix(role.nbr,nrow=length(network.names))
rownames(role.nbr) <- network.names
file.numbers <- paste(folder.data,"original.clusters.numbers.txt",sep="")
write.table(role.nbr,file=file.numbers,row.names=FALSE,col.names=FALSE)
print(role.nbr)
