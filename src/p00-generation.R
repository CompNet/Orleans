# Generates dummy data to play with
# (and test the rest of the scripts).
#
# version: 1
# Author: Vincent Labatut 06/2013
#
# setwd("~/eclipse/workspaces/Networks/Orleans/")
# setwd("C:/Eclipse/workspaces/Networks/Orleans/")
# source("src/p00-generation.R")
###############################################################################


###############################################################################
# setup files
###############################################################################
folder.data <- "data/"	


###############################################################################
# init parameters
###############################################################################
# desired number of clusters		# TODO can be changed for testing
n.cluster <- 7
n.communities <- 36
# desired number of instances		# TODO can be changed for testing
n.instances <- 250		
# we have 2x4 measures (mutually exclusive communities, directed network)
n.fields <- 8


###############################################################################
# generate measures
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
# introduce infinite values to mimic actual data
for(i in 1:n.fields)
	data[floor(runif(1,min=1,max=n.instances)),i] <- Inf

###############################################################################
# record measures
###############################################################################
cat("[",format(Sys.time(),"%a %d %b %Y %X"),"] Record measures to file\n",sep="")
data <- data[,-(n.fields+1)]
file.data <- paste(folder.data,"rolemeasures.raw.txt",sep="")
write.table(x=data,file=file.data,row.names=FALSE,col.names=FALSE)


###############################################################################
# generate and record communities
###############################################################################
cat("[",format(Sys.time(),"%a %d %b %Y %X"),"] Generate communities\n",sep="")
coms <- floor(runif(n.instances,min=1,max=n.communities))
cat("[",format(Sys.time(),"%a %d %b %Y %X"),"] Record communities to file\n",sep="")
file.coms <- paste(folder.data,"communities.txt",sep="")
write.table(x=coms,file=file.coms,row.names=FALSE,col.names=FALSE)
