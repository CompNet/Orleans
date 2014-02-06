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
folder.data <- "Data/"	


###############################################################################
# init parameters
###############################################################################
# desired number of clusters		# TODO can be changed for testing
n.cluster <- 7
n.communities <- 36
# desired number of instances		# TODO can be changed for testing
n.instances <- 250
# we have 2x4 role measures (mutually exclusive communities, directed network)
n.fields <- 8


###############################################################################
# generate fake role measures
###############################################################################
cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] Generating ",n.instances," instances distributed over ",n.cluster," clusters...\n",sep="")
	x <- matrix(ncol=n.fields+1,nrow=n.instances)
	cluster.size <- floor(n.instances / n.cluster)
	for(c in 1:n.cluster)
	{	from <- (c-1)*cluster.size + 1
		nbr <- cluster.size
		if(c==n.cluster)
			nbr <- n.instances - from + 1
		to <- from + nbr - 1
		for(f in 1:n.fields)
			x[from:to,f] <- rnorm(n=nbr,mean=10*runif(1),sd=runif(1))
		x[from:to,n.fields+1] <- c
	}
	# randomize instance order
	x <- x[sample(nrow(x)),]
	# introduce infinite values to mimic actual data
	for(i in 1:n.fields)
		x[floor(runif(1,min=1,max=n.instances)),i] <- Inf


###############################################################################
# record cluster membership
###############################################################################
cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] Record membership vector to file\n",sep="")
	file.membership <- paste(folder.data,"cluster.k",n.cluster,".txt",sep="")
	write.table(x=cbind(1:n.instances,x[,n.fields+1]), file=file.membership, row.names=FALSE, col.names=FALSE)


###############################################################################
# record role measures
###############################################################################
cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] Record role measures to file\n",sep="")
	x <- x[,-(n.fields+1)]
	file.data <- paste(folder.data,"rolemeasures.raw.txt",sep="")
	write.table(x=x,file=file.data,row.names=FALSE,col.names=FALSE)


###############################################################################
# generate and record fake communities
###############################################################################
cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] Generate communities\n",sep="")
	coms <- floor(runif(n.instances,min=1,max=n.communities))
cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] Record communities to file\n",sep="")
	file.coms <- paste(folder.data,"communities.txt",sep="")
	write.table(x=coms,file=file.coms,row.names=FALSE,col.names=FALSE)


###############################################################################
# generate and record a fake network
###############################################################################
cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] Generate network\n",sep="")
	#g <- barabasi.game(n=n.instances, power=3, m=2)
	g <- erdos.renyi.game(n=n.instances, p.or.m=0.1, type="gnp", directed=TRUE, loops=FALSE)
cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] Record network to file\n",sep="")
	file.net <- paste(folder.data,"network.edgelist",sep="")
	write.graph(graph=g, file=file.net, format="edgelist")
cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] Process degree sequences\n",sep="")
	degrees <- cbind(degree(graph=g,mode="out"),degree(graph=g,mode="in"),degree(graph=g,mode="all"))
cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] Record degree sequences to file\n",sep="")
	file.degree <- paste(folder.data,"degrees.txt",sep="")
	write.table(x=degrees, file=file.degree, row.names=FALSE, col.names=FALSE)

	
###############################################################################
# generate and record fake social capitalism measures
###############################################################################
cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] Generate social capitalism measures\n",sep="")
	ratio <- runif(n=n.instances,min=0,max=10)
	overlap <- runif(n=n.instances,min=0,max=1)
	soccap <- cbind(ratio,overlap)
cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] Record social capitalism measures to file\n",sep="")
	file.soccap <- paste(folder.data,"soccapmeasures.txt",sep="")
	write.table(x=soccap, file=file.soccap, row.names=FALSE, col.names=FALSE)


