# Processes statistics regarding the estimated communities,
# mainly regarding their sizes.
#
# version: 2
# Author: Vincent Labatut 06/2013, 01/2015
#
# setwd("~/eclipse/workspaces/Networks/Orleans/")
# setwd("C:/Eclipse/workspaces/Networks/Orleans/")
# source("PostProcessing/community-stats.R")
###############################################################################


###############################################################################
# Returns the standard filename for the community size histogram.
#
# folder.data: folder containing all input and output files.
# comdet.algo: community detection algorithm.
###############################################################################
get.community.size.histo.filename <- function(folder.data, comdet.algo)
{	result <- paste(folder.data,"comdet=",comdet.algo,sep="")
	result <- paste(result,".sizes.histo.pdf",sep="")
	return(result)
}

###############################################################################
# Returns the standard filename for the power law test regarding community
# size distribution.
#
# folder.data: folder containing all input and output files.
# comdet.algo: community detection algorithm.
###############################################################################
get.community.size.powerlaw.filename <- function(folder.data, comdet.algo)
{	result <- paste(folder.data,"comdet=",comdet.algo,sep="")
	result <- paste(result,".sizes.powerlaw.txt",sep="")
	return(result)
}

###############################################################################
# Processes the means, standard-deviations and sizes of the detected clusters.
#
# folder.data: folder containing all input and output files.
# role.meas: type of role measures.
# clust.algo: cluster analysis method.
# comdet.algo: community detection algorithm.
###############################################################################
process.community.stats <- function(folder.data, comdet.algo)
{	# load membership vector
	start.time <- Sys.time()
	cat("[",format(start.time,"%a %d %b %Y %H:%M:%S"),"] Loading membership vector\n",sep="")
		membership.file <- get.communities.filename(folder.data, comdet.algo)
		membership <- as.vector(as.matrix(read.table(membership.file)))
	end.time <- Sys.time()
	total.time <- end.time - start.time
	cat("[",format(end.time,"%a %d %b %Y %H:%M:%S"),"] Load completed in ",format(total.time),"\n",sep="")
	
	# plot histogram
	plot.file <- get.community.size.histo.filename(folder.data, comdet.algo)
	cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] Plot histogram in file ",plot.file,"\n",sep="")
	pdf(file=plot.file, bg="white")
	hist(membership,probability=TRUE,main=paste("Distribution of community sizes"),xlab="Community sizes",col="RED")
	dev.off()
	
	# fit to power law
	cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] ....Check if community size distribution follows a power law\n",sep="")
	fit <- matrix(ncol=2,nrow=1)
	colnames(fit) <- c("p-value","exponent")
	plf <- power.law.fit(x=membership, implementation="plfit")
	fit[1,"p-value"] <- plf$KS.p
	fit[1,"exponent"] <- plf$alpha
	pl.file <- get.community.size.powerlaw.filename(folder.data, comdet.algo)
	cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] ....Record power law test results in file ",pl.file,"\n",sep="")
	print(fit)
	write.table(fit,pl.file,col.names=TRUE,row.names=FALSE)
}
