# Compares the partitions (clusters) obtained with the
# new method with those obtained with the old one.
#
# Note it is necessary to rename the cluster file obtained 
# with the old measures before applying this script. 
#
# version: 1
# Author: Vincent Labatut 09/2013
#
# setwd("~/eclipse/workspaces/Networks/Orleans/")
# setwd("C:/Eclipse/workspaces/Networks/Orleans/")
# source("src/p10-comparison.R")
###############################################################################
library("flexclust")


###############################################################################
# setup files
###############################################################################
folder.data <- "data/"	
file.old <- "cluster.old.txt"		# TODO you can possibly change that
k <- 6								# TODO we work only on the clusters found for this k


###############################################################################
# load old membership vector
###############################################################################
start.time <- Sys.time();
cat("[",format(start.time,"%a %d %b %Y %H:%M:%S"),"] Loading membership vector\n",sep="")
	membership.file <- paste(folder.data,file.old,sep="")
	membership.old <- as.matrix(read.table(membership.file))[,2] + 1	# the k-means implementation starts numbering clusters from 0
end.time <- Sys.time();
total.time <- end.time - start.time;
cat("[",format(end.time,"%a %d %b %Y %H:%M:%S"),"] Load completed in ",total.time,"\n",sep="")


###############################################################################
# load new membership vector
###############################################################################
start.time <- Sys.time();
cat("[",format(start.time,"%a %d %b %Y %H:%M:%S"),"] Loading membership vector\n",sep="")
	membership.file <- paste(folder.data,"cluster.k",k,".txt",sep="")
	membership.new <- as.matrix(read.table(membership.file))[,2] + 1	# the k-means implementation starts numbering clusters from 0
end.time <- Sys.time();
total.time <- end.time - start.time;
cat("[",format(end.time,"%a %d %b %Y %H:%M:%S"),"] Load completed in ",total.time,"\n",sep="")


###############################################################################
# process similarity
###############################################################################
start.time <- Sys.time();
cat("[",format(start.time,"%a %d %b %Y %H:%M:%S"),"] Processing Rand index\n",sep="")
	result.ri <- randIndex(membership.old,membership.new,correct=FALSE)
end.time <- Sys.time();
total.time <- end.time - start.time;
cat("[",format(end.time,"%a %d %b %Y %H:%M:%S"),"] Process completed (RI=",result.ri,") in ",total.time,"\n",sep="")

start.time <- Sys.time();
cat("[",format(start.time,"%a %d %b %Y %H:%M:%S"),"] Processing adjusted Rand index\n",sep="")
	result.ari <- randIndex(membership.old,membership.new,correct=TRUE)
end.time <- Sys.time();
total.time <- end.time - start.time;
cat("[",format(end.time,"%a %d %b %Y %H:%M:%S"),"] Process completed (ARI=",result.ari,") in ",total.time,"\n",sep="")


###############################################################################
# record data
###############################################################################
cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] Record data\n",sep="")
	agreement.file <- paste(folder.data,"agreement.txt",sep="")
	values <- matrix(ncol=1,nrow=2,data=c(result.ri, result.ari))
	rownames(values) <- c("RI", "ARI")
	write.table(means,mean.file,row.names=TRUE,col.names=FALSE)
