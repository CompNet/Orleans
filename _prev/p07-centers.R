# Processes statistics regarding the estimated clusters
# using the non-normalized data: average, standard deviation
# and size.
#
# version: 1
# Author: Vincent Labatut 06/2013
#
# setwd("~/eclipse/workspaces/Networks/Orleans/")
# setwd("C:/Eclipse/workspaces/Networks/Orleans/")
# source("src/p07-centers.R")
###############################################################################


###############################################################################
# setup files
###############################################################################
folder.data <- "Data/"	
file.input <- "rolemeasures.raw.txt"		# TODO you can possibly change that
k <- 6										# TODO we work only on the clusters found for this k


###############################################################################
# load membership vector
###############################################################################
start.time <- Sys.time();
cat("[",format(start.time,"%a %d %b %Y %H:%M:%S"),"] Loading membership vector\n",sep="")
	membership.file <- paste(folder.data,"cluster.k",k,".txt",sep="")
	membership <- as.matrix(read.table(membership.file))[,2] + 1	# the k-means implementation starts numbering clusters from 0
end.time <- Sys.time();
total.time <- end.time - start.time;
cat("[",format(end.time,"%a %d %b %Y %H:%M:%S"),"] Load completed in ",total.time,"\n",sep="")
	

###############################################################################
# load raw data
###############################################################################
start.time <- Sys.time();
cat("[",format(start.time,"%a %d %b %Y %H:%M:%S"),"] Loading raw data\n",sep="")
	file.data <- paste(folder.data,file.input,sep="")
	x <- as.matrix(read.table(file.data))
end.time <- Sys.time();
total.time <- end.time - start.time;
cat("[",format(end.time,"%a %d %b %Y %H:%M:%S"),"] Load completed in ",total.time,"\n",sep="")


###############################################################################
# clean raw data
###############################################################################
start.time <- Sys.time();
cat("[",format(start.time,"%a %d %b %Y %H:%M:%S"),"] Cleaning data\n",sep="")
for(c in 1:ncol(x))
{	cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] ..Processing col.",c,"\n",sep="")
	
	# get rid of NA (?)
	idx.na <- which(is.na(x[,c]))
	if(length(idx.na)>0)
	{	x[idx.na,c] <- 0
		cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] ....WARNING: ",length(idx.na)," NA values found in the raw data (col.",c,")\n")
	}
	
	# get rid of infinite values
	idx.inf <- which(is.infinite(x[,c]) | x[,c]==1.79769e+308)
	if(length(idx.inf)>0)
	{	x[idx.inf,c] <- 0
		cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] ....Replacing ",length(idx.inf)," infinite values by 0 in col.",c,")\n")
	}
}
end.time <- Sys.time();
total.time <- end.time - start.time;
cat("[",format(end.time,"%a %d %b %Y %H:%M:%S"),"] Cleaning completed in ",total.time,"\n",sep="")


###############################################################################
# process means and standard deviations
###############################################################################
cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] Process means, standard deviations and sizes\n",sep="")
nbr.clusters <- length(unique(membership))
means <- matrix(nrow=nbr.clusters,ncol=ncol(x))
stdevs <- matrix(nrow=nbr.clusters,ncol=ncol(x))
sizes <- matrix(nrow=nbr.clusters,ncol=1)
for(i in 1:nbr.clusters)
{	idx <- which(membership==i)
	sizes[i] <- length(idx)
	cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] ..Size of cluster ",i,": ",sizes[i],"\n",sep="")
	for(j in 1:ncol(x))
	{	means[i,j] <- mean(x[idx,j])
		cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] ....Average role measure ",j," in cluster ",i,": ",means[i,j],"\n",sep="")
		stdevs[i,j] <- sd(x[idx,j])
		cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] ....Standard deviation for role measure ",j," in cluster ",i,": ",stdevs[i,j],"\n",sep="")
	}
}
print(sizes)
print(means)
print(stdevs)


###############################################################################
# record data
###############################################################################
cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] Record data\n",sep="")
mean.file <- paste(folder.data,"clusters.mean.txt",sep="")
write.table(means,mean.file,row.names=FALSE,col.names=FALSE)
stdev.file <- paste(folder.data,"clusters.stdev.txt",sep="")
write.table(stdevs,stdev.file,row.names=FALSE,col.names=FALSE)
size.file <- paste(folder.data,"clusters.size.txt",sep="")
write.table(sizes,size.file,row.names=FALSE,col.names=FALSE)
