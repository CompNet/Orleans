# Process the centers of the estimated clusters
# using the non-normalized data.
# v1
# 
# Author: Vincent Labatut 06/2013
# source("C:/Eclipse/workspaces/Networks/Orleans/src/main-centers.R")
# source("/home/vlabatut/eclipse/workspaces/Networks/Orleans/src/main-centers.R")
###############################################################################

# data folder 					#TODO update depending on local file system
#folder.data <- "C:/Eclipse/workspaces/Networks/Orleans/data/"	
folder.data <- "/home/vlabatut/eclipse/workspaces/Networks/Orleans/data/"	

# load membership vector
cat("[",format(Sys.time(),"%a %d %b %Y %X"),"] Load membership vector\n",sep="")
membership.file <- paste(folder.data,"normalized.numbered.txt.membership",sep="")
t <- read.table(membership.file)
membership <- t[,2] + 1
t <- NULL; gc()

# load data
cat("[",format(Sys.time(),"%a %d %b %Y %X"),"] Load raw data\n",sep="")
data.file <- paste(folder.data,"data.txt",sep="")
data <- as.matrix(read.table(data.file))
data <- data[,-(1:2)]

# clean data
cat("[",format(Sys.time(),"%a %d %b %Y %X"),"] Clean data\n",sep="")
for(c in 1:ncol(data))
{	# get rid of NA (?)
	cat("[",format(Sys.time(),"%a %d %b %Y %X"),"] ..",c,": Remove NA symbols\n",sep="")
	idx.na <- which(is.na(data[,c]))
	if(length(idx.na)>0)
		data[idx.na,c] <- 0
	
	# get rid of infinite values
	cat("[",format(Sys.time(),"%a %d %b %Y %X"),"] ..",c,": Remove infinite symboles\n",sep="")
	mn <- min(data[!is.infinite(data[,c]),c])
	mx <- max(data[!is.infinite(data[,c]),c])				
	idx.mn <- which(is.infinite(data[,c]) & data[,c]<0)
	idx.mx <- which(is.infinite(data[,c]) & data[,c]>0)
	if(length(idx.mn)>0)
		data[idx.mn,c] <- mn
	if(length(idx.mx)>0)
		data[idx.mx,c] <- mx
}

# process means and standard deviations
cat("[",format(Sys.time(),"%a %d %b %Y %X"),"] Process means, standard deviations and sizes\n",sep="")
nbr.clusters <- length(unique(membership))
means <- matrix(nrow=nbr.clusters,ncol=ncol(data))
stdevs <- matrix(nrow=nbr.clusters,ncol=ncol(data))
sizes <- matrix(nrow=nbr.clusters,ncol=1)
for(i in 1:nbr.clusters)
{	idx <- which(membership==i)
	sizes[i] <- length(idx)
	cat("[",format(Sys.time(),"%a %d %b %Y %X"),"] ..size of cluster ",i,": ",sizes[i],"\n",sep="")
	for(j in 1:ncol(data))
	{	means[i,j] <- mean(data[idx,j])
		cat("[",format(Sys.time(),"%a %d %b %Y %X"),"] ....average measure",j," in cluster ",i,": ",means[i,j],"\n",sep="")
		stdevs[i,j] <- sd(data[idx,j])
		cat("[",format(Sys.time(),"%a %d %b %Y %X"),"] ....standard deviation for measure",j," in cluster ",i,": ",stdevs[i,j],"\n",sep="")
	}
}
print(sizes)
print(means)
print(stdevs)

# record data
cat("[",format(Sys.time(),"%a %d %b %Y %X"),"] Record data\n",sep="")
mean.file <- paste(folder.data,"mean.txt",sep="")
write.table(means,mean.file,row.names=FALSE,col.names=FALSE)
stdev.file <- paste(folder.data,"stdev.txt",sep="")
write.table(stdevs,stdev.file,row.names=FALSE,col.names=FALSE)
size.file <- paste(folder.data,"size.txt",sep="")
write.table(sizes,size.file,row.names=FALSE,col.names=FALSE)
