# Normalizes the raw data, so that the clustering tool can be applied later.
# Expected data: a table, values separated by tabulations, in the following
# order:
#	- outgoing internal intensity
#	- incoming internal intensity
#	- outgoing density
#	- incoming density
#	- outgoing external intensity
#	- incoming external intensity
#	- outgoing heterogeneity
#	- incoming heterogeneity
# Infinite values are represented by +Inf or -Inf. NA are not allowed.
#
# version: 1
# Author: Vincent Labatut 06/2013
#
# setwd("~/eclipse/workspaces/Networks/Orleans/")
# setwd("C:/Eclipse/workspaces/Networks/Orleans/")
# source("src/p05-normalize.R")
###############################################################################
library("clusterSim")


###############################################################################
# setup files
###############################################################################
folder.data <- "data/"	
file.input <- "rolemeasures.raw.txt"	# TODO you can possibly change that


###############################################################################
# load raw data
###############################################################################
start.time <- Sys.time();
cat("[",format(start.time,"%a %d %b %Y %X"),"] Loading raw data...\n",sep="")
	file.data <- paste(folder.data,file.input,sep="")
	data <- as.matrix(read.table(file.data))
end.time <- Sys.time();
total.time <- end.time - start.time;
cat("[",format(end.time,"%a %d %b %Y %X"),"] Load completed in ",total.time,"\n",sep="")


###############################################################################
# normalize data (center & reduce)
###############################################################################
start.time <- Sys.time();
cat("[",format(start.time,"%a %d %b %Y %X"),"] Normalizing data...\n",sep="")
	for(c in 1:ncol(data))
	{	cat("[",format(Sys.time(),"%a %d %b %Y %X"),"] ..Processing col.",c,"\n",sep="")
		
		# get rid of NA (?)
		idx.na <- which(is.na(data[,c]))
		if(length(idx.na)>0)
		{	data[idx.na,c] <- 0
			cat("[",format(Sys.time(),"%a %d %b %Y %X"),"] ....WARNING: ",length(idx.na)," NA values found in the raw data (col.",c,")\n")
		}
		
		# get rid of infinite values
		idx.inf <- which(is.infinite(data[,c]))
		if(length(idx.inf)>0)
		{	data[idx.inf,c] <- 0
			cat("[",format(Sys.time(),"%a %d %b %Y %X"),"] ....Replacing ",length(idx.inf)," infinite values by 0 in col.",c,")\n")
		}
		
		# normalize
		average <- mean(data[,c] | data[,c]==1.79769e+308)
		stdev <- sd(data[,c])
		cat("[",format(Sys.time(),"%a %d %b %Y %X"),"] ....Normalizing col.",c,": avg=",average," stdev=",stdev,"\n",sep="")
		data[,c] <- (data[,c] - average) / stdev
	}
end.time <- Sys.time();
total.time <- end.time - start.time;
cat("[",format(end.time,"%a %d %b %Y %X"),"] Normalization completed in ",total.time,"\n",sep="")


###############################################################################
# record normalized data
###############################################################################
start.time <- Sys.time();
cat("[",format(start.time,"%a %d %b %Y %X"),"] Recording normalized data\n",sep="")
	file.norm <- paste(folder.data,"rolemeasures.normalized.txt",sep="")
	write.table(x=data,file=file.norm,row.names=FALSE,col.names=FALSE)
end.time <- Sys.time();
total.time <- end.time - start.time;
cat("[",format(end.time,"%a %d %b %Y %X"),"] Recording completed in ",total.time,"\n",sep="")
