# Generates hive plots of the network.
#
# version: 1
# Author: Vincent Labatut 09/2013
#
# setwd("~/eclipse/workspaces/Networks/Orleans/")
# setwd("C:/Eclipse/workspaces/Networks/Orleans/")
# source("src/p11-hive.R")
###############################################################################
library("HiveR")


###############################################################################
# setup files
###############################################################################
folder.data <- "data/"	
k <- 6											# TODO we work only on the clusters found for this k
file.input.soccap <- "soccapmeasures.txt"		# TODO you can possibly change that
file.input.rolemeas <- "rolemeasures.raw.txt"	# TODO you can possibly change that
file.input.network <- "network.edgelist"		# TODO you can possibly change that
rolemeas.names <- c(							# TODO you might change that, if necessary
		"intensity-int-out","intensity-int-in","diversity-out","diversity-in","intensity-ext-out","intensity-ext-in","homogeneity-out","homogeneity-in")
soccap.names <- c(								# TODO you might change that, if necessary
		"ratio", "overlap")
sample.size <- 100000							# TODO processing the whole dataset is to long, so the power-law distribution is tested only on a sample


###############################################################################
# load social capitalism indices
###############################################################################
start.time <- Sys.time();
cat("[",format(start.time,"%a %d %b %Y %H:%M:%S"),"] Loading social capitalism indices data\n",sep="")
	# load the data
	file.soccap <- paste(folder.data,file.input.soccap,sep="")
	soccap.indices <- as.matrix(read.table(file.soccap))
end.time <- Sys.time();
total.time <- end.time - start.time;
cat("[",format(end.time,"%a %d %b %Y %H:%M:%S"),"] Load completed in ",total.time,"\n",sep="")


###############################################################################
# sample a few objects
###############################################################################
cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] Sample ",sample.size," objects\n",sep="")
sampled <- sample(x=1:nrow(soccap.indices),size=sample.size)


###############################################################################
# identify social capitalists
###############################################################################
start.time <- Sys.time();
cat("[",format(start.time,"%a %d %b %Y %H:%M:%S"),"] Detecting social capitalists\n",sep="")
	soccap.indices <- as.matrix(read.table(file.soccap))
	soccap.status <- rep(x="User",times=sample.size)
	soccap.idx <- which(soccap.indices[,2]>0.8)
	temp.idx <- which(soccap.indices[soccap.idx,1]>1)
	ifyfm.idx <- soccap.idx[temp.idx]
	soccap.status[ifyfm.idx] <- "IFYFM"
	fmify.idx <- soccap.idx[-temp.idx]
	soccap.status[fmify.idx] <- "FMIFY"
	soccap.indices <- NULL; gc()
end.time <- Sys.time();
total.time <- end.time - start.time;
cat("[",format(end.time,"%a %d %b %Y %H:%M:%S"),"] Process completed in ",total.time,"\n",sep="")


###############################################################################
# load raw data (i.e. role mesures)
###############################################################################
start.time <- Sys.time();
cat("[",format(start.time,"%a %d %b %Y %H:%M:%S"),"] Loading raw data\n",sep="")
	file.data <- paste(folder.data,file.input.rolemeas,sep="")
	data <- as.matrix(read.table(file.data))
end.time <- Sys.time();
total.time <- end.time - start.time;
cat("[",format(end.time,"%a %d %b %Y %H:%M:%S"),"] Load completed in ",total.time,"\n",sep="")


###############################################################################
# clean raw data
###############################################################################
start.time <- Sys.time();
cat("[",format(start.time,"%a %d %b %Y %H:%M:%S"),"] Cleaning data\n",sep="")
	for(c in 1:ncol(data))
	{	cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] ..Processing col.",c,"\n",sep="")
		
		# get rid of NA (?)
		idx.na <- which(is.na(data[,c]))
		if(length(idx.na)>0)
		{	data[idx.na,c] <- 0
			cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] ....WARNING: ",length(idx.na)," NA values found in the raw data (col.",c,")\n",sep="")
		}
		
		# get rid of infinite values
		idx.inf <- which(is.infinite(data[,c]))
		if(length(idx.inf)>0)
		{	data[idx.inf,c] <- 0
			cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] ....Replacing ",length(idx.inf)," infinite values by 0 in col.",c,")\n",sep="")
		}
	}
end.time <- Sys.time();
total.time <- end.time - start.time;
cat("[",format(end.time,"%a %d %b %Y %H:%M:%S"),"] Cleaning completed in ",total.time,"\n",sep="")


###############################################################################
# read the graph
###############################################################################
start.time <- Sys.time();
cat("[",format(start.time,"%a %d %b %Y %H:%M:%S"),"] Load graph\n",sep="")
	links <- read.table(file.input.network)
end.time <- Sys.time();
total.time <- end.time - start.time;
cat("[",format(end.time,"%a %d %b %Y %H:%M:%S"),"] Loading completed in ",total.time,"\n",sep="")
	

###############################################################################
# retain only sampled links
###############################################################################
start.time <- Sys.time();
cat("[",format(start.time,"%a %d %b %Y %H:%M:%S"),"] Sample graph\n",sep="")
	idx1 <- match(sampled,links[,1])
	idx1 <- idx1[!is.na(idx1)]
	idx2 <- match(sampled,links[,2])
	idx2 <- idx2[!is.na(idx2)]
	idx <- intersect(idx1, idx2)
	links <- links[idx,]
	idx1 <- NULL; idx2 <- NULL; idx <- NULL; gc()
end.time <- Sys.time();
total.time <- end.time - start.time;
cat("[",format(end.time,"%a %d %b %Y %H:%M:%S"),"] Process completed in ",total.time,"\n",sep="")
	

###############################################################################
# generate hive plots
###############################################################################
edge2HPD(edge_df=links, axis.cols=NULL, ...)