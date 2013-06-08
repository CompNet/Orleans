# Normalizes the data by centring-reducing it.
# v1
# 
# Author: Vincent Labatut 06/2013
# source("C:/Eclipse/workspaces/Networks/Orleans/src/normalize-data.R")
###############################################################################
library("cluster")

###############################################################################
# Loads the raw data, centers and reduces them, then record the resulting 
# normalized data. The result is cached, and may therefore be retrieved from 
# a previous file.
#
# folder.data:
#	Output folder, used when writing/loading normalized data.
# force.process:
#	Whether or not we want to use cached results.
# returns:
#	Normalized data.
###############################################################################
normalize.data <- function(folder.data, force.process=TRUE)
{	file.norm <- paste(folder.data,"normalized.txt",sep="")
	
	# load cached normalized data
	if(file.exists(file.norm) && !force.process)
	{	start.time <- Sys.time();
		cat("[",format(start.time,"%a %d %b %Y %X"),"] Loading normalized data...\n",sep="")
			data <- as.matrix(read.table(file.norm))
		end.time <- Sys.time();
		total.time <- end.time - start.time;
		cat("[",format(end.time,"%a %d %b %Y %X"),"] Load completed in ",total.time,"\n",sep="")
	}
	
	# perform normalization and record normalized data
	else
	{	# load raw data
		start.time <- Sys.time();
		cat("[",format(start.time,"%a %d %b %Y %X"),"] Loading raw data...\n",sep="")
			file.data <- paste(folder.data,"data.txt",sep="")
			data <- as.matrix(read.table(file.data))
			nbr.instances <- nrow(data)
		end.time <- Sys.time();
		total.time <- end.time - start.time;
		cat("[",format(end.time,"%a %d %b %Y %X"),"] Load completed in ",total.time,"\n",sep="")
		
		# normalize data (center-reduce)
		start.time <- Sys.time();
		cat("[",format(start.time,"%a %d %b %Y %X"),"] Normalizing data...\n",sep="")
			for(c in 1:ncol(data))
			{	average <- mean(data[,c])
				stdev <- sd(data[,c])
				data[,c] <- (data[,c] - average) / stdev
			}
		end.time <- Sys.time();
		total.time <- end.time - start.time;
		cat("[",format(end.time,"%a %d %b %Y %X"),"] Normalization completed in ",total.time,"\n",sep="")
		
		# record normalized data
		start.time <- Sys.time();
		cat("[",format(start.time,"%a %d %b %Y %X"),"] Recording normalized data...\n",sep="")
			write.table(x=data,file=file.norm,row.names=FALSE,col.names=FALSE)
		end.time <- Sys.time();
		total.time <- end.time - start.time;
		cat("[",format(end.time,"%a %d %b %Y %X"),"] Recording completed in ",total.time,"\n",sep="")
	}
	
	return(data)
}

###############################################################################
# test
###############################################################################
