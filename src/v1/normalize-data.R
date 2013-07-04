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
		cat("[",format(start.time,"%a %d %b %Y %H:%M:%S"),"] Loading normalized data...\n",sep="")
			data <- as.matrix(read.table(file.norm))
		end.time <- Sys.time();
		total.time <- end.time - start.time;
		cat("[",format(end.time,"%a %d %b %Y %H:%M:%S"),"] Load completed in ",total.time,"\n",sep="")
	}
	
	# perform normalization and record normalized data
	else
	{	# load raw data
		start.time <- Sys.time();
		cat("[",format(start.time,"%a %d %b %Y %H:%M:%S"),"] Loading raw data...\n",sep="")
			file.data <- paste(folder.data,"data.txt",sep="")
			t <- read.table(file.data)
			data <- as.matrix(t)
data <- data[,-(1:2)] # must ignore first two columns
		end.time <- Sys.time();
		total.time <- end.time - start.time;
		cat("[",format(end.time,"%a %d %b %Y %H:%M:%S"),"] Load completed in ",total.time,"\n",sep="")
		
		# normalize data (center-reduce)
		start.time <- Sys.time();
		cat("[",format(start.time,"%a %d %b %Y %H:%M:%S"),"] Normalizing data...\n",sep="")
			for(c in 1:ncol(data))
			{	# get rid of NA (?)
				idx.na <- which(is.na(data[,c]))
				if(length(idx.na)>0)
					data[idx.na,c] <- 0
				
				# get rid of infinite values
#				mn <- min(data[!is.infinite(data[,c]),c])
#				mx <- max(data[!is.infinite(data[,c]),c])				
#				idx.mn <- which(is.infinite(data[,c]) & data[,c]<0)
#				idx.mx <- which(is.infinite(data[,c]) & data[,c]>0)
#				if(length(idx.mn)>0)
#					data[idx.mn,c] <- mn
#				if(length(idx.mx)>0)
#					data[idx.mx,c] <- mx
				idx.inf <- which(is.infinite(data[,c]))
				if(length(idx.inf)>0)
					data[idx.inf,c] <- 0
			
				# normalize
				average <- mean(data[,c])
				stdev <- sd(data[,c])
				cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] .Column ",c,": avg=",average," stdev=",stdev,"\n",sep="")
				data[,c] <- (data[,c] - average) / stdev
			}
		end.time <- Sys.time();
		total.time <- end.time - start.time;
		cat("[",format(end.time,"%a %d %b %Y %H:%M:%S"),"] Normalization completed in ",total.time,"\n",sep="")
		
		# record normalized data
		start.time <- Sys.time();
		cat("[",format(start.time,"%a %d %b %Y %H:%M:%S"),"] Recording normalized data...\n",sep="")
			write.table(x=data,file=file.norm,row.names=FALSE,col.names=FALSE)
		end.time <- Sys.time();
		total.time <- end.time - start.time;
		cat("[",format(end.time,"%a %d %b %Y %H:%M:%S"),"] Recording completed in ",total.time,"\n",sep="")
	}
	
	return(data)
}

###############################################################################
# test
###############################################################################
#folder.data <- "/home/vlabatut/eclipse/workspaces/Networks/Orleans/data/"
#file.norm <- paste(folder.data,"normalized.txt",sep="")
#file.int.data <- paste(folder.data,"data.internal.txt",sep="")
#file.ext.data <- paste(folder.data,"data.external.txt",sep="")
#data <- as.matrix(read.table(file.int.data,sep=";"))[,-5]
#mx <- max(data)
#for(c in 3:4)
#{	idx.inf <- which(data[,c]==mx)
#	data[idx.inf,c] <- +Inf
#}
#data <- rbind(data,rep(0,ncol(data)))
#data.ext <- as.matrix(read.table(file.ext.data))
#data[nrow(data),1:2] <- data.ext[nrow(data.ext),1:2]
#	all(data[,2]==data.ext[,2])
#data.ext <- data.ext[,-(1:2)]
#data <- cbind(data,data.ext)
#file.data <- paste(folder.data,"data.txt",sep="")
#write.table(x=data,file=file.data,row.names=FALSE,col.names=FALSE)

#file.norm <- paste(folder.data,"normalized.numbered.txt",sep="")
#data <- data[,-(1:2)]
## >> normalize
#data <- cbind(1:nrow(data),data)
#dim(data)
#write.table(x=data,file=file.norm,row.names=FALSE,col.names=FALSE)

