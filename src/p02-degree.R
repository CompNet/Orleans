# Processes various calculations on the degree.
# 
# Version: 1
# Author: Vincent Labatut 06/2013
#
# source("C:/Eclipse/workspaces/Networks/Orleans/src/main-degree.R")
# source("/home/vlabatut/eclipse/workspaces/Networks/Orleans/src/main-degree.R")
###############################################################################
library("igraph")
source("src/ecdflt.R")


###############################################################################
# setup files
###############################################################################
folder.data <- "data/"	
file.input <- "degrees.txt"		# TODO you can possibly change that
degree.names <- c(							# TODO you might change that, if necessary
		"out", "in", "all")


###############################################################################
# load degree data
###############################################################################
start.time <- Sys.time();
cat("[",format(start.time,"%a %d %b %Y %X"),"] Loading degree data\n",sep="")
	# load the data
	file.deg <- paste(folder.data,file.input,sep="")
	degrees <- as.matrix(read.table(file.deg))
	# possibly add the total degree, if missing
	if(ncol(degrees)==2)
		degrees <- cbind(degrees,degrees[,1]+degrees[,2])
end.time <- Sys.time();
total.time <- end.time - start.time;
cat("[",format(end.time,"%a %d %b %Y %X"),"] Load completed in ",total.time,"\n",sep="")


###############################################################################
# plot degree distributions
###############################################################################
start.time <- Sys.time();
cat("[",format(start.time,"%a %d %b %Y %X"),"] Plot degree distributions\n",sep="")
	for(i in 1:ncol(degrees))
	{	deg.name <- paste(degree.names[i],"-degree",sep="")
		
		# histogram
		plot.file <- paste(folder.data,"degree.",degree.names[i],".histo.pdf",sep="")
		cat("[",format(Sys.time(),"%a %d %b %Y %X"),"] ..Plot ",deg.name," histogram in file ",plot.file,"\n",sep="")
		pdf(file=plot.file, bg="white")
		hist(degrees[,i],probability=TRUE,breaks=100,main=paste("Distribution of",deg.name),xlab=deg.name,col="RED")
		dev.off()
		
		# cumulative distribution
		plot.file <- paste(folder.data,"degree.",degree.names[i],".cumdist.pdf",sep="")
		cat("[",format(Sys.time(),"%a %d %b %Y %X"),"] ..Plot ",deg.name," cumulative distribution in file ",plot.file,"\n",sep="")
		pdf(file=plot.file, bg="white")
		ecdflt(x=degrees[,i], xlab=deg.name, main=paste("Distribution of",deg.name), col="RED") #, log="y"
		dev.off()
	}
end.time <- Sys.time();
total.time <- end.time - start.time;
cat("[",format(end.time,"%a %d %b %Y %X"),"] Plotting completed in ",total.time,"\n",sep="")


###############################################################################
# load raw data
###############################################################################
start.time <- Sys.time();
cat("[",format(start.time,"%a %d %b %Y %X"),"] Loading raw data\n",sep="")
	file.data <- paste(folder.data,file.input,sep="")
	data <- as.matrix(read.table(file.data))
end.time <- Sys.time();
total.time <- end.time - start.time;
cat("[",format(end.time,"%a %d %b %Y %X"),"] Load completed in ",total.time,"\n",sep="")


###############################################################################
# clean raw data
###############################################################################
start.time <- Sys.time();
cat("[",format(start.time,"%a %d %b %Y %X"),"] Cleaning data\n",sep="")
	for(c in 1:ncol(data))
	{	cat("[",format(Sys.time(),"%a %d %b %Y %X"),"] ..Processing col.",c,"\n",sep="")
		
		# get rid of NA (?)
		idx.na <- which(is.na(data[,c]))
		if(length(idx.na)>0)
		{	data[idx.na,c] <- 0
			cat("[",format(Sys.time(),"%a %d %b %Y %X"),"] ....WARNING: ",length(idx.na)," NA values found in the raw data (col.",c,")\n")
		}
		
		# get rid of infinite values
		idx.inf <- which(is.infinite(data[,c]) | data[,c]==1.79769e+308)
		if(length(idx.inf)>0)
		{	data[idx.inf,c] <- 0
			cat("[",format(Sys.time(),"%a %d %b %Y %X"),"] ....Replacing ",length(idx.inf)," infinite values by 0 in col.",c,")\n")
		}
	}
end.time <- Sys.time();
total.time <- end.time - start.time;
cat("[",format(end.time,"%a %d %b %Y %X"),"] Cleaning completed in ",total.time,"\n",sep="")


###############################################################################
# process degree vs. measure correlations
###############################################################################
cat("[",format(Sys.time(),"%a %d %b %Y %X"),"] Process and record correlations between measures\n",sep="")
cor.mat <- cor(degrees, data)
cor.file <- paste(folder.data,"degrees-measures.correlations.txt",sep="")
write.table(cor.mat,cor.file,row.names=FALSE,col.names=FALSE)
