# Removes the first index column sometimes present in
# community structure files, which is completely useless
# to us.
#
# Version: v1
# Author: Vincent Labatut 11/2013
#
# setwd("~/eclipse/workspaces/Networks/Orleans/")
# setwd("C:/Eclipse/workspaces/Networks/Orleans/")
# source("src/extras/com.convert.R")
###############################################################################


###############################################################################
# setup files
###############################################################################
folder.data <- "data/"	
file.input <- "communities.txt"		# TODO you can possibly change that


###############################################################################
# load community membership
###############################################################################
start.time <- Sys.time();
cat("[",format(start.time,"%a %d %b %Y %H:%M:%S"),"] Cleaning community membership\n",sep="")
	file.com <- paste(folder.data,file.input,sep="")
	communities <- as.matrix(read.table(file.com))
	record <- FALSE
	
	# possibly remove first column (node indices = not relevant)
	if(ncol(communities)==2)
	{	communities <- communities[,2]
		record <- TRUE
	}
	
	# in case communities start from 0
	if(min(communities)==0)
	{	communities <- communities + 1
		record <- TRUE
	}
	
	# record the resulting communities
	if(record)
		write.table(x=communities,file=file.com,row.names=FALSE,col.names=FALSE)
	
end.time <- Sys.time();
total.time <- end.time - start.time;
cat("[",format(end.time,"%a %d %b %Y %H:%M:%S"),"] Cleaning completed in ",total.time,"\n",sep="")
