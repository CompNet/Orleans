# Process the distance matrix for the specified data.
# v1
# 
# Author: Vincent Labatut 06/2013
# source("C:/Eclipse/workspaces/Networks/Orleans/src/normalize-data.R")
###############################################################################
library("cluster")

###############################################################################
# Process the distance matrix for the specified data and distance function.
# The result is cached, and may therefore be retrieved from a previous file.
#
# folder.data:
#	Output folder, used when writing/loading normalized data.
# dist.function:
#	Function used to process distances.
# force.process:
#	Whether or not we want to use cached results.
# returns:
#	Distance matrix.
###############################################################################
process.distances <- function(folder.data, dist.function, force.process=TRUE)
{	file.dist <- paste(folder.data,"distances.bin",sep="")
	
	# load cached distance matrix
	if(file.exists(file.dist) && !force.process)
	{	start.time <- Sys.time()
		cat("[",format(start.time,"%a %d %b %Y %X"),"] Loading previously processed distances...\n",sep="")
			load(file=file.dist)
		end.time <- Sys.time();
		total.time <- end.time - start.time;
		cat("[",format(end.time,"%a %d %b %Y %X"),"] Load completed in ",total.time,"\n",sep="")
	}
	
	# process and record distance matrix 
	else
	{	
		# process distances
		start.time <- Sys.time();
		cat("[",format(start.time,"%a %d %b %Y %X"),"] Processing distances...\n",sep="")
			dist.matrix <- dist(data, method=dist.function)
		end.time <- Sys.time();
		total.time <- end.time - start.time;
		cat("[",format(end.time,"%a %d %b %Y %X"),"] Distance processing completed in ",total.time,"\n",sep="")
		
		# cache to disk
		start.time <- Sys.time();
		cat("[",format(start.time,"%a %d %b %Y %X"),"] Recording distances for later use...\n",sep="")
			save(dist.matrix,file=file.dist)
		end.time <- Sys.time();
		total.time <- end.time - start.time;
		cat("[",format(end.time,"%a %d %b %Y %X"),"] Distance recording completed in ",total.time,"\n",sep="")
	}
}

###############################################################################
# test
###############################################################################
