# Processes the agreement between cluster analysis tools.
# v1
# 
# Author: Vincent Labatut 06/2013
# source("C:/Eclipse/workspaces/Networks/Orleans/src/process-agreement.R")
###############################################################################
library("cluster")

###############################################################################
# Loads the membership vectors estimated by the various tested clustering tools,
# and compare those partitions using the ARI.
#
# clust.algos:
#	Clustering algorithms to consider
# folder.data:
#	Output folder, used when writing/loading normalized data.
# force.process:
#	Whether or not we want to use cached results.
# returns:
#	Agreement matrix.
###############################################################################
process.agreement <- function(clust.algos, folder.data, force.process=TRUE)
{	file.agreement <- paste(folder.data,"agreement.txt",sep="")
	
	# load cached normalized data
	if(file.exists(file.agreement) && ! force.process)
	{	agreement.matrix <- as.matrix(read.table(file.agreement))
	}
	
	# process agreement and record results
	else
	{
		start.time <- Sys.time();
		cat("[",format(start.time,"%a %d %b %Y %H:%M:%S"),"] Processing agreement data...\n",sep="")
			agreement.matrix <- matrix(nrow=length(clust.algos),ncol=length(clust.algos))
			rownames(agreement.matrix) <- clust.algos
			colnames(agreement.matrix) <- clust.algos
			for(a1 in 1:(length(clust.algos)-1))
			{	file1 <- paste(folder.data,clust.algos[a1],".txt",sep="")
				part1 <- as.matrix(read.table(file1))
				
				for(a2 in (a1+1):length(clust.algos))
				{	file2 <- paste(folder.data,clust.algos[a2],".txt",sep="")
					part2 <- as.matrix(read.table(file2))
					
					val <- randIndex(as.integer(part1),as.integer(part2),correct=TRUE)
					agreement.matrix[a1,a2] <- val
					agreement.matrix[a2,a1] <- val
				}
			}
		end.time <- Sys.time();
		total.time <- end.time - start.time;
		cat("[",format(end.time,"%a %d %b %Y %H:%M:%S"),"] Processing completed in ",total.time,"\n",sep="")
		
		cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] Recording agreement matrix...\n",sep="")
		write.table(x=agreement.matrix, file=file.agreement)
	}
	
	return(agreement.matrix)
}

###############################################################################
# test
###############################################################################
