# Record the Silhouette performance of a clustering.
# v1.1
# 
# Author: Vincent Labatut 06/2013
# source("C:/Eclipse/workspaces/Networks/Orleans/src/save-perf.R")
###############################################################################

###############################################################################
# Record the specified performance for the specified algorithm.
#
# performance: 
#	silhouette score to be recorded.
# nbr: 
#	number of detected clusters.
# algo:
#	name of the concerned algorithm.
# folder:
#	folder containing the performance file.
###############################################################################
save.performance <- function(performance, nbr, algo, folder)
{	# set full file name
	file.name <- paste(folder,"performances.txt",sep="")
	
	# get possibly existing table
	if(file.exists(file.name))
	{	t <- read.table(file.name, row.names=1)
		t[algo,"Silhouette"] <- performance
		t[algo,"Size"] <- nbr
	}
	else
	{	m <- matrix(nrow=1,ncol=2)
		colnames(m) <- c("Silhouette","Size")
		rownames(m) <- algo
		m[algo,"Silhouette"] <- performance
		m[algo,"Size"] <- nbr
		t <- as.table(m)
	}
	
	# update table
	#print(performance)	
	#print(m)	
	write.table(x=t, file=file.name)
}

###############################################################################
# test
###############################################################################
