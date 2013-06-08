# Performs a Principal Components Analysis on the specified data.
# This allows representing the clusters in a 2D space.
# v1
# 
# Author: Vincent Labatut 06/2013
# source("C:/Eclipse/workspaces/Networks/Orleans/src/perform-pca.R")
###############################################################################
library("cluster")

###############################################################################
# Performs a PCA on the specified data, and record it for later use.
#
# data: 
#	Data, supposedly normalized. Rows are instances, columns are fields.
# folder.data:
#	Output folder, used when writing/loading the PCA results.
# force.process:
#	Whether or not we want to use cached results.
# returns:
#	The two first main components resulting from the PCA.
###############################################################################
perform.pca <- function(data, folder.data, force.process=TRUE)
{	file.pca <- paste(folder.data,"pca.txt",sep="")
	
	# load cached pca data
	if(file.exists(file.pca) && !force.process)
	{	start.time <- Sys.time();
		cat("[",format(start.time,"%a %d %b %Y %X"),"] Loading PCA results...\n",sep="")
			pca <- as.matrix(read.table(file.pca))
		end.time <- Sys.time();
		total.time <- end.time - start.time;
		cat("[",format(end.time,"%a %d %b %Y %X"),"] Load completed in ",total.time,"\n",sep="")
	}
	
	# perform and record pca
	else
	{	start.time <- Sys.time();
		cat("[",format(start.time,"%a %d %b %Y %X"),"] Processing PCA...\n",sep="")
			pca.obj <- princomp(data)
			pca <- pca.obj$scores[,1:2]
			write.table(x=pca,file=file.pca,row.names=FALSE,col.names=FALSE)
		end.time <- Sys.time();
		total.time <- end.time - start.time;
		cat("[",format(end.time,"%a %d %b %Y %X"),"] Process completed in ",total.time,"\n",sep="")
	}
	
	return(pca)
}

###############################################################################
# test
###############################################################################
