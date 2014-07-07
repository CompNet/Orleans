# Processes various calculations related to 
# cluster analysis.
# 
# Version: 2
# Author: Vincent Labatut 06/2013,07/2014
#
# setwd("~/eclipse/workspaces/Networks/Orleans/")
# setwd("C:/Eclipse/workspaces/Networks/Orleans/")
# source("GenerateData/cluster-analysis.R")
###############################################################################

###############################################################################
# Returns the standard cluster filename.
###############################################################################
get.cluster.filename <- function(algo, n.clust)
{	resultat <- paste("clusters.",algo,".k",n.clust,".txt",sep="")
	return(resultat)
}
