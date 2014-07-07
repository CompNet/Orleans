# Main program allowing to launch the whole process
# on a single network. The parameters must be set
# accordingly.
# 
# Version: 1
# Author: Vincent Labatut 06/2014
#
# setwd("~/eclipse/workspaces/Networks/Orleans/")
# setwd("C:/Eclipse/workspaces/Networks/Orleans/")
# source("main.R")
###############################################################################

###############################################################################
# Parameters
###############################################################################
# whether or not to generate data (for testing purposes)
generate.data <- TRUE
# community detection algorithm
comdet.algo <- "LV"			#TODO LV, LVdir
# role measures
role.meas <- "GA"			#TODO GA, GAdir, DLPp, DLPo
# clustering algorithm
clust.algo <- "kmeans"		#TODO kmeans, xmeans, gkmeans, fgkmeans
# data folder (all out and in files must be in this folder)
net.folder <- "data/test/"		#TODO  

###############################################################################
# Load functions
###############################################################################
source("/GenerateData/generate-data.R")

###############################################################################
# 0. Detecting communities
###############################################################################
if(generate.data)
{	n <- 1000
	n.clust <- 7
	n.com <- 36
	directed <- TRUE
	generate.data(folder.data=net.folder, role.meas, n, n.clust, clust.algo, n.com, comdet.algo)
}

###############################################################################
# 1. Detecting communities
###############################################################################
comdet.folder <- "CommunityDetection"

###############################################################################
# 2. Processing role measures
###############################################################################
role.folder <- "RoleMeasures"


###############################################################################
# 3. Clustering role measures
###############################################################################
cluster.folder <- "ClusterAnalysis"


###############################################################################
# 4. Additional stats
###############################################################################
# social capitalism indices vs. role measures (correlation, plot)
# degree distribution
# degree vs. role measures (correlation, plot)
