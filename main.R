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
source("CommunityDetection/community-detection.R")
source("GenerateData/generate-data.R")

###############################################################################
# Parameters
###############################################################################
# whether or not to generate data (for testing purposes)
gen.data <- FALSE
# community detection algorithm
comdet.algo <- "LV"			#TODO LV, LVdir
# role measures
role.meas <- "GA"			#TODO GA, GAdir, DLPp, DLPo
# clustering algorithm
clust.algo <- "kmeans"		#TODO kmeans, xmeans, gkmeans, fgkmeans
# data folder (all out and in files must be in this folder)
net.folder <- "data/test/"		#TODO  

###############################################################################
# 0. Detecting communities
###############################################################################
if(gen.data)
{	n <- 1000
	n.clust <- 7
	n.com <- 36
	directed <- TRUE
	generate.data(folder.data=net.folder, role.meas, n,directed,  n.clust, clust.algo, n.com, comdet.algo)
}

###############################################################################
# 1. Detecting communities
###############################################################################
detect.communities(folder.data=net.folder, algo=comdet.algo)
#TODO check for community stability

###############################################################################
# 2. Processing role measures
###############################################################################
process.rolemeas(folder.data=net.folder, role.meas, comdet.algo)

###############################################################################
# 3. Clustering role measures
###############################################################################
cluster.folder <- "ClusterAnalysis"
#1) normalize
#2) apply cluster analysis

###############################################################################
# 4. Additional stats
###############################################################################
# social capitalism indices vs. role measures (correlation, plot)
# degree (distribution, plot)
# degree vs. role measures (correlation, plot)
# community sizes (distribution, plot)
# role measures (distribution, correlation, plot)