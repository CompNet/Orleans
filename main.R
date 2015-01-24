# Main program allowing to launch the whole process
# on a single network. The parameters must be set
# accordingly.
# 
# Version: 1
# Author: Vincent Labatut 06/2014
#
# setwd("~/eclipse/workspaces/Networks/Orleans/")
# setwd("D:/Eclipse/workspaces/Networks/Orleans/")
# source("main.R")
###############################################################################
library("igraph")

source("ClusterAnalysis/cluster-analysis.R")
source("CommunityDetection/community-detection.R")
source("GenerateData/generate-data.R")
source("PostProcessing/post-processing.R")
source("RoleMeasures/role-measures.R")
source("SocapIndices/socap-indices.R")

###############################################################################
# Parameters
###############################################################################
# whether or not to generate data (for testing purposes)
gen.data <- FALSE
# community detection algorithm
comdet.algo <- "LV"			#TODO LV, LV-dir, OSLOM-undir-p, OSLOM-dir-p, OSLOM-undir-o, OSLOM-dir-o
# role measures
role.meas <- "GA"			#TODO GA, GA-dir, DLP-p, DLP-o
# clustering algorithm
clust.algo <- "kmeans"		#TODO kmeans, pkmeans, xmeans, gkmeans, fgkmeans, gpkmeans, fgpkmeans
# data folder (all out and in files must be in this folder)
net.folder <- "data/test/"		#TODO put the network folder

###############################################################################
# 0. Generate data
###############################################################################
if(gen.data)
{	cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] Generate data\n",sep="")
	
	n <- 1000
	n.clust <- 7
	n.com <- 36
	directed <- TRUE
	generate.data(folder.data=net.folder, role.meas, n, directed,  n.clust, clust.algo, n.com, comdet.algo)
}

###############################################################################
# 1. Processing social capitalism indices
###############################################################################
#cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] Process social capitalism indices\n",sep="")
process.socap.indices(folder.data=net.folder, role.meas, force=TRUE)

###############################################################################
# 2. Detecting communities
###############################################################################
#cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] Detect communities\n",sep="")
#detect.communities(folder.data=net.folder, comdet.algo)
#TODO check for community stability

###############################################################################
# 3. Processing community role measures
###############################################################################
#cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] Process community role measures\n",sep="")
#process.role.measures(folder.data=net.folder, role.meas, comdet.algo)

###############################################################################
# 4. Clustering role measures
###############################################################################
#cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] Detect clusters\n",sep="")
#detect.clusters(folder.data=net.folder, role.meas, clust.algo, comdet.algo)

###############################################################################
# 5. Additional stats and plots
###############################################################################
cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] Perform post processing\n",sep="")
#post.process(folder.data=net.folder, role.meas, clust.algo, comdet.algo)

###############################################################################
cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] Done\n",sep="")
