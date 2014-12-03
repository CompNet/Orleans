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
comdet.algo <- "LV"			#TODO LV, LV-dir, OSLOM-undir-p, OSLOM-dir-p, OSLOM-undir-o, OSLOM-dir-o
# role measures
role.meas <- "GA"			#TODO GA, GA-dir, DLP-p, DLP-o
# clustering algorithm
clust.algo <- "xmeans"		#TODO kmeans, pkmeans, xmeans, gkmeans, fgkmeans
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
# 1. Detecting communities
###############################################################################
#cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] Detect communities\n",sep="")
#detect.communities(folder.data=net.folder, comdet.algo)
#TODO check for community stability

###############################################################################
# 2. Processing role measures
###############################################################################
#cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] Process role measures\n",sep="")
#process.rolemeas(folder.data=net.folder, role.meas, comdet.algo)

###############################################################################
# 3. Clustering role measures
###############################################################################
#cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] Detect clusters\n",sep="")
#detect.clusters(folder.data=net.folder, role.meas, clust.algo, comdet.algo)

###############################################################################
# 4. Additional stats
###############################################################################
# social capitalism indices vs. role measures (correlation, plot)
# degree (distribution, plot)
# degree vs. role measures (correlation, plot)
# community sizes (distribution, plot)
# role measures (distribution, correlation, plot)
# record/plot clustering/comdet quality measures

cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] Generate communities\n",sep="")
