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
# community detection algorithm
comdet.algo <- ""
# role measures
role.mes <- ""
# clustering algorithm
clust.algo <- ""
# network folder (all produced files are put in this folder)
net.folder <- ""

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
# 4. 
###############################################################################
