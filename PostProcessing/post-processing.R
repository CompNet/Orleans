# Processes various statistics and generates some
# plots once the clusters have been detected.
# 
# Version: 2
# Author: Vincent Labatut 06/2013,01/2015
#
# setwd("~/eclipse/workspaces/Networks/Orleans/")
# setwd("~/src/Orleans/")
# setwd("C:/Eclipse/workspaces/Networks/Orleans/")
# source("PostProcess/post-process.R")
###############################################################################
source("ClusterAnalysis/cluster-analysis.R")
source("PostProcessing/cluster-original.R")
source("PostProcessing/cluster-pca.R")
source("PostProcessing/cluster-stats.R")
source("RoleMeasures/role-measures.R")


###############################################################################
# Performs various calculations related to the clusters and/or communities
# and/or measures.
#
# folder.data: folder containing all input and output files.
# role.meas: code representing the used role measures (cf. RoleMeasures/role-measures.R)
# clust.algo: cluster analysis method.
# comdet.algo: community detection algorithm.
###############################################################################
post.process <- function(folder.data, role.meas, clust.algo, comdet.algo)
{	# process cluster stats
#	process.cluster.stats(folder.data, role.meas, clust.algo, comdet.algo)
	
	# plot clusters
		# process and plot PCA
#		process.cluster.pca(folder.data, role.meas, clust.algo, comdet.algo, sample.size=NA)
		# original plot from the guimera & amaral paper
		draw.original.plots(folder.data, role.meas, clust.algo, comdet.algo, sample.size=NA)
	
	# social capitalism indices vs. role measures (correlation, plot)
	# degree (distribution, plot)
	# degree vs. role measures (correlation, plot)
	# community sizes (distribution, plot)
	# role measures (distribution, correlation, plot)
	# record/plot clustering/comdet quality measures
	
}
