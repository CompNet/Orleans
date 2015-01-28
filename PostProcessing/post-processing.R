# Processes various statistics and generates some
# plots once the clusters have been detected.
# 
# Version: 2
# Author: Vincent Labatut 06/2013, 01/2015
#
# setwd("~/eclipse/workspaces/Networks/Orleans/")
# setwd("~/src/Orleans/")
# setwd("C:/Eclipse/workspaces/Networks/Orleans/")
# source("PostProcessing/post-process.R")
###############################################################################
source("PostProcessing/cluster-anova.R")
source("PostProcessing/cluster-original.R")
source("PostProcessing/cluster-pca.R")
source("PostProcessing/cluster-stats.R")
source("PostProcessing/community-stats.R")
source("PostProcessing/compare-values.R")
source("PostProcessing/hive-plot.R")
source("PostProcessing/process-distributions.R")


###############################################################################
# Performs various calculations related to the clusters and/or communities
# and/or measures.
#
# folder.data: folder containing all input and output files.
# role.meas: code representing the used role measures (cf. RoleMeasures/role-measures.R)
# clust.algo: cluster analysis method.
# comdet.algo: community detection algorithm.
# force: whether or not force the recalculation of certain already existing files.
###############################################################################
post.process <- function(folder.data, role.meas, clust.algo, comdet.algo, force=FALSE)
{	
	# process community stats
#	process.community.stats(folder.data, comdet.algo)
	
	# process cluster stats
#	process.cluster.stats(folder.data, role.meas, clust.algo, comdet.algo)
	
	# plot clusters
		# process and plot PCA
#		process.cluster.pca(folder.data, role.meas, clust.algo, comdet.algo, sample.size=NA)
		# original plot from the guimera & amaral paper
#		draw.original.plots(folder.data, role.meas, clust.algo, comdet.algo, sample.size=NA)
	
	# perform anova on clusters
#	process.cluster.anova(folder.data, role.meas, clust.algo, comdet.algo)
	
	# load cluster membership vector
#	start.time <- Sys.time()
#	cat("[",format(start.time,"%a %d %b %Y %H:%M:%S"),"] Loading membership vector\n",sep="")
#		membership.file <- get.cluster.filename(folder.data, role.meas, clust.algo, comdet.algo)
#		mbsp.clusters <- as.vector(as.matrix(read.table(membership.file)))
#	end.time <- Sys.time()
#	total.time <- end.time - start.time
#	cat("[",format(end.time,"%a %d %b %Y %H:%M:%S"),"] Load completed in ",format(total.time),"\n",sep="")
	
	# distributions
		# get the degrees
#		degrees <- retrieve.degrees(folder.data,role.meas,force)
		# overall distribution
#		process.overall.distribution(folder.data, family="degrees", names=get.degree.names(), values=degrees, loglog=FALSE)
		# distribution in function of the clusters
#		process.partition.distribution(folder.data, membership=mbsp.clusters, clusters=TRUE, family="degree", names=get.degree.names(), values=degrees, loglog=FALSE)
		
		# get the community role measures
#		rolemeas.vals <- retrieve.role.measures(folder.data, role.meas, comdet.algo, force)
		# overall distribution
#		family <- paste("comdet=",comdet.algo,".rolemeas=",role.meas,".clust=",clust.algo,".rolemeas",sep="")
#		process.overall.distribution(folder.data, family, names=get.rolemeas.names(role.meas), values=rolemeas.vals, loglog=FALSE)
		# distribution in function of the clusters (we could also do the communities, if needed)
#		process.partition.distribution(folder.data, membership=mbsp.clusters, clusters=TRUE, family, names=get.rolemeas.names(role.meas), values=rolemeas.vals, loglog=FALSE)
		
		# get the social capitalism indices
#		socaps <- retrieve.socap.indices(folder.data,role.meas,force)
		# overall distribution
#		process.overall.distribution(folder.data, family="socap", names=get.socap.names(), values=socaps, loglog=FALSE)
		# distribution in function of the clusters (we could also do the communities, if needed)
#		process.partition.distribution(folder.data, membership=mbsp.clusters, clusters=TRUE, family="socap", names=get.socap.names(), values=socaps, loglog=FALSE)
	
	# comparisons
		# community role measures vs. social capitalism indices
#		process.values.comparison(folder.data, 
#				family1=family, names1=get.rolemeas.names(role.meas), values1=rolemeas.vals,
#				family2="socap", names2=get.socap.names(), values2=socaps) 
		
		# community role measures vs. degrees
#		process.values.comparison(folder.data, 
#			family1=family, names1=get.rolemeas.names(role.meas), values1=rolemeas.vals,
#			family2="degrees", names2=get.degree.names(), values2=degrees) 

	# community sizes (distribution, plot) >> TODO a faire dans d�tection de coms
	
	# distribution of roles (clusters) in communities and vice-versa
	draw.hiveplots(folder.data, role.meas, clust.algo, comdet.algo, sample.size=NA, force)
}
