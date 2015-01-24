# Plots the estimated clusters on a 2D
# space using PCA. We only work on a part
# of the data, otherwise the process is
# too long. Feel free to increase the size
# of the sample, if your station is a war machine.
#
# version: 1
# Author: Vincent Labatut 06/2013
#
# setwd("~/eclipse/workspaces/Networks/Orleans/")
# setwd("C:/Eclipse/workspaces/Networks/Orleans/")
# source("PostProcessing/cluster-pca.R")
###############################################################################


###############################################################################
# Returns the standard filename for the cluster PCA plot.
#
# folder.data: folder containing all input and output files.
# role.meas: type of role measures.
# n.clust: number of detected clusters (optional).
# clust.algo: cluster analysis algorithm.
# comdet.algo: community detection algorithm.
# sample.size: size of the considered data sample.
###############################################################################
get.cluster.plot.filename <- function(folder.data, role.meas, clust.algo, comdet.algo, n.clust=0, sample.size)
{	result <- paste(folder.data,"comdet=",comdet.algo,sep="")
	result <- paste(result,".rolemeas=",role.meas,sep="")
	result <- paste(result,".clust=",clust.algo,sep="")
	if(n.clust>0)
		result <- paste(result,".k=",n.clust,sep="")
	if(!is.na(sample.size))
		result <- paste(result,".smplsz=",sample.size,sep="")
	result <- paste(result,".clusters.pca.pdf",sep="")
	return(result)
}

###############################################################################
# Returns the standard filename for the cluster PCA data.
#
# folder.data: folder containing all input and output files.
# role.meas: type of role measures.
# n.clust: number of detected clusters (optional).
# clust.algo: cluster analysis algorithm.
# comdet.algo: community detection algorithm.
# sample.size: size of the considered data sample.
###############################################################################
get.cluster.pca.filename <- function(folder.data, role.meas, clust.algo, comdet.algo, n.clust=0, sample.size)
{	result <- paste(folder.data,"comdet=",comdet.algo,sep="")
	result <- paste(result,".rolemeas=",role.meas,sep="")
	result <- paste(result,".clust=",clust.algo,sep="")
	if(n.clust>0)
		result <- paste(result,".k=",n.clust,sep="")
	if(!is.na(sample.size))
		result <- paste(result,".smplsz=",sample.size,sep="")
	result <- paste(result,".clusters.pca.txt",sep="")
	return(result)
}

###############################################################################
# Performs a PCA on the normalized data, then plot the result in 2D, using
# colors to represent clusters.
#
# folder.data: folder containing all input and output files.
# role.meas: type of role measures.
# clust.algo: cluster analysis method.
# comdet.algo: community detection algorithm.
# sample.size: if the PCA takes too long, you can work on a sample of the data.
#			   this parameter specifies the size of this sample (NA to use the
#			   whole dataset).
###############################################################################
process.cluster.pca <- function(folder.data, role.meas, clust.algo, comdet.algo, sample.size=NA)
{	# load membership vector
	start.time <- Sys.time();
	cat("[",format(start.time,"%a %d %b %Y %H:%M:%S"),"] Loading membership vector\n",sep="")
		membership.file <- get.cluster.filename(folder.data, role.meas, clust.algo, comdet.algo)
		membership <- as.vector(as.matrix(read.table(membership.file)))
	end.time <- Sys.time();
	total.time <- end.time - start.time;
	cat("[",format(end.time,"%a %d %b %Y %H:%M:%S"),"] Load completed in ",format(total.time),"\n",sep="")
	
	# load normalized data (role measures)
	start.time <- Sys.time();
	cat("[",format(start.time,"%a %d %b %Y %H:%M:%S"),"] Loading raw data\n",sep="")
		file.data <- get.rolemeas.filename(folder.data, role.meas, norm=TRUE, comdet.algo)
		data <- as.matrix(read.table(file.data,header=TRUE))
		rolemeas.names <- get.rolemeas.names(role.meas)
	end.time <- Sys.time();
	total.time <- end.time - start.time;
	cat("[",format(end.time,"%a %d %b %Y %H:%M:%S"),"] Load completed in ",format(total.time),"\n",sep="")
	
	# possibly sample a few objects
	if(is.na(sample.size))
	{	cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] No sampling (processing the whole dataset)\n",sep="")
		sampled <- 1:nrow(data)
	}
	else
	{	cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] Sample ",sample.size," objects\n",sep="")
		sampled <- sample(x=1:nrow(data),size=sample.size)
	}
	
	# perform PCA
	cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] Perform PCA\n",sep="")
		pca.obj <- princomp(data[sampled,])
		pca.data <- pca.obj$scores[,1:2]
		#write.table(x=pca.data,file=file.pca,row.names=FALSE,col.names=FALSE)
		print(pca.obj)
		print(summary(pca.obj))
	
	# record results
	pca.file <- get.cluster.pca.filename(folder.data, role.meas, clust.algo, comdet.algo, sample.size=sample.size)
	cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] Record results in file ",pca.file,"\n",sep="")
		sink(pca.file)
			print(pca.obj)
			print(summary(pca.obj))
		sink()

	# plot result
	plot.file <- get.cluster.plot.filename(folder.data, role.meas, clust.algo, comdet.algo, sample.size=sample.size)
	cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] Plot results in file ",plot.file,"\n",sep="")
		pdf(file=plot.file, bg="white")
		plot(pca.data[,1], pca.data[,2],col=(membership[sampled]+1),xlab="First Principal Component",ylab="Second Principal Component",main="Clusters")
		# if you want to focus on a smaller part of the plot, use this command instead:
		# plot(pca.data[,1], pca.data[,2],col=membership[sampled],xlab="First Principal Component",ylab="Second Principal Component",main="Clusters",xlim=c(0,20),ylim=c(0,20))
		dev.off()
}
