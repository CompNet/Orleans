# Plots the estimated clusters on a 2D
# space using PCA. We only work on a part
# of the data, otherwise the process is
# too long. Feel free to increase the size
# of the sampling, if your station is a war machine.
#
# version: 1
# Author: Vincent Labatut 09/2013
#
# setwd("~/eclipse/workspaces/Networks/Orleans/")
# setwd("C:/Eclipse/workspaces/Networks/Orleans/")
# source("src/old_dir/p08-plot.R")
###############################################################################


###############################################################################
# setup files
###############################################################################
folder.data <- "data/"	
k <- 6						# TODO we work only on the clusters found for this k
sample.size <- 100000

		
###############################################################################
# load membership vector
###############################################################################
start.time <- Sys.time();
cat("[",format(start.time,"%a %d %b %Y %H:%M:%S"),"] Loading membership vector\n",sep="")
	membership.file <- paste(folder.data,"cluster.k",k,".txt",sep="")
	membership <- as.matrix(read.table(membership.file))[,2] + 1	# the k-means implementation starts numbering clusters from 0
end.time <- Sys.time();
total.time <- end.time - start.time;
cat("[",format(end.time,"%a %d %b %Y %H:%M:%S"),"] Load completed in ",format(total.time),"\n",sep="")


###############################################################################
# load normalized data
###############################################################################
start.time <- Sys.time();
cat("[",format(start.time,"%a %d %b %Y %H:%M:%S"),"] Loading normalized data\n",sep="")
	file.data <- paste(folder.data,"rolemeasures.normalized.txt",sep="")
	data <- as.matrix(read.table(file.data))
end.time <- Sys.time();
total.time <- end.time - start.time;
cat("[",format(end.time,"%a %d %b %Y %H:%M:%S"),"] Load completed in ",format(total.time),"\n",sep="")


###############################################################################
# sample a few objects
###############################################################################
cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] Sample ",sample.size," objects\n",sep="")
sampled <- sample(x=1:nrow(data),size=sample.size)


###############################################################################
# perform PCA
###############################################################################
cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] Perform PCA\n",sep="")
pca.obj <- princomp(data[sampled,])
pca.data <- pca.obj$scores[,1:2]
#write.table(x=pca.data,file=file.pca,row.names=FALSE,col.names=FALSE)
print(pca.obj)
print(summary(pca.obj))


###############################################################################
# record results
###############################################################################
pca.file <- paste(folder.data,"pca.k",k,".summary.txt",sep="")
sink(pca.file)
	print(pca.obj)
	print(summary(pca.obj))
sink()


###############################################################################
# plot result
###############################################################################
plot.file <- paste(folder.data,"pca.k",k,".pdf",sep="")
cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] Plot results in file ",plot.file,"\n",sep="")
pdf(file=plot.file, bg="white")
plot(pca.data[,1], pca.data[,2],col=membership[sampled],xlab="First Principal Component",ylab="Second Principal Component",main=paste("Clusters for k=",k,sep=""))
# if you want to focus on a smaller part of the plot, use this command instead:
# plot(pca.data[,1], pca.data[,2],col=membership[sampled],xlab="First Principal Component",ylab="Second Principal Component",main=paste("Clusters for k=",k,sep=""),xlim=c(0,20),ylim=c(0,20))
dev.off()
