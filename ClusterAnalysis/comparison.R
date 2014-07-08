# Generates some artificial data and compares the available clustering tools.
#
# version: 1
# Author: Vincent Labatut 02/2013
#
# setwd("~/eclipse/workspaces/Networks/Orleans/")
# setwd("C:/Eclipse/workspaces/Networks/Orleans/")
# source("ClusterAnalysis/clust.tool.comp.R")
###############################################################################
library("flexclust")					# rand index

source("ClusterAnalysis/gkmeans.R")		# (fast) global k-means
source("ClusterAnalysis/xmeans.R")		# x-means


###############################################################################
# setup files
###############################################################################
folder.data <- "Data/"
folder.out <- paste(folder.data,"clust.tool.comp",sep="")
if(!file.exists(folder.out))
	dir.create(folder.out)
folder.out <- paste(folder.out,"/",sep="")


###############################################################################
# init parameters
###############################################################################
ns.cluster <- c(5,10)							# numbers of clusters
ns.instances <- c(100,1000,10000				# numbers of instances
#	,100000
)
ns.fields <- c(2,10)							# numbers of attributes
algo.names <- c(								# clustering algorithms
	"gkm", "fgkm", 
	"xm"
)			


###############################################################################
# apply process
###############################################################################
options(scipen=999)
for(n.clust in ns.cluster)
{	cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] Processing ",n.clust," clusters\n",sep="")
	
	for(n in ns.instances)
	{	cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] ..Processing ",n," instances\n",sep="")
		
		for(n.fields in ns.fields)
		{	cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] ....Processing ",n.fields," attributes\n",sep="")
			file.base <- paste(folder.out,"clust=",n.clust,".inst=",n,".fields=",n.fields,sep="")
			file.membership <- paste(file.base,".membership.txt",sep="")
			file.data <- paste(file.base,".data.txt",sep="")
			
			if(file.exists(file.membership) & file.exists(file.data))
			{	# load existing data
				cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] ......Loading the existing data\n",sep="")
				membership <- as.matrix(read.table(file.membership))
				x <- as.matrix(read.table(file.data))
			}else
			{	# generate new data
				cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] ......Generating the data\n",sep="")
				x <- matrix(ncol=(n.fields+1),nrow=n)
				cluster.size <- floor(n / n.clust)
				for(c in 1:n.clust)
				{	from <- (c-1)*cluster.size + 1
					nbr <- cluster.size
					if(c==n.clust)
						nbr <- n - from + 1
					to <- from + nbr - 1
					for(f in 1:n.fields)
						x[from:to,f] <- rnorm(n=nbr,mean=10*runif(1),sd=runif(1))
					x[from:to,n.fields+1] <- c
				}
				# randomize instances order
				x <- x[sample(nrow(x)),]
				# separate data and actual clusters
				membership <- x[,ncol(x)]
				x <- x[,-ncol(x)]
				
				# record data and actual clusters
				cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] ......Recording the generated data\n",sep="")
				write.table(x=membership, file=file.membership, row.names=FALSE, col.names=FALSE)
				write.table(x=x, file=file.data, row.names=FALSE, col.names=FALSE)
				# record plots
				file.plot <- paste(file.base,".data.pdf",sep="")
				pdf(file=file.plot, bg="white")
				plot(x=x, col=membership)
				dev.off()
			}
			
			# init the performance table
			file.perf <- paste(file.base,".performance.txt",sep="")
			if(file.exists(file.perf))
			{	# load the existing table
				cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] ......Loading the existing performance table\n",sep="")
				performance <- as.matrix(read.table(file.perf, header=TRUE,row.names=1))
			}else
			{	# create a new table
				performance <- matrix(ncol=3,nrow=length(algo.names))
				rownames(performance) <- algo.names
				colnames(performance) <- c("Clusters","ARI", "Time")
			}	
			
			# apply clustering methods
			cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] ......Performing the clustering\n",sep="")
			for(algo.name in algo.names)
			{	cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] ......Treating algo ",algo.name,"\n",sep="")
				file.membership <- paste(file.base,".",algo.name,".clusters.txt",sep="")
				file.centers <- paste(file.base,".",algo.name,".centers.txt",sep="")
				
				if(file.exists(file.membership) & file.exists(file.centers))
				{	# we do nothing then, actually
					cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] ........Algo has been already applied previously\n",sep="")
				}else
				{	start.time <- Sys.time()
					cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] ........Estimating the clusters\n",sep="")
						if(algo.name=="gkm")
						{	res <- gkmeans(x=x, fast=FALSE, k.bounds=c(2,15), criterion="ASW", trace=TRUE)
							est.membership <- res$cluster
							est.centers <- res$centers
						}else if(algo.name=="fgkm")
						{	res <- gkmeans(x=x, fast=TRUE, k.bounds=c(2,15), criterion="ASW", trace=TRUE)	
							est.membership <- res$cluster
							est.centers <- res$centers
						}else if(algo.name=="xm")
						{	res <- xmeans(x=x, pr.proc=FALSE)
							est.membership <- res$cluster
							est.centers <- res$centers
						}
					end.time <- Sys.time()
					total.time <- end.time - start.time
					cat("[",format(end.time,"%a %d %b %Y %H:%M:%S"),"] ........Estimation completed in ",format(total.time),"\n",sep="")
					time.str <- format(total.time)
					performance[algo.name,"Time"] <- time.str
					
					# compare with actual clusters
					est.clust <- length(unique(est.membership))
					performance[algo.name,"Clusters"] <- est.clust
					ari <- randIndex(as.vector(membership),as.vector(est.membership),correct=TRUE)
					performance[algo.name,"ARI"] <- ari
					
					# record estimated clusters and centers
					cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] ........Recording the estimated data\n",sep="")
					write.table(x=est.membership, file=file.membership, row.names=FALSE, col.names=FALSE)
					write.table(x=est.centers, file=file.centers, row.names=FALSE, col.names=FALSE)
					# record plots
					file.plot <- paste(file.base,".",algo.name,".pdf",sep="")
					pdf(file=file.plot, bg="white")
					plot(x=x, col=est.membership)
					points(est.centers, col=1:nrow(est.centers), pch = 8)
					dev.off()
				}
			}
			
			# record the performance table
			cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] ......Recording the performance table\n",sep="")
			write.table(x=performance, file=file.perf, row.names=TRUE, col.names=TRUE)
		}
	}
}
