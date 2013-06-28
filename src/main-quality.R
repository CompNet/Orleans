# Process the quality of estimated clusters
# v1
# 
# Author: Vincent Labatut 06/2013
# source("C:/Eclipse/workspaces/Networks/Orleans/src/main-quality.R")
# source("/home/vlabatut/eclipse/workspaces/Networks/Orleans/src/main-quality.R")
###############################################################################

library("clusterSim")

# data folder 					#TODO update depending on local file system
#folder.data <- "C:/Eclipse/workspaces/Networks/Orleans/data/"	
folder.data <- "/home/vlabatut/eclipse/workspaces/Networks/Orleans/data/"	

# load membership vector
start.time <- Sys.time();
cat("[",format(start.time,"%a %d %b %Y %X"),"] Load membership vector\n",sep="")
	membership.file <- paste(folder.data,"normalized.numbered.txt.membership",sep="")
	t <- read.table(membership.file)
	membership <- t[,2] + 1
	t <- NULL; gc()
end.time <- Sys.time();
total.time <- end.time - start.time;
cat("[",format(end.time,"%a %d %b %Y %X"),"] Load completed in ",total.time,"\n",sep="")
	

# load cluster centers
#centers.file <- paste(folder.data,"normalized.numbered.txt.cluster_centres",sep="")

# load data
#start.time <- Sys.time();
#cat("[",format(start.time,"%a %d %b %Y %X"),"] Load normalized data\n",sep="")
#	data.file <- paste(folder.data,"normalized.numbered.txt",sep="")
#	data <- as.matrix(read.table(data.file))[,-1]
#end.time <- Sys.time();
#total.time <- end.time - start.time;
#cat("[",format(end.time,"%a %d %b %Y %X"),"] Load completed in ",total.time,"\n",sep="")
	

# process measure
start.time <- Sys.time();
cat("[",format(start.time,"%a %d %b %Y %X"),"] Process Davies-Bouldin measure\n",sep="")
	result <- index.DB(x=data, cl=membership, centrotypes="centroids")
	print(result)
end.time <- Sys.time();
total.time <- end.time - start.time;
cat("[",format(end.time,"%a %d %b %Y %X"),"] Processing completed in ",total.time,"\n",sep="")
	
#    2	0.4117679
#	 3	0.7021821
#	 4	0.8353625
#    5	0.8177260		0.817726
#    6	0.9681769
#	 7	0.3453222
#	 8	3.2677520
#	 9	1.0125190
#	10	1.0298560
#	11	0.9910423
#	12	1.3587930
#	13	0.9164861
#	14	1.2381470
#	15	1.2583600

# plot DB measures
# y <- c(0.4117679,0.7021821,0.8353625,0.8177260,0.9681769,0.3453222,3.2677520,1.0125190,1.0298560,0.9910423,1.358793,0.9164861,1.2381470,1.2583600)
# x <- c(2:15)
# plot.file <- paste(folder.data,"davies-bouldin.pdf",sep="")
# pdf(file=plot.file,bg="white")
# plot(x,y,type="n",xlab="Clusters",ylab="Davies-Bouldin index");lines(x,y,col="RED")
# dev.off()
