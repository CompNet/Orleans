# Processes various calculations on the degree.
# v1
# 
# Author: Vincent Labatut 06/2013
# source("C:/Eclipse/workspaces/Networks/Orleans/src/main-degree.R")
# source("/home/vlabatut/eclipse/workspaces/Networks/Orleans/src/main-degree.R")
###############################################################################

# data folder 					#TODO update depending on local file system
#folder.data <- "C:/Eclipse/workspaces/Networks/Orleans/data/"	
folder.data <- "/home/vlabatut/eclipse/workspaces/Networks/Orleans/data/"	

# load data
cat("[",format(Sys.time(),"%a %d %b %Y %X"),"] Load raw data\n",sep="")
data.file <- paste(folder.data,"data.txt",sep="")
data <- as.matrix(read.table(data.file))

# clean data
cat("[",format(Sys.time(),"%a %d %b %Y %X"),"] Clean data\n",sep="")
for(c in 1:ncol(data))
{	# get rid of NA (?)
	cat("[",format(Sys.time(),"%a %d %b %Y %X"),"] ..",c,": Remove NA symbols\n",sep="")
	idx.na <- which(is.na(data[,c]))
	if(length(idx.na)>0)
		data[idx.na,c] <- 0
	
	# get rid of infinite values
	cat("[",format(Sys.time(),"%a %d %b %Y %X"),"] ..",c,": Remove infinite symbols\n",sep="")
	idx.inf <- which(is.infinite(data[,c]))
	if(length(idx.inf)>0)
		data[idx.inf,c] <- 0
}

# load degree by cluster
data <- cbind(data, matrix(NA,ncol=5,nrow=nrow(data)))
cat("[",format(Sys.time(),"%a %d %b %Y %X"),"] Load degree by cluster\n",sep="")
for(i in 0:5)
{	cat("[",format(Sys.time(),"%a %d %b %Y %X"),"] ..Load cluster ",i," file\n",sep="")
	cluster.file <- paste(folder.data,"Cluster",i,"_degrees.txt",sep="")
	t <- read.table(cluster.file,sep=";")
	deg <- as.matrix(t)
	idx <- deg[,1] + 1
	data[idx,(ncol(data)-4):(ncol(data)-3)] <- deg[,2:3]
	data[idx,ncol(data)-2] <- deg[,2] + deg[,3]
	data[idx,(ncol(data)-1):ncol(data)] <- deg[,4:5]
}

# names
names <- c("id","community","intensity-int-out","intensity-int-in","diversity-out","diversity-in","intensity-ext-out","intensity-ext-in","homogeneity-out","homogeneity-in","degree-all","degree-out","degree-in","ratio","overlap")
colnames(data) <- names

# process correlations
cat("[",format(Sys.time(),"%a %d %b %Y %X"),"] Process degrees vs. measures correlations\n",sep="")
cor.file <- paste(folder.data,"correlations.txt",sep="")
cor.mat <- cor(data[,3:10],data[,11:13])
write.table(cor.mat,cor.file,row.names=FALSE,col.names=FALSE)
