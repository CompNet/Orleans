# Process the centers of the estimated clusters
# using the non-normalized data.
# v1
# 
# Author: Vincent Labatut 06/2013
# source("C:/Eclipse/workspaces/Networks/Orleans/src/main-plot-clusters.R")
# source("/home/vlabatut/eclipse/workspaces/Networks/Orleans/src/main-plot-clusters.R")
###############################################################################

# data folder 					#TODO update depending on local file system
#folder.data <- "C:/Eclipse/workspaces/Networks/Orleans/data/"	
folder.data <- "/home/vlabatut/eclipse/workspaces/Networks/Orleans/data/"	

# load membership vector
cat("[",format(Sys.time(),"%a %d %b %Y %X"),"] Load membership vector\n",sep="")
membership.file <- paste(folder.data,"normalized.numbered.txt.membership",sep="")
t <- read.table(membership.file)
membership <- t[,2] + 1
t <- NULL

# load data
cat("[",format(Sys.time(),"%a %d %b %Y %X"),"] Load data\n",sep="")
data.file <- paste(folder.data,"normalized.numbered.txt",sep="")
data <- as.matrix(read.table(data.file))
data <- data[,-1]

# sample a few objects
sample.size <- 100000
cat("[",format(Sys.time(),"%a %d %b %Y %X"),"] Sample ",sample.size," objects\n",sep="")
sampled <- sample(x=1:nrow(data),size=sample.size)

# perform PCA
cat("[",format(Sys.time(),"%a %d %b %Y %X"),"] Perform PCA\n",sep="")
pca.obj <- princomp(data[sampled,])
pca.data <- pca.obj$scores[,1:2]
#write.table(x=pca.data,file=file.pca,row.names=FALSE,col.names=FALSE)
print(pca.obj)
summary(pca.obj)

# record results
anova.file <- paste(folder.data,"pca.summary.txt",sep="")
sink(anova.file)
	print(pca.obj)
	summary(pca.obj)
sink()

# plot result
plot.file <- paste(folder.data,"pca",".pdf",sep="")
cat("[",format(Sys.time(),"%a %d %b %Y %X"),"] Plot results in file ",plot.file,"\n",sep="")
pdf(file=plot.file, bg="white")
plot(data[sampled,1], data[sampled,2],col=membership[sampled],xlab="First Principal Component",ylab="Second Principal Component",main="Cluster")
#plot(data[sampled,1], data[sampled,2],col=membership[sampled],xlab="First Principal Component",ylab="Second Principal Component",main="Cluster",xlim=c(0,20),ylim=c(0,20))
dev.off()
