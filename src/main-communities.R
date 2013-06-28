# Process the centers of the estimated clusters
# using the non-normalized data.
# v1
# 
# Author: Vincent Labatut 06/2013
# source("C:/Eclipse/workspaces/Networks/Orleans/src/main-plot-communities.R")
# source("/home/vlabatut/eclipse/workspaces/Networks/Orleans/src/main-plot-communities.R")
###############################################################################
library("igraph")

# data folder 					#TODO update depending on local file system
#folder.data <- "C:/Eclipse/workspaces/Networks/Orleans/data/"	
folder.data <- "/home/vlabatut/eclipse/workspaces/Networks/Orleans/data/"	

# load membership vector
cat("[",format(Sys.time(),"%a %d %b %Y %X"),"] Load cluster membership\n",sep="")
membership.file <- paste(folder.data,"normalized.numbered.txt.membership",sep="")
t <- read.table(membership.file)
membership <- t[,2] + 1
t <- NULL;gc()

# load data
cat("[",format(Sys.time(),"%a %d %b %Y %X"),"] Load community membership\n",sep="")
data.file <- paste(folder.data,"data.txt",sep="")
communities <- as.matrix(read.table(data.file))
communities <- communities[,2]; gc()

# process community size distribution
cat("[",format(Sys.time(),"%a %d %b %Y %X"),"] Process community sizes\n",sep="")
com.nbr <- length(unique(communities))
com.sizes <- rep(0,com.nbr)
#com.sizes <- sapply(1:com.nbr,function(x) 
#	{	res<-length(which(communities==x))
#		cat("size of com",c,": ",res,"\n",sep="")
#		return(res)
#	}
#)
for(i in 1:length(communities))
{	com <- communities[i]
	if(i%%100000==0)
		cat("[",format(Sys.time(),"%a %d %b %Y %X"),"] ..Processing node ",i," (",com,")\n",sep="")
	com.sizes[com] <- com.sizes[com] + 1
}
comsize.file <- paste(folder.data,"community.sizes.txt",sep="")
write.table(com.sizes, comsize.file, row.names=TRUE, col.names=FALSE)

# plot community size distribution
cat("[",format(Sys.time(),"%a %d %b %Y %X"),"] Plot community sizes\n",sep="")
plot.file <- paste(folder.data,"community.size.distribution.pdf",sep="")
cat("[",format(Sys.time(),"%a %d %b %Y %X"),"] ..Plot community size distribution in file ",plot.file,"\n",sep="")
pdf(file=plot.file, bg="white")
hist(com.sizes,probability=TRUE,breaks=5,main="Community Size Distribution",xlab="Community Size")
dev.off()

# test for power law fitness
cat("[",format(Sys.time(),"%a %d %b %Y %X"),"] Check for power-law distributions\n",sep="")
fit <- matrix(ncol=2,nrow=1)
colnames(fit) <- c("p-value","exponent")
plf <- power.law.fit(x=data[,i], implementation="plfit")
fit[1,"p-value"] <- plf$KS.p
fit[1,"exponent"] <- plf$alpha
print(plf)
power.file <- paste(folder.data,"power-law-tests.txt",sep="")
write.table(fit,power.file,row.names=FALSE)
