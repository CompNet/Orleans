# Plots and test the distribution of the measures
# using the non-normalized data.
# v1
# 
# Author: Vincent Labatut 06/2013
# source("C:/Eclipse/workspaces/Networks/Orleans/src/main-plot-distributions.R")
# source("/home/vlabatut/eclipse/workspaces/Networks/Orleans/src/main-plot-distributions.R")
###############################################################################
library("igraph")

# data folder 					#TODO update depending on local file system
#folder.data <- "C:/Eclipse/workspaces/Networks/Orleans/data/"	
folder.data <- "/home/vlabatut/eclipse/workspaces/Networks/Orleans/data/"	

# load membership vector
cat("[",format(Sys.time(),"%a %d %b %Y %X"),"] Load membership vector\n",sep="")
membership.file <- paste(folder.data,"normalized.numbered.txt.membership",sep="")
t <- read.table(membership.file)
membership <- t[,2] + 1
t <- NULL; gc()

# load data
cat("[",format(Sys.time(),"%a %d %b %Y %X"),"] Load raw data\n",sep="")
data.file <- paste(folder.data,"data.txt",sep="")
data <- as.matrix(read.table(data.file))
data <- data[,-(1:2)]

# clean data
cat("[",format(Sys.time(),"%a %d %b %Y %X"),"] Clean data\n",sep="")
for(c in 1:ncol(data))
{	# get rid of NA (?)
	idx.na <- which(is.na(data[,c]))
	if(length(idx.na)>0)
		data[idx.na,c] <- 0
	cat("[",format(Sys.time(),"%a %d %b %Y %X"),"] ..",c,": Removed ",length(idx.na)," 'NA' symbols\n",sep="")
	
	# get rid of infinite values
	mn <- min(data[!is.infinite(data[,c]),c])
	mx <- max(data[!is.infinite(data[,c]),c])				
	idx.mn <- which(is.infinite(data[,c]) & data[,c]<0)
	idx.mx <- which(is.infinite(data[,c]) & data[,c]>0)
	if(length(idx.mn)>0)
		data[idx.mn,c] <- mn
	if(length(idx.mx)>0)
		data[idx.mx,c] <- mx
	cat("[",format(Sys.time(),"%a %d %b %Y %X"),"] ..",c,": Removed ",length(idx.mn)," negative and ",length(idx.mx)," positive infinite symboles\n",sep="")
}

# plot measure distributions
cat("[",format(Sys.time(),"%a %d %b %Y %X"),"] Plot measure distributions\n",sep="")
measure.names <- c("diversity-out","diversity-in","intensity-out","intensity-in","homogeneity-out","homogeneity-in")
for(i in 1:ncol(data))
{	plot.file <- paste(folder.data,"measure.",i,".",measure.names[i],".distribution.pdf",sep="")
	cat("[",format(Sys.time(),"%a %d %b %Y %X"),"] ..Plot measure distribution in file ",plot.file,"\n",sep="")
	pdf(file=plot.file, bg="white")
	hist(data[,i],probability=TRUE,breaks=100,main=paste("Distribution of",measure.names[i]),xlab=measure.names[i])
	dev.off()
}

# process measure correlations
cat("[",format(Sys.time(),"%a %d %b %Y %X"),"] Plot and record correlations betwwen measures\n",sep="")
cor.mat <- cor(data)
cor.file <- paste(folder.data,"correlations.txt",sep="")
write.table(cor.mat,cor.file,row.names=FALSE,col.names=FALSE)

# test for power law fitness
cat("[",format(Sys.time(),"%a %d %b %Y %X"),"] Check for power-law distributions\n",sep="")
fit <- matrix(ncol=2,nrow=ncol(data))
colnames(fit) <- c("p-value","exponent")
rownames(fit) <- measure.names
for(i in 1:ncol(data))
{	cat("[",format(Sys.time(),"%a %d %b %Y %X"),"] ..Processing measure ",measure.names[i],"\n",sep="")
	plf <- power.law.fit(x=data[,i], implementation="plfit")
	fit[i,"p-value"] <- plf$KS.p
	fit[i,"exponent"] <- plf$alpha
}
print(fit)
power.file <- paste(folder.data,"power-law-tests.txt",sep="")
write.table(fit,power.file)
