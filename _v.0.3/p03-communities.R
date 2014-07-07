# Analyzes the communities: process their size distribution,
# then tries to fit it to a power law.
# 
# Expected file: a single vector of community numbers, one
# for each node in the network. So in this file we have as 
# many lines as nodes. Communities are supposed to be numbered
# starting from 1.
#
# version: 1
# Author: Vincent Labatut 06/2013
#
# setwd("~/eclipse/workspaces/Networks/Orleans/")
# setwd("C:/Eclipse/workspaces/Networks/Orleans/")
# source("src/p03-communities.R")
###############################################################################
library("igraph")
source("src/extras/ecdflt.R")


###############################################################################
# setup files
###############################################################################
folder.data <- "data/"	
file.input <- "communities.txt"		# TODO you can possibly change that


###############################################################################
# load community membership
###############################################################################
start.time <- Sys.time();
cat("[",format(start.time,"%a %d %b %Y %H:%M:%S"),"] Loading community membership\n",sep="")
	file.com <- paste(folder.data,file.input,sep="")
	communities <- as.matrix(read.table(file.com))
	# just in case communities start from 0
	if(min(communities)==0)
		communities <- communities + 1
end.time <- Sys.time();
total.time <- end.time - start.time;
cat("[",format(end.time,"%a %d %b %Y %H:%M:%S"),"] Load completed in ",format(total.time),"\n",sep="")


###############################################################################
# process community size distribution
###############################################################################
start.time <- Sys.time();
cat("[",format(start.time,"%a %d %b %Y %H:%M:%S"),"] Process community sizes\n",sep="")
	com.nbr <- length(unique(communities))
	com.sizes <- rep(0,com.nbr)
	for(i in 1:length(communities))
	{	com <- communities[i]
		if(i%%100000==0)
			cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] ..Processing node ",i," (",com,") / ",length(communities),"\n",sep="")
		com.sizes[com] <- com.sizes[com] + 1
	}
	comsize.file <- paste(folder.data,"communities.sizes.txt",sep="")
	write.table(com.sizes, comsize.file, row.names=FALSE, col.names=FALSE)
end.time <- Sys.time();
total.time <- end.time - start.time;
cat("[",format(end.time,"%a %d %b %Y %H:%M:%S"),"] Processed in ",format(total.time)," (",length(com.sizes)," communities)\n",sep="")


###############################################################################
# plot community size distributions
###############################################################################
cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] Plot community sizes\n",sep="")
	plot.file <- paste(folder.data,"communities.sizes.histo.pdf",sep="")
	cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] ..Histogram plotted in file ",plot.file,"\n",sep="")
	pdf(file=plot.file, bg="white")
	hist(com.sizes,probability=TRUE,breaks=5,main="Community Size Distribution",xlab="Community Size",col="RED")
	dev.off()
	
	plot.file <- paste(folder.data,"communities.sizes.cumdist.pdf",sep="")
	cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] ..Cumulative distribution plotted in file ",plot.file,"\n",sep="")
	pdf(file=plot.file, bg="white")
	ecdflt(x=com.sizes, xlab="Community Size", main="Community Size Complementary Cumulative Distribution", log="y", col="RED", complementary=TRUE)
	dev.off()


###############################################################################
# test for power law distribution
###############################################################################
start.time <- Sys.time();
cat("[",format(start.time,"%a %d %b %Y %H:%M:%S"),"] Check for power-law distributions\n",sep="")
	fit <- matrix(ncol=2,nrow=1)
	colnames(fit) <- c("p-value","exponent")
	plf <- power.law.fit(x=com.sizes, implementation="plfit")
	fit[1,"p-value"] <- plf$KS.p
	fit[1,"exponent"] <- plf$alpha
	print(plf)
	power.file <- paste(folder.data,"communities.sizes.powerlawfit.txt",sep="")
	write.table(fit,power.file,row.names=FALSE)
end.time <- Sys.time();
total.time <- end.time - start.time;
cat("[",format(end.time,"%a %d %b %Y %H:%M:%S"),"] Process completed in ",format(total.time),"\n",sep="")
