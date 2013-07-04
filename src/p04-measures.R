# Plots and test the distribution of the role
# measures (using the raw data).
#
# version: 1
# Author: Vincent Labatut 06/2013
#
# setwd("~/eclipse/workspaces/Networks/Orleans/")
# setwd("C:/Eclipse/workspaces/Networks/Orleans/")
# source("src/p04-measures.R")
###############################################################################
library("igraph")
source("src/ecdflt.R")


###############################################################################
# setup files
###############################################################################
folder.data <- "data/"	
file.input <- "rolemeasures.raw.txt"		# TODO you can possibly change that
measure.names <- c(							# TODO you might change that, if necessary
		"intensity-int-out","intensity-int-in","diversity-out","diversity-in","intensity-ext-out","intensity-ext-in","homogeneity-out","homogeneity-in")
sample.size <- 100000						# TODO processing the whole dataset is to long, so the power-law distribution is tested only on a sample


###############################################################################
# load raw data
###############################################################################
start.time <- Sys.time();
cat("[",format(start.time,"%a %d %b %Y %X"),"] Loading raw data\n",sep="")
	file.data <- paste(folder.data,file.input,sep="")
	data <- as.matrix(read.table(file.data))
end.time <- Sys.time();
total.time <- end.time - start.time;
cat("[",format(end.time,"%a %d %b %Y %X"),"] Load completed in ",total.time,"\n",sep="")


###############################################################################
# clean raw data
###############################################################################
start.time <- Sys.time();
cat("[",format(start.time,"%a %d %b %Y %X"),"] Cleaning data\n",sep="")
	for(c in 1:ncol(data))
	{	cat("[",format(Sys.time(),"%a %d %b %Y %X"),"] ..Processing col.",c,"\n",sep="")
		
		# get rid of NA (?)
		idx.na <- which(is.na(data[,c]))
		if(length(idx.na)>0)
		{	data[idx.na,c] <- 0
			cat("[",format(Sys.time(),"%a %d %b %Y %X"),"] ....WARNING: ",length(idx.na)," NA values found in the raw data (col.",c,")\n")
		}
		
		# get rid of infinite values
		idx.inf <- which(is.infinite(data[,c]))# | data[,c]==1.79769e+308)
		if(length(idx.inf)>0)
		{	data[idx.inf,c] <- 0
			cat("[",format(Sys.time(),"%a %d %b %Y %X"),"] ....Replacing ",length(idx.inf)," infinite values by 0 in col.",c,")\n")
		}
	}
end.time <- Sys.time();
total.time <- end.time - start.time;
cat("[",format(end.time,"%a %d %b %Y %X"),"] Cleaning completed in ",total.time,"\n",sep="")


###############################################################################
# sample a few objects
###############################################################################
cat("[",format(Sys.time(),"%a %d %b %Y %X"),"] Sample ",sample.size," objects\n",sep="")
sampled <- sample(x=1:nrow(data),size=sample.size)


###############################################################################
# plot role measure distributions
###############################################################################
start.time <- Sys.time();
cat("[",format(start.time,"%a %d %b %Y %X"),"] Plot role measure distributions\n",sep="")
	for(i in 1:ncol(data))
	{	# histogram
		plot.file <- paste(folder.data,"rolemeasure.",i,".",measure.names[i],".histo.pdf",sep="")
		cat("[",format(Sys.time(),"%a %d %b %Y %X"),"] ..Plot role measure histogram in file ",plot.file,"\n",sep="")
		pdf(file=plot.file, bg="white")
		hist(data[,i],probability=TRUE,breaks=100,main=paste("Distribution of",measure.names[i]),xlab=measure.names[i],col="RED")
		dev.off()
		
		# (partial) cumulative distribution
		plot.file <- paste(folder.data,"rolemeasure.",i,".",measure.names[i],".cumdist.pdf",sep="")
		cat("[",format(Sys.time(),"%a %d %b %Y %X"),"] ..Plot role measure cumulative distribution in file ",plot.file,"\n",sep="")
		pdf(file=plot.file, bg="white")
		ecdflt(x=data[sampled,i], xlab=measure.names[i], main=paste("Complementary Cumulative Distribution of",measure.names[i]), points=1000, log="y", col="RED", complementary=TRUE) #
		dev.off()
	}
end.time <- Sys.time();
total.time <- end.time - start.time;
cat("[",format(end.time,"%a %d %b %Y %X"),"] Plotting completed in ",total.time,"\n",sep="")
	

###############################################################################
# process role measure correlations
###############################################################################
cat("[",format(Sys.time(),"%a %d %b %Y %X"),"] Record correlations between measures\n",sep="")
cor.mat <- cor(data)
rownames(cor.mat) <- measure.names
colnames(cor.mat) <- measure.names
cor.file <- paste(folder.data,"rolemeasures.correlations.txt",sep="")
write.table(cor.mat,cor.file,row.names=TRUE,col.names=TRUE)
print(cor.mat)


###############################################################################
# test for power law fitness
###############################################################################
cat("[",format(Sys.time(),"%a %d %b %Y %X"),"] Check for power-law distributions\n",sep="")
fit <- matrix(ncol=2,nrow=ncol(data))
colnames(fit) <- c("p-value","exponent")
rownames(fit) <- measure.names
for(i in 1:ncol(data))
{	cat("[",format(Sys.time(),"%a %d %b %Y %X"),"] ..Processing role measure ",measure.names[i],"\n",sep="")
	plf <- power.law.fit(x=data[sampled,i], implementation="plfit")
	fit[i,"p-value"] <- plf$KS.p
	fit[i,"exponent"] <- plf$alpha
}
print(fit)
power.file <- paste(folder.data,"rolemeasures.powerlawfit.txt",sep="")
write.table(fit,power.file)
