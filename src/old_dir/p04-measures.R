# Plots and test the distribution of the role
# measures (using the raw data).
#
# version: 1
# Author: Vincent Labatut 09/2013
#
# setwd("~/eclipse/workspaces/Networks/Orleans/")
# setwd("C:/Eclipse/workspaces/Networks/Orleans/")
# source("src/old_dir/p04-measures.R")
###############################################################################
library("igraph")
source("src/extras/ecdflt.R")


###############################################################################
# setup files
###############################################################################
folder.data <- "data/"	
file.input <- "rolemeasures.raw.txt"
rolemeas.names <- c("z-score-out", "z-score-in", "participation-out", "participation-in")
sample.size <- 100000


###############################################################################
# load raw data
###############################################################################
start.time <- Sys.time();
cat("[",format(start.time,"%a %d %b %Y %H:%M:%S"),"] Loading raw data\n",sep="")
	file.data <- paste(folder.data,file.input,sep="")
	data <- as.matrix(read.table(file.data))
end.time <- Sys.time();
total.time <- end.time - start.time;
cat("[",format(end.time,"%a %d %b %Y %H:%M:%S"),"] Load completed in ",format(total.time),"\n",sep="")


###############################################################################
# clean raw data
###############################################################################
start.time <- Sys.time();
cat("[",format(start.time,"%a %d %b %Y %H:%M:%S"),"] Cleaning data\n",sep="")
	for(c in 1:ncol(data))
	{	cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] ..Processing col.",c,"\n",sep="")
		
		# get rid of NA (?)
		idx.na <- which(is.na(data[,c]))
		if(length(idx.na)>0)
		{	data[idx.na,c] <- 0
			cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] ....WARNING: ",length(idx.na)," NA values found in the raw data (col.",c,")\n",sep="")
		}
		
		# get rid of infinite values
		idx.inf <- which(is.infinite(data[,c]))# | data[,c]==1.79769e+308)
		if(length(idx.inf)>0)
		{	data[idx.inf,c] <- 0
			cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] ....Replacing ",length(idx.inf)," infinite values by 0 in col.",c,")\n",sep="")
		}
	}
end.time <- Sys.time();
total.time <- end.time - start.time;
cat("[",format(end.time,"%a %d %b %Y %H:%M:%S"),"] Cleaning completed in ",format(total.time),"\n",sep="")


###############################################################################
# sample a few objects
###############################################################################
cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] Sample ",sample.size," objects\n",sep="")
sampled <- sample(x=1:nrow(data),size=sample.size)


###############################################################################
# plot role measure distributions
###############################################################################
start.time <- Sys.time();
cat("[",format(start.time,"%a %d %b %Y %H:%M:%S"),"] Plot role measure distributions\n",sep="")
	for(i in 1:ncol(data))
	{	# histogram
		plot.file <- paste(folder.data,"rolemeasure.",i,".",rolemeas.names[i],".histo.pdf",sep="")
		cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] ..Plot role measure histogram in file ",plot.file,"\n",sep="")
		pdf(file=plot.file, bg="white")
		hist(data[,i],probability=TRUE,breaks=100,main=paste("Distribution of",rolemeas.names[i]),xlab=rolemeas.names[i],col="RED")
		dev.off()
		
		# (partial) cumulative distribution
		plot.file <- paste(folder.data,"rolemeasure.",i,".",rolemeas.names[i],".cumdist.pdf",sep="")
		cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] ..Plot role measure cumulative distribution in file ",plot.file,"\n",sep="")
		pdf(file=plot.file, bg="white")
		ecdflt(x=data[sampled,i], xlab=rolemeas.names[i], main=paste("Complementary Cumulative Distribution of",rolemeas.names[i]), points=1000, log="y", col="RED", complementary=TRUE) #
		dev.off()
	}
end.time <- Sys.time();
total.time <- end.time - start.time;
cat("[",format(end.time,"%a %d %b %Y %H:%M:%S"),"] Plotting completed in ",format(total.time),"\n",sep="")
	

###############################################################################
# process role measure correlations
###############################################################################
cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] Record correlations between measures\n",sep="")
cor.mat <- cor(data)
rownames(cor.mat) <- rolemeas.names
colnames(cor.mat) <- rolemeas.names
cor.file <- paste(folder.data,"rolemeasures.correlations.txt",sep="")
write.table(cor.mat,cor.file,row.names=TRUE,col.names=TRUE)
print(cor.mat)


###############################################################################
# test for power law fitness
###############################################################################
cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] Check for power-law distributions\n",sep="")
fit <- matrix(ncol=2,nrow=ncol(data))
colnames(fit) <- c("p-value","exponent")
rownames(fit) <- rolemeas.names
for(i in 1:ncol(data))
{	cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] ..Processing role measure ",rolemeas.names[i],"\n",sep="")
	plf <- power.law.fit(x=data[sampled,i], implementation="plfit")
	fit[i,"p-value"] <- plf$KS.p
	fit[i,"exponent"] <- plf$alpha
}
print(fit)
power.file <- paste(folder.data,"rolemeasures.powerlawfit.txt",sep="")
write.table(fit,power.file)
