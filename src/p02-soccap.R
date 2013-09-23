# Processes various calculations on the social
# capitalists indices.
# 
# Version: 1
# Author: Vincent Labatut 06/2013
#
# setwd("~/eclipse/workspaces/Networks/Orleans/")
# setwd("C:/Eclipse/workspaces/Networks/Orleans/")
# source("src/p02-soccap.R")
###############################################################################
library("igraph")
source("src/ecdflt.R")


###############################################################################
# setup files
###############################################################################
folder.data <- "data/"	
file.input.soccap <- "soccapmeasures.txt"			# TODO you can possibly change that
file.input.rolemeas <- "rolemeasures.raw.txt"		# TODO you can possibly change that
rolemeas.names <- c(						# TODO you might change that, if necessary
		"intensity-int-out","intensity-int-in","diversity-out","diversity-in","intensity-ext-out","intensity-ext-in","homogeneity-out","homogeneity-in")
soccap.names <- c(							# TODO you might change that, if necessary
		"ratio", "overlap")
sample.size <- 100000						# TODO processing the whole dataset is to long, so the power-law distribution is tested only on a sample


###############################################################################
# load social capitalism indices
###############################################################################
start.time <- Sys.time();
cat("[",format(start.time,"%a %d %b %Y %H:%M:%S"),"] Loading social capitalism indices data\n",sep="")
	# load the data
	file.soccap <- paste(folder.data,file.input.soccap,sep="")
	soccap.indices <- as.matrix(read.table(file.soccap))
end.time <- Sys.time();
total.time <- end.time - start.time;
cat("[",format(end.time,"%a %d %b %Y %H:%M:%S"),"] Load completed in ",total.time,"\n",sep="")


###############################################################################
# sample a few objects
###############################################################################
cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] Sample ",sample.size," objects\n",sep="")
sampled <- sample(x=1:nrow(soccap.indices),size=sample.size)


###############################################################################
# plot social capitalism indices distributions
###############################################################################
start.time <- Sys.time();
cat("[",format(start.time,"%a %d %b %Y %H:%M:%S"),"] Plot social capitalism indices distributions\n",sep="")
	for(i in 1:ncol(soccap.indices))
	{	# histogram
		plot.file <- paste(folder.data,soccap.names[i],".histo.pdf",sep="")
		cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] ..Plot ",soccap.names[i]," histogram in file ",plot.file,"\n",sep="")
		pdf(file=plot.file, bg="white")
		hist(soccap.indices[,i],probability=TRUE,breaks=100,main=paste("Distribution of",soccap.names[i]),xlab=soccap.names[i],col="RED")
		dev.off()
		
		# (partial) cumulative distribution
		plot.file <- paste(folder.data,soccap.names[i],".cumdist.pdf",sep="")
		cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] ..Plot ",soccap.names[i]," cumulative distribution in file ",plot.file,"\n",sep="")
		pdf(file=plot.file, bg="white")
		ecdflt(x=soccap.indices[sampled,i], xlab=soccap.names[i], main=paste("Complementary Cumulative Distribution of",soccap.names[i]), log="y", complementary=TRUE, col="RED", points=1000) #
		dev.off()
	}
end.time <- Sys.time();
total.time <- end.time - start.time;
cat("[",format(end.time,"%a %d %b %Y %H:%M:%S"),"] Plotting completed in ",total.time,"\n",sep="")


###############################################################################
# load raw data
###############################################################################
start.time <- Sys.time();
cat("[",format(start.time,"%a %d %b %Y %H:%M:%S"),"] Loading raw data\n",sep="")
	file.data <- paste(folder.data,file.input.rolemeas,sep="")
	data <- as.matrix(read.table(file.data))
end.time <- Sys.time();
total.time <- end.time - start.time;
cat("[",format(end.time,"%a %d %b %Y %H:%M:%S"),"] Load completed in ",total.time,"\n",sep="")


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
		idx.inf <- which(is.infinite(data[,c]))
		if(length(idx.inf)>0)
		{	data[idx.inf,c] <- 0
			cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] ....Replacing ",length(idx.inf)," infinite values by 0 in col.",c,")\n",sep="")
		}
	}
end.time <- Sys.time();
total.time <- end.time - start.time;
cat("[",format(end.time,"%a %d %b %Y %H:%M:%S"),"] Cleaning completed in ",total.time,"\n",sep="")


###############################################################################
# process social capitalism indices vs. role measures correlations
###############################################################################
cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] Process and record correlations between social capitalism indices and role measures\n",sep="")
cor.mat <- cor(soccap.indices, data)
rownames(cor.mat) <- soccap.names
colnames(cor.mat) <- rolemeas.names
cor.file <- paste(folder.data,"soccap.vs.rolemeasures.correlations.txt",sep="")
write.table(cor.mat,cor.file,row.names=TRUE,col.names=TRUE)
print(cor.mat)


###############################################################################
# plot social capitalism indices vs. role measures
###############################################################################
cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] Ploting role measures vs. social capitalism indices\n",sep="")
for(i in 1:ncol(soccap.indices))
{	for(j in 1:ncol(data))
	{	cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] Ploting ",rolemeas.names[j]," vs. ",soccap.names[i],"\n",sep="")
		plot.file <- paste(folder.data,soccap.names[i],".vs.",rolemeas.names[j],".pdf",sep="")
		pdf(file=plot.file, bg="white")
		plot(soccap.indices[sampled,i],data[sampled,j],main=paste(rolemeas.names[j],"vs.",soccap.names[i]),xlab=soccap.names[i],ylab=rolemeas.names[j],col="RED")
		dev.off()
	}
}
