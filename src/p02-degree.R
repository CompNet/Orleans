# Processes various calculations on the degree.
# 
# Version: 1
# Author: Vincent Labatut 06/2013
#
# setwd("~/eclipse/workspaces/Networks/Orleans/")
# setwd("C:/Eclipse/workspaces/Networks/Orleans/")
# source("src/p02-degree.R")
###############################################################################
library("igraph")
source("src/ecdflt.R")


###############################################################################
# setup files
###############################################################################
folder.data <- "data/"	
file.input1 <- "degrees.txt"				# TODO you can possibly change that
file.input2 <- "rolemeasures.raw.txt"		# TODO you can possibly change that
measure.names <- c(							# TODO you might change that, if necessary
		"intensity-int-out","intensity-int-in","diversity-out","diversity-in","intensity-ext-out","intensity-ext-in","homogeneity-out","homogeneity-in")
degree.names <- c(							# TODO you might change that, if necessary
		"out", "in", "all")
sample.size <- 100000						# TODO processing the whole dataset is to long, so the power-law distribution is tested only on a sample


###############################################################################
# load degree data
###############################################################################
start.time <- Sys.time();
cat("[",format(start.time,"%a %d %b %Y %X"),"] Loading degree data\n",sep="")
	# load the data
	file.deg <- paste(folder.data,file.input1,sep="")
	degrees <- as.matrix(read.table(file.deg))
	# possibly add the total degree, if missing
	if(ncol(degrees)==2)
		degrees <- cbind(degrees,degrees[,1]+degrees[,2])
end.time <- Sys.time();
total.time <- end.time - start.time;
cat("[",format(end.time,"%a %d %b %Y %X"),"] Load completed in ",total.time,"\n",sep="")


###############################################################################
# sample a few objects
###############################################################################
cat("[",format(Sys.time(),"%a %d %b %Y %X"),"] Sample ",sample.size," objects\n",sep="")
sampled <- sample(x=1:nrow(degrees),size=sample.size)


###############################################################################
# plot degree distributions
###############################################################################
start.time <- Sys.time();
cat("[",format(start.time,"%a %d %b %Y %X"),"] Plot degree distributions\n",sep="")
	for(i in 1:ncol(degrees))
	{	deg.name <- paste(degree.names[i],"-degree",sep="")
		
		# histogram
		plot.file <- paste(folder.data,"degree.",degree.names[i],".histo.pdf",sep="")
		cat("[",format(Sys.time(),"%a %d %b %Y %X"),"] ..Plot ",deg.name," histogram in file ",plot.file,"\n",sep="")
		pdf(file=plot.file, bg="white")
		hist(degrees[,i],probability=TRUE,breaks=100,main=paste("Distribution of",deg.name),xlab=deg.name,col="RED")
		dev.off()
		
		# (partial) cumulative distribution
		plot.file <- paste(folder.data,"degree.",degree.names[i],".cumdist.pdf",sep="")
		cat("[",format(Sys.time(),"%a %d %b %Y %X"),"] ..Plot ",deg.name," cumulative distribution in file ",plot.file,"\n",sep="")
		pdf(file=plot.file, bg="white")
		ecdflt(x=degrees[sampled,i], xlab=deg.name, main=paste("Complementary Cumulative Distribution of",deg.name), log="y", complementary=TRUE, col="RED", points=1000) #
		dev.off()
	}
end.time <- Sys.time();
total.time <- end.time - start.time;
cat("[",format(end.time,"%a %d %b %Y %X"),"] Plotting completed in ",total.time,"\n",sep="")


###############################################################################
# load raw data
###############################################################################
start.time <- Sys.time();
cat("[",format(start.time,"%a %d %b %Y %X"),"] Loading raw data\n",sep="")
	file.data <- paste(folder.data,file.input2,sep="")
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
		idx.inf <- which(is.infinite(data[,c])) # | data[,c]==1.79769e+308)
		if(length(idx.inf)>0)
		{	data[idx.inf,c] <- 0
			cat("[",format(Sys.time(),"%a %d %b %Y %X"),"] ....Replacing ",length(idx.inf)," infinite values by 0 in col.",c,")\n")
		}
	}
end.time <- Sys.time();
total.time <- end.time - start.time;
cat("[",format(end.time,"%a %d %b %Y %X"),"] Cleaning completed in ",total.time,"\n",sep="")


###############################################################################
# process degree vs. measure correlations
###############################################################################
cat("[",format(Sys.time(),"%a %d %b %Y %X"),"] Process and record correlations between role measures\n",sep="")
cor.mat <- cor(degrees, data)
rownames(cor.mat) <- degree.names
colnames(cor.mat) <- measure.names
cor.file <- paste(folder.data,"degrees-rolemeasures.correlations.txt",sep="")
write.table(cor.mat,cor.file,row.names=FALSE,col.names=FALSE)
print(cor.mat)


###############################################################################
# plot degrees vs. role measures
###############################################################################
cat("[",format(Sys.time(),"%a %d %b %Y %X"),"] Ploting role measures vs. degrees\n",sep="")
for(i in 1:ncol(degrees))
{	deg.name <- paste(degree.names[i],"-degree",sep="")
	
	for(j in 1:ncol(data))
	{	cat("[",format(Sys.time(),"%a %d %b %Y %X"),"] Ploting ",measure.names[j]," vs. ",deg.name,"\n",sep="")
		plot.file <- paste(folder.data,"degree-",degree.names[i],".vs.",measure.names[j],".pdf")
		pdf(file=plot.file, bg="white")
		plot(degrees[sampled,i],data[sampled,j],main=paste(measure.names[j],"vs.",deg.name),xlab=deg.name,ylab=measure.names[j],col="RED")
		dev.off()
	}
}
