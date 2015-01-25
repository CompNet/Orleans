# Generates plots regarding the distributions of some values
# over the whole data and over each cluster.
#
# version: 2
# Author: Vincent Labatut 06/2013, 01/2015
#  
# setwd("~/eclipse/workspaces/Networks/Orleans/")
# setwd("C:/Eclipse/workspaces/Networks/Orleans/")
# source("PostProcessing/process-distributions.R")
###############################################################################


###############################################################################
# Returns the standard filename for the histogram summarizing a given series.
#
# folder.data: folder containing all input and output files.
# family: name of the considered *group* of measures.
# name: name of the considered measure.
# clusters: TRUE to handle clusters, FALSE for communities (optional: only for partition-based distributions).
# c: number of the considered cluster/partition (optional).
###############################################################################
get.distrib.histo.filename <- function(folder.data, family, name, clusters=NA, c=NA)
{	result <- paste(folder.data,family,sep="")
	result <- paste(result,".",name,sep="")
	result <- paste(result,".histo",sep="")
	if(!is.na(clusters) && !is.na(c))
	{	if(clusters)
			result <- paste(result,".clstr=",c,sep="")
		else
			result <- paste(result,".cmnty=",c,sep="")
	}
	result <- paste(result,".pdf",sep="")
	return(result)	
}

###############################################################################
# Returns the standard filename for the cumulative distribution summarizing a given series.
#
# folder.data: folder containing all input and output files.
# family: name of the considered *group* of measures.
# name: name of the considered measure.
# clusters: TRUE to handle clusters, FALSE for communities (optional: only for partition-based distributions).
# c: number of the considered cluster/partition (optional).
###############################################################################
get.distrib.cumul.filename <- function(folder.data, family, name, loglog, clusters=NA, c=NA)
{	result <- paste(folder.data,family,sep="")
	result <- paste(result,".",name,sep="")
	result <- paste(result,".cumulative",sep="")
	if(loglog)
		result <- paste(result,"LL",sep="")
	if(!is.na(clusters) && !is.na(c))
	{	if(clusters)
			result <- paste(result,".clstr=",c,sep="")
		else
			result <- paste(result,".cmnty=",c,sep="")
	}
	result <- paste(result,".pdf",sep="")
	return(result)	
}

###############################################################################
# Returns the standard filename for the correlation matrix between the series.
#
# folder.data: folder containing all input and output files.
# family: name of the considered *group* of measures.
# clusters: TRUE to handle clusters, FALSE for communities (optional: only for partition-based distributions).
# c: number of the considered cluster/partition (optional).
###############################################################################
get.distrib.cor.filename <- function(folder.data, family, clusters=NA, c=NA)
{	result <- paste(folder.data,family,sep="")
	result <- paste(result,".cor",sep="")
	if(!is.na(clusters) && !is.na(c))
	{	if(clusters)
			result <- paste(result,".clstr=",c,sep="")
		else
			result <- paste(result,".cmnty=",c,sep="")
	}
	result <- paste(result,".txt",sep="")
	return(result)	
}

###############################################################################
# Plots the overall histogram and cumulative distribution for the specified
# values. Also processes the correlation for each pair of series.
#
# folder.data: folder containing all input and output files.
# family: name of the considered *group* of measures.
# names: names of the considered values.
# values: matrix whose columns correspond to series to be plotted.
# loglog: whether the cumulative distribution should be plot on a log-log scale.
###############################################################################
process.overall.distribution <- function(folder.data, family, names, values, loglog=FALSE)
{	start.time <- Sys.time();
		# process distributions
		for(i in 1:ncol(values))
		{	cat("[",format(start.time,"%a %d %b %Y %H:%M:%S"),"] Process ",names[i]," overall distribution\n",sep="")
			
			# histogram
			plot.file <- get.distrib.histo.filename(folder.data, family, names[i])
			cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] ..Plot ",names[i]," histogram in file ",plot.file,"\n",sep="")
			pdf(file=plot.file, bg="white")
			hist(values[,i],probability=TRUE,breaks=100,main=paste("Distribution of",names[i]),xlab=names[i],col="RED")
			dev.off()
			
			# (partial) cumulative distribution
			plot.file <- get.distrib.cumul.filename(folder.data, family, names[i], loglog)
			cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] ..Plot ",names[i]," cumulative distribution in file ",plot.file,"\n",sep="")
			pdf(file=plot.file, bg="white")
			if(loglog)
				ll <- "xy"
			else
				ll <- ""
			ecdflt(x=values[,i], xlab=names[i], main=paste("Complementary Cumulative Distribution of", names[i]), complementary=TRUE, col="RED", points=1000, log=ll)
			dev.off()
		}

		# process correlations
		cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] ..Process and record the correlations between the series\n",sep="")
		cor.mat <- cor(values)
		cor.file <- get.distrib.cor.filename(folder.data, family)
		print(cor.mat)
		write.table(cor.mat,cor.file,row.names=TRUE,col.names=TRUE)
	
	end.time <- Sys.time();
	total.time <- end.time - start.time;
	cat("[",format(end.time,"%a %d %b %Y %H:%M:%S"),"] Plotting completed in ",format(total.time),"\n",sep="")
}


###############################################################################
# Plots the part-related histogram and cumulative distribution for the specified
# values and partition (can be either clusters or communities).
#
# folder.data: folder containing all input and output files.
# membership: individual membership to the considered parts.
# clusters: TRUE when dealing with clusters, FALSE for communities.
# family: name of the considered *group* of measures.
# names: names of the considered values.
# values: matrix whose columns correspond to series to be plotted.
# loglog: whether the cumulative distribution should be plot on a log-log scale.
###############################################################################
process.partition.distribution <- function(folder.data, membership, clusters, family, names, values, loglog=FALSE)
{	start.time <- Sys.time();
		parts <- sort(unique(membership))
		for(c in 1:length(parts))
		{	part <- parts[c]
			cat("[",format(start.time,"%a %d %b %Y %H:%M:%S"),"] Process part ",part,"\n",sep="")
			idx <- which(membership==part)
			
			# process the distribution for each series
			for(i in 1:ncol(values))
			{	cat("[",format(start.time,"%a %d %b %Y %H:%M:%S"),"] ..Process ",names[i]," overall distribution\n",sep="")
				
				# histogram
				plot.file <- get.distrib.histo.filename(folder.data, family, names[i], clusters, c=part)
				cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] ....Plot ",names[i]," histogram in file ",plot.file,"\n",sep="")
				pdf(file=plot.file, bg="white")
				hist(values[idx,i],probability=TRUE,breaks=100,main=paste("Distribution of",names[i]),xlab=names[i],col="RED")
				dev.off()
				
				# (partial) cumulative distribution
				plot.file <- get.distrib.cumul.filename(folder.data, family, names[i], loglog, clusters, c=part)
				cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] ....Plot ",names[i]," cumulative distribution in file ",plot.file,"\n",sep="")
				pdf(file=plot.file, bg="white")
				if(loglog)
					ll <- "xy"
				else
					ll <- ""
				ecdflt(x=values[idx,i], xlab=names[i], main=paste("Complementary Cumulative Distribution of",names[i]), complementary=TRUE, col="RED", points=1000, log=ll)
				dev.off()
			}
			
			# process correlations between series
			cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] ..Process and record the correlations between the series\n",sep="")
			cor.mat <- cor(values)
			cor.file <- get.distrib.cor.filename(folder.data, family, clusters, c=part)
			print(cor.mat)
			write.table(cor.mat,cor.file,row.names=TRUE,col.names=TRUE)
		}
	end.time <- Sys.time();
	total.time <- end.time - start.time;
	cat("[",format(end.time,"%a %d %b %Y %H:%M:%S"),"] Plotting completed in ",format(total.time),"\n",sep="")
}
