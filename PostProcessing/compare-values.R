# Processes the correlation between two groups of series
# (each pair of series is considered). Also plots each
# series against one from the other group.
#
# version: 1
# Author: Vincent Labatut 01/2015
#  
# setwd("~/eclipse/workspaces/Networks/Orleans/")
# setwd("C:/Eclipse/workspaces/Networks/Orleans/")
# source("PostProcessing/compare-values.R")
###############################################################################


###############################################################################
# Returns the standard filename for the text file containing the correlation results.
#
# folder.data: folder containing all input and output files.
# family1: name of the first considered *group* of measures.
# name1: name of the first considered measure.
# family2: name of the second considered *group* of measures.
# name2: name of the second considered measure.
###############################################################################
get.correl.results.filename <- function(folder.data, family1, family2)
{	result <- paste(folder.data,family1,sep="")
	result <- paste(result,".vs",sep="")
	result <- paste(result,".",family2,sep="")
	result <- paste(result,".txt",sep="")
	return(result)	
}

###############################################################################
# Returns the standard filename for the plot comparing two series.
#
# folder.data: folder containing all input and output files.
# family1: name of the first considered *group* of measures.
# family2: name of the second considered *group* of measures.
###############################################################################
get.correl.vsplot.filename <- function(folder.data, family1, name1, family2, name2)
{	result <- paste(folder.data,family1,sep="")
	result <- paste(result,".",name1,sep="")
	result <- paste(result,".vs",sep="")
	result <- paste(result,".",family2,sep="")
	result <- paste(result,".",name2,sep="")
	result <- paste(result,".pdf",sep="")
	return(result)	
}

###############################################################################
# Plots the overall histogram and cumulative distribution for the specified
# values. Also processes the correlation for each pair of series.
#
# folder.data: folder containing all input and output files.
# family1: name of the first considered *group* of measures.
# names1: names of the first considered values.
# values1: matrix whose columns correspond to the first group of series to be plotted.
# family2: name of the second considered *group* of measures.
# names2: names of the second considered values.
# values2: matrix whose columns correspond to the second group of series to be plotted.
###############################################################################
process.values.comparison <- function(folder.data, family1, names1, values1, family2, names2, values2)
{	start.time <- Sys.time()
		# process correlation matrix
		cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] ..Process and record the correlations between the series\n",sep="")
		cor.mat <- cor(values1,values2)
		cor.file <- get.correl.results.filename(folder.data, family1, family2)
		print(cor.mat)
		write.table(cor.mat,cor.file,row.names=TRUE,col.names=TRUE)
	
		# process comparison plots
		for(i in 1:ncol(values1))
		{	cat("[",format(start.time,"%a %d %b %Y %H:%M:%S"),"] Process ",names1[i]," vs...\n",sep="")
			
			for(j in 1:ncol(values2))
			{	cat("[",format(start.time,"%a %d %b %Y %H:%M:%S"),"]   ...",names2[j],"\n",sep="")
				plot.file <- get.correl.vsplot.filename(folder.data, family1, names1[i], family2, names2[j])
				pdf(file=plot.file, bg="white")
				plot(values1[,i],values2[,j],main=paste(names1[i],"vs",names2[j]),xlab=names1[i],ylab=names2[j],col="RED")
				dev.off()
			}
		}
	end.time <- Sys.time()
	total.time <- end.time - start.time
	cat("[",format(end.time,"%a %d %b %Y %H:%M:%S"),"] Plotting completed in ",format(total.time),"\n",sep="")
}
