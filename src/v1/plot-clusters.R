# Plot clusters on screen and record the plot as a PDF file.
# v1.1
# 
# Author: Vincent Labatut 06/2013
# source("C:/Eclipse/workspaces/Networks/Orleans/src/plot-clusters.R")
###############################################################################

###############################################################################
# Receives raw data (table of instances), performs a PCA and plot the
# resulting two main components. The plot can also be recorded as a PDF
# file, depending on the 'file' parameter.
#
# data: 
#	Data to be plotted, as a two column table. usually the result of a PCA.
# membership:
#	Number of the cluster of each instance, in the same order.
# file.plot:
#	If a filename is specified, it is used to record a PDF representation
#	of the plot.
###############################################################################
plot.clusters <- function(data, membership, file.plot=NULL)
{	start.time <- Sys.time();
	cat("[",format(start.time,"%a %d %b %Y %H:%M:%S"),"] Plotting data and clusters\n",sep="")

	# title
	title <- "Clusters"
	if(!is.null(file.plot))
	{	name <- basename(file.plot)
		name <- substr(x=name,start=1,stop=nchar(name)-nchar(".pdf"))
		title <- paste(name," ",title,sep="")
	}
	
	# plot them in 2D
	cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] Plot a 2D representation of the instance distribution...\n",sep="")
	dev.new(width=4, height=4)
	plot(data[,1], data[,2],col=membership,xlab="First Principal Component",ylab="Second Principal Component",main=title)

	# possibly record the plot
	if(!is.null(file.plot))
	{	cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] Record the 2D representation of the instance distribution...\n",sep="")
		pdf(file=file.plot, bg="white")
		plot(data[,1], data[,2],col=membership,xlab="First Principal Component",ylab="Second Principal Component",main=title)
		dev.off()
	}

	end.time <- Sys.time();
	total.time <- end.time - start.time;
	cat("[",format(end.time,"%a %d %b %Y %H:%M:%S"),"] plotting completed in ",total.time,"\n",sep="")
}

###############################################################################
# test
###############################################################################
