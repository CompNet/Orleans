# Plot Amaral & Guimera's original measures.
#
# version: 1
# Author: Vincent Labatut 06/2013
#
# setwd("~/eclipse/workspaces/Networks/Orleans/")
# setwd("C:/Eclipse/workspaces/Networks/Orleans/")
# source("src/old_undir/p10-original.R")
###############################################################################
library("igraph")
source("src/ecdflt.R")


###############################################################################
# setup files
###############################################################################
folder.data <- "data/"	
file.input <- "rolemeasures.raw.txt"
rolemeas.names <- c("zscore", "participation")
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
		idx.inf <- which(is.infinite(data[,c]))# | data[,c]==1.79769e+308)
		if(length(idx.inf)>0)
		{	data[idx.inf,c] <- 0
			cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] ....Replacing ",length(idx.inf)," infinite values by 0 in col.",c,")\n",sep="")
		}
	}
end.time <- Sys.time();
total.time <- end.time - start.time;
cat("[",format(end.time,"%a %d %b %Y %H:%M:%S"),"] Cleaning completed in ",total.time,"\n",sep="")


###############################################################################
# sample a few objects
###############################################################################
cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] Sample ",sample.size," objects\n",sep="")
sampled <- sample(x=1:nrow(data),size=sample.size)


###############################################################################
# identify roles
###############################################################################
# amaral's measures: within-community degree (z) and participation coefficient
start.time <- Sys.time();
cat("[",format(start.time,"%a %d %b %Y %H:%M:%S"),"] Identifying original node roles\n",sep="")
	roles <- rep(NA,sample.size)
	for(i in 1:sample.size)
	{	if(i%%1000==0)
			cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] ..Processing node ",i,"/",sample.size,"\n",sep="")
		
		# non-hubs
		if(amaral_z[i]<2.5)
		{	if(data[sampled[i],1]<0.05)
				roles[i] <- 1	# Ultra-Peripheral
			else if(data[sampled[i],1]<0.625)
				roles[i] <- 2	# Peripheral
			else if(data[sampled[i],1]<0.8)
				roles[i] <- 3	# Non-Hub Connector
			else
				roles[i] <- 4	# Non-Hub Kinless
		}
		# hubs
		else
		{	if(data[sampled[i],2]<0.3)
				roles[i] <- 5	# Provincial Hub
			else if(data[sampled[i],2]<0.75)
				roles[i] <- 6	# Connector Hub
			else
				roles[i] <- 7	# Kinless Hub
		}
	}
end.time <- Sys.time();
total.time <- end.time - start.time;
cat("[",format(end.time,"%a %d %b %Y %H:%M:%S"),"] Identification completed in ",total.time,"\n",sep="")
	

###############################################################################
# plot role measure distributions using the original layout
###############################################################################
start.time <- Sys.time();
cat("[",format(start.time,"%a %d %b %Y %H:%M:%S"),"] Plot roles using original layout\n",sep="")
	# init plot
	plot.file <- paste(folder.data,"rolemeasures.original.pdf",sep="")
	pdf(file=plot.file, bg="white")
	plot(data[sampled,2],data[sampled,1],pch=21,bg="red",cex=2,cex.lab=1.5,cex.axis=1.5,
			xlab="Participation Coefficient",ylab="Within Community Degree")
	
	# draw rectangles corresponding to roles
	rect(xleft=-0.25,ybottom=-1.2,xright=0.05,ytop=2.5,col="#7f7f7f",border=NA)
	rect(xleft=0.05,ybottom=-1.2,xright=0.625,ytop=2.5,col="#fe8081",border=NA)
	rect(xleft=0.625,ybottom=-1.2,xright=0.8,ytop=2.5,col="#80fe80",border=NA)
	rect(xleft=-0.25,ybottom=2.5,xright=0.3,ytop=4,col="#feff80",border=NA)
	rect(xleft=0.3,ybottom=2.5,xright=0.75,ytop=4,col="#ddc5c5",border=NA)
	rect(xleft=-0.0275,ybottom=-1.18,xright=0.6925,ytop=3.74,border="black")
	
	# plot nodes
	role.colors <- c("darkgrey","red","green","blue","yellow","magenta","grey")
	for(r in 1:length(roles))
	{	color <- role.colors[r]
		idx <- which(roles==role)
		points(data[sampled[idx],2],data[sampled[idx],1],pch=21,bg=color,cex=2)
	}
	
	# add legends
	text(x=0.15,y=3.5,labels="Provincial Hubs",cex=1.25)
	text(x=0.5,y=3.5,labels="Connector Hubs",cex=1.25)
	text(x=0.025,y=0.75,labels="Ultra-peripheral Non-hubs",srt=90,cex=1.25)
	text(x=0.325,y=-1,labels="Peripheral Non-hubs",cex=1.25)
	text(x=0.66,y=0.75,labels="Connnector Non-hubs",srt=90,cex=1.25)
	
	dev.off()
end.time <- Sys.time();
total.time <- end.time - start.time;
cat("[",format(end.time,"%a %d %b %Y %H:%M:%S"),"] Plotting completed in ",total.time,"\n",sep="")
