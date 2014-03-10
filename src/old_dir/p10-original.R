# Plot Amaral & Guimera's original measures.
#
# version: 1
# Author: Vincent Labatut 09/2013
#
# setwd("~/eclipse/workspaces/Networks/Orleans/")
# setwd("C:/Eclipse/workspaces/Networks/Orleans/")
# source("src/old_dir/p10-original.R")
###############################################################################
library("igraph")
source("src/extras/ecdflt.R")
k <- 6										# TODO we work only on the clusters found for this k
zoom <- 20									# TODO maximal z-score value to be plotted (or NA for no limit)

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
# identify roles
###############################################################################
# amaral's measures: within-community degree (z) and participation coefficient
start.time <- Sys.time();
cat("[",format(start.time,"%a %d %b %Y %H:%M:%S"),"] Identifying original node roles\n",sep="")
	roles <- matrix(NA,nrow=sample.size,ncol=2)
	for(i in 1:sample.size)
	{	if(i%%1000==0)
			cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] ..Processing node ",i,"/",sample.size,"\n",sep="")
		for(j in 1:2)
		{	# non-hubs
			if(data[sampled[i],j]<2.5)
			{	if(data[sampled[i],j+2]<0.05)
					roles[i,j] <- 1		# Ultra-Peripheral
				else if(data[sampled[i],j+2]<0.625)
					roles[i,j] <- 2		# Peripheral
				else if(data[sampled[i],j+2]<0.8)
					roles[i,j] <- 3		# Non-Hub Connector
				else
					roles[i,j] <- 4		# Non-Hub Kinless
			}
			# hubs
			else
			{	if(data[sampled[i],j+2]<0.3)
					roles[i,j] <- 5		# Provincial Hub
				else if(data[sampled[i],j+2]<0.75)
					roles[i,j] <- 6		# Connector Hub
				else
					roles[i,j] <- 7		# Kinless Hub
			}
		}
	}
end.time <- Sys.time();
total.time <- end.time - start.time;
cat("[",format(end.time,"%a %d %b %Y %H:%M:%S"),"] Identification completed in ",format(total.time),"\n",sep="")
	

###############################################################################
# plot role measure distributions using the original layout
###############################################################################
start.time <- Sys.time();
cat("[",format(start.time,"%a %d %b %Y %H:%M:%S"),"] Plot roles using original layout\n",sep="")
	dir.names <- c("out","in")
	for(i in 1:length(dir.names))
	{	# get extreme values for y axis
		min.y <- min(data[sampled,i])
		if(is.na(zoom))
		{	max.y <- max(data[sampled,i])
			plot.file <- paste(folder.data,"rolemeasures.original.",dir.names[i],".all.pdf",sep="")
		}
		else
		{	max.y <- 20
			plot.file <- paste(folder.data,"rolemeasures.original.",dir.names[i],".zoom.pdf",sep="")
		}
		
		# init plot
		margin <- 0.025
		pdf(file=plot.file, bg="white")
		plot(0,cex.lab=1.5,cex.axis=1.5,
				xlim=c(0-margin,1+margin), ylim=c(min.y-margin,max.y+margin),
				xlab="Participation Coefficient (P)",ylab="Within Community Degree (Z)",
				xaxs="i", yaxs="i",		# remove internal margins, since we already deal with them manually
				main="Community Roles"
		)
		
		# draw rectangles corresponding to roles
		rect(xleft=0-margin, ybottom=min.y-margin, xright=0.0500,   ytop=2.50,			col="#7f7f7f", border=NA)		# dark grey
		rect(xleft=0.0500,   ybottom=min.y-margin, xright=0.6250,   ytop=2.50,			col="#fe8081", border=NA)		# red
		rect(xleft=0.6250,   ybottom=min.y-margin, xright=0.8000,   ytop=2.50,			col="#80fe80", border=NA)		# green
		rect(xleft=0.8000,   ybottom=min.y-margin, xright=1+margin, ytop=2.50,			col="#8989f5", border=NA)		# blue
		rect(xleft=0-margin, ybottom=2.50,		   xright=0.3000,   ytop=max.y+margin,	col="#feff80", border=NA)		# yellow
		rect(xleft=0.3000,   ybottom=2.50,		   xright=0.7500,   ytop=max.y+margin,	col="#ddc5c5", border=NA)		# purple-ish
		rect(xleft=0.7500,   ybottom=2.50,		   xright=1+margin, ytop=max.y+margin,	col="#f8f8f8", border=NA)		# light grey
		
		# plot nodes
		role.colors <- c("darkgrey","red","green","blue","yellow","magenta","grey")
		for(r in 1:length(role.colors))
		{	color <- role.colors[r]
			idx <- which(roles[,i]==r)
			points(data[sampled[idx],i+2],data[sampled[idx],i],pch=21,bg=color,cex=2)
		}
		
		# add legends
		text(x= 0.000, y=min.y,				labels="Ultra-peripheral Non-hubs",	cex=1.25,adj=c(0.00,NA),srt=90)
		text(x= 0.330, y=min.y+1.4*margin,	labels="Peripheral Non-hubs",		cex=1.25,adj=c(0.50,NA))
		text(x= 0.660, y=min.y,				labels="Connnector Non-hubs",		cex=1.25,adj=c(0.00,NA),srt=90)
		text(x= 0.990, y=min.y,				labels="Kinless Non-hubs",			cex=1.25,adj=c(0.00,NA),srt=90)
		text(x= 0.125, y=max.y-2*margin,	labels="Provincial Hubs",			cex=1.25,adj=c(0.50,NA))
		text(x= 0.525, y=max.y-2*margin,	labels="Connector Hubs",			cex=1.25,adj=c(0.50,NA))
		text(x= 0.885, y=max.y-2*margin,	labels="Kinless Hubs",				cex=1.25,adj=c(0.50,NA))
		
		# draw black frame
		rect(xleft=0-margin, ybottom=min.y-margin, xright=1+margin, ytop=max.y+margin,				   border="black")	# border
		
		dev.off()
	}
end.time <- Sys.time();
total.time <- end.time - start.time;
cat("[",format(end.time,"%a %d %b %Y %H:%M:%S"),"] Plotting completed in ",format(total.time),"\n",sep="")


###############################################################################
# load membership vector
###############################################################################
start.time <- Sys.time();
cat("[",format(start.time,"%a %d %b %Y %H:%M:%S"),"] Loading membership vector\n",sep="")
membership.file <- paste(folder.data,"cluster.k",k,".txt",sep="")
membership <- as.matrix(read.table(membership.file))[,2] + 1	# the k-means implementation starts numbering clusters from 0
end.time <- Sys.time();
total.time <- end.time - start.time;
cat("[",format(end.time,"%a %d %b %Y %H:%M:%S"),"] Load completed in ",format(total.time),"\n",sep="")


###############################################################################
# plot estimated clusters using the original layout
###############################################################################
start.time <- Sys.time();
cat("[",format(start.time,"%a %d %b %Y %H:%M:%S"),"] Plot roles using original layout\n",sep="")
	dir.names <- c("out","in")
	for(i in 1:length(dir.names))
	{	# get extreme values for y axis
		min.y <- min(data[sampled,i])
		if(is.na(zoom))
		{	max.y <- max(data[sampled,i])
			plot.file <- paste(folder.data,"clusters.original.",dir.names[i],".all.pdf",sep="")
		}
		else
		{	max.y <- 20
			plot.file <- paste(folder.data,"clusters.original.",dir.names[i],".zoom.pdf",sep="")
		}
		
		# init plot
		margin <- 0.025
		pdf(file=plot.file, bg="white")
		plot(0,cex.lab=1.5,cex.axis=1.5,
				xlim=c(0-margin,1+margin), ylim=c(min.y-margin,max.y+margin),
				xlab="Participation Coefficient (P)",ylab="Within Community Degree (Z)",
				xaxs="i", yaxs="i",		# remove internal margins, since we already deal with them manually
				main="Community Roles"
		)
		
		# draw rectangles corresponding to roles
		rect(xleft=0-margin, ybottom=min.y-margin, xright=0.0500,   ytop=2.50,			col="#7f7f7f", border=NA)		# dark grey
		rect(xleft=0.0500,   ybottom=min.y-margin, xright=0.6250,   ytop=2.50,			col="#fe8081", border=NA)		# red
		rect(xleft=0.6250,   ybottom=min.y-margin, xright=0.8000,   ytop=2.50,			col="#80fe80", border=NA)		# green
		rect(xleft=0.8000,   ybottom=min.y-margin, xright=1+margin, ytop=2.50,			col="#8989f5", border=NA)		# blue
		rect(xleft=0-margin, ybottom=2.50,		   xright=0.3000,   ytop=max.y+margin,	col="#feff80", border=NA)		# yellow
		rect(xleft=0.3000,   ybottom=2.50,		   xright=0.7500,   ytop=max.y+margin,	col="#ddc5c5", border=NA)		# purple-ish
		rect(xleft=0.7500,   ybottom=2.50,		   xright=1+margin, ytop=max.y+margin,	col="#f8f8f8", border=NA)		# light grey
		
		# plot nodes
		for(c in 1:k)
		{	idx <- which(membership==c)
			points(data[sampled[idx],i+2],data[sampled[idx],i],pch=21,bg=c,cex=2)
		}
		
		# add legends
		text(x= 0.000, y=min.y,				labels="Ultra-peripheral Non-hubs",	cex=1.25,adj=c(0.00,NA),srt=90)
		text(x= 0.330, y=min.y+1.4*margin,	labels="Peripheral Non-hubs",		cex=1.25,adj=c(0.50,NA))
		text(x= 0.660, y=min.y,				labels="Connnector Non-hubs",		cex=1.25,adj=c(0.00,NA),srt=90)
		text(x= 0.990, y=min.y,				labels="Kinless Non-hubs",			cex=1.25,adj=c(0.00,NA),srt=90)
		text(x= 0.125, y=max.y-2*margin,	labels="Provincial Hubs",			cex=1.25,adj=c(0.50,NA))
		text(x= 0.525, y=max.y-2*margin,	labels="Connector Hubs",			cex=1.25,adj=c(0.50,NA))
		text(x= 0.885, y=max.y-2*margin,	labels="Kinless Hubs",				cex=1.25,adj=c(0.50,NA))
		
		# draw black frame
		rect(xleft=0-margin, ybottom=min.y-margin, xright=1+margin, ytop=max.y+margin,				   border="black")	# border
		
		dev.off()
	}
end.time <- Sys.time();
total.time <- end.time - start.time;
cat("[",format(end.time,"%a %d %b %Y %H:%M:%S"),"] Plotting completed in ",format(total.time),"\n",sep="")
