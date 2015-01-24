# Plot Amaral & Guimera's original measures.
#
# version: 1
# Author: Vincent Labatut 09/2013
#
# setwd("~/eclipse/workspaces/Networks/Orleans/")
# setwd("C:/Eclipse/workspaces/Networks/Orleans/")
# source("PostProcessing/cluster-original.R")
###############################################################################


###############################################################################
# Returns the standard filename for the original plot PDF file.
#
# folder.data: folder containing all input and output files.
# role.meas: type of role measures.
# n.clust: number of detected clusters (optional).
# clust.algo: cluster analysis algorithm.
# comdet.algo: community detection algorithm.
# sample.size: size of the considered data sample.
# zoom: whether the plot is full or zoomed in.
# dir: for the directed version: in or out.
# clusters: whether the regular (FALSE) or cluster (TRUE) colors are used.
###############################################################################
get.original.plot.filename <- function(folder.data, role.meas, clust.algo, comdet.algo, n.clust=0, sample.size, zoom, dir=NA, clusters)
{	result <- paste(folder.data,"comdet=",comdet.algo,sep="")
	result <- paste(result,".rolemeas=",role.meas,sep="")
	result <- paste(result,".clust=",clust.algo,sep="")
	if(n.clust>0)
		result <- paste(result,".k=",n.clust,sep="")
	if(!is.na(sample.size))
		result <- paste(result,".smplsz=",sample.size,sep="")
	if(!is.na(sample.size))
		result <- paste(result,".zoom",sample.size,sep="")
	result <- paste(result,".original",sep="")
	if(clusters)
		result <- paste(result,"-clust",sep="")
	if(!is.na(dir))
		result <- paste(result,".dir=",dir,sep="")
	result <- paste(result,".pdf",sep="")
	return(result)
}

###############################################################################
# Draws the original Guimera & Amaral plots, with the same colors. You can use
# different threshold values, though. Also, another plot is generated, using
# the detected clusters as colors (instead of the classic role-based ones).
#
# folder.data: folder containing all input and output files.
# role.meas: type of role measures.
# clust.algo: cluster analysis method.
# comdet.algo: community detection algorithm.
# sample.size: if the PCA takes too long, you can work on a sample of the data.
#			   this parameter specifies the size of this sample (NA to use the
#			   whole dataset).
# z.threshold: value used for the z measure (default: G&A original value).
# P.threshold: values used for the P measure (default: G&A original values).
###############################################################################
draw.original.plots <- function(folder.data, role.meas, clust.algo, comdet.algo, sample.size=NA, z.threshold=2.5, P.thresholds=c(0.05,0.625,0.8,0.3,0.75))
{	zoom <- NA # force a zoom on a part of the plot
	rolemeas.names <- get.rolemeas.names(role.meas)
	
	# we can plot these only for the Guimera-Amaral measures, or the directed variants we proposed.
	if(role.meas!='GA' && role.meas!='GA-dir')
		cat("[",format(start.time,"%a %d %b %Y %H:%M:%S"),"] No original plot for the selected role measures\n",sep="")
	
	else
	{	# load membership vector
		start.time <- Sys.time();
		cat("[",format(start.time,"%a %d %b %Y %H:%M:%S"),"] Loading membership vector\n",sep="")
			membership.file <- get.cluster.filename(folder.data, role.meas, clust.algo, comdet.algo)
			membership <- as.vector(as.matrix(read.table(membership.file)))
		end.time <- Sys.time();
		total.time <- end.time - start.time;
		cat("[",format(end.time,"%a %d %b %Y %H:%M:%S"),"] Load completed in ",format(total.time),"\n",sep="")
		
		# load raw data
		start.time <- Sys.time();
		cat("[",format(start.time,"%a %d %b %Y %H:%M:%S"),"] Loading raw data\n",sep="")
			file.data <- get.rolemeas.filename(folder.data, role.meas, norm=FALSE, comdet.algo)
			data <- as.matrix(read.table(file.data,header=TRUE))
		end.time <- Sys.time();
		total.time <- end.time - start.time;
		cat("[",format(end.time,"%a %d %b %Y %H:%M:%S"),"] Load completed in ",format(total.time),"\n",sep="")
		
		# possibly sample a few objects
		if(is.na(sample.size))
		{	cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] No sampling (processing the whole dataset)\n",sep="")
			sampled <- 1:nrow(data)
		}
		else
		{	cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] Sample ",sample.size," objects\n",sep="")
			sampled <- sample(x=1:nrow(data),size=sample.size)
		}
		
		# GA undirected
		if(role.meas=='GA')
		{	roles <- identify.roles(data,sampled,z.threshold,P.thresholds)
			plot.file <- get.original.plot.filename(folder.data, role.meas, clust.algo, comdet.algo, n.clust=0, sample.size, zoom, dir=NA, clusters=FALSE)
			plot.original.regular(data,sampled,roles,zoom,plot.file,z.threshold,P.thresholds)
			plot.file <- get.original.plot.filename(folder.data, role.meas, clust.algo, comdet.algo, n.clust=0, sample.size, zoom, dir=NA, clusters=TRUE)
			plot.original.clusters(data,sampled,membership,zoom,plot.file,z.threshold,P.thresholds)
		}
		
		# GA directed
		else if(role.meas=='GA-dir')
		{	roles.in <- identify.roles(data[,c(1,3)],sampled,z.threshold,P.thresholds)
			plot.file.in <- get.original.plot.filename(folder.data, role.meas, clust.algo, comdet.algo, n.clust=0, sample.size, zoom, dir="in", clusters=FALSE)
			plot.original.regular(data[,c(1,3)],sampled,roles.in,zoom,plot.file.in,z.threshold,P.thresholds)
			plot.file.in <- get.original.plot.filename(folder.data, role.meas, clust.algo, comdet.algo, n.clust=0, sample.size, zoom, dir="in", clusters=TRUE)
			plot.original.clusters(data[,c(1,3)],sampled,membership,zoom,plot.file.in,z.threshold,P.thresholds)
			
			roles.out <- identify.roles(data[,c(2,4)],sampled,z.threshold,P.thresholds)
			plot.file.out <- get.original.plot.filename(folder.data, role.meas, clust.algo, comdet.algo, n.clust=0, sample.size, zoom, dir="out", clusters=FALSE)
			plot.original.regular(data[,c(2,4)],sampled,roles.out,zoom,plot.file,z.threshold,P.thresholds)
			plot.file.out <- get.original.plot.filename(folder.data, role.meas, clust.algo, comdet.algo, n.clust=0, sample.size, zoom, dir="out", clusters=TRUE)
			plot.original.clusters(data[,c(2,4)],sampled,membership,zoom,plot.file,z.threshold,P.thresholds)
		}
	}
}

###############################################################################
# Processes the role codes associated to the specified value, according to
# the specified thresholds.
#
# data: the role measure values.
# sampled: id of the sampled values.
# z.threshold: value used for the z measure (default: G&A original value).
# P.threshold: values used for the P measure (default: G&A original values).
###############################################################################
identify.roles <- function(data, sampled, z.threshold, P.thresholds)
{	start.time <- Sys.time();
	cat("[",format(start.time,"%a %d %b %Y %H:%M:%S"),"] Identifying node roles\n",sep="")
		sample.size <- length(sampled)
		roles <- sapply(1:sample.size, function(i)
			{	if(i%%1000==0)
					cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] ..Processing node ",i,"/",sample.size,"\n",sep="")
				
				# non-hubs
				if(data[sampled[i],2]<z.threshold)
				{	if(data[sampled[i],1]<P.thresholds[1])
						result <- 1	# Ultra-Peripheral
					else if(data[sampled[i],1]<P.thresholds[2])
						result <- 2	# Peripheral
					else if(data[sampled[i],1]<P.thresholds[3])
						result <- 3	# Non-Hub Connector
					else
						result <- 4	# Non-Hub Kinless
				}
				# hubs
				else
				{	if(data[sampled[i],1]<P.thresholds[4])
						result <- 5	# Provincial Hub
					else if(data[sampled[i],1]<P.thresholds[5])
						result <- 6	# Connector Hub
					else
						result <- 7	# Kinless Hub
				}
				
				return(result)
			})
	end.time <- Sys.time();
	total.time <- end.time - start.time;
	cat("[",format(end.time,"%a %d %b %Y %H:%M:%S"),"] Identification completed in ",format(total.time),"\n",sep="")
	
	return(roles)
}

###############################################################################
# Plots the role measure distributions using the original G&A layout.
#
# data: the role measure values.
# sampled: id of the sampled values.
# roles: previously processed role codes.
# zoom: whether the plot should be zoomed-in.
# plot.file: name of the generated PDF.
# z.threshold: value used for the z measure (default: G&A original value).
# P.threshold: values used for the P measure (default: G&A original values).
###############################################################################
plot.original.regular <- function(data, sampled, roles, zoom, plot.file, z.threshold, P.thresholds)
{	start.time <- Sys.time();
	cat("[",format(start.time,"%a %d %b %Y %H:%M:%S"),"] Plot roles using original layout\n",sep="")
		# get extreme values for y axis
		min.y <- min(data[sampled,2])
		if(is.na(zoom))
			max.y <- max(data[sampled,2])
		else
			max.y <- 20	# zoo
		
		# init plot
		margin.x <- 0.025
		margin.y <- (max.y-min.y)/40
		pdf(file=plot.file, bg="white")
		plot(0,cex.lab=1.5,cex.axis=1.5,
			xlim=c(0-margin.x,1+margin.x), ylim=c(min.y-margin.y,max.y+margin.y),
			xlab="Participation Coefficient (P)",ylab="Within Community Degree (Z)",
			xaxs="i", yaxs="i",		# remove internal margins, since we already deal with them manually
			main="Community Roles"
		)
		
		# draw rectangles corresponding to roles
		rect(xleft=0-margin.x, 		ybottom=min.y-margin.y, xright=P.thresholds[1],	ytop=z.threshold,	 col="#7f7f7f", border=NA)		# dark grey
		rect(xleft=P.thresholds[1],	ybottom=min.y-margin.y,	xright=P.thresholds[2], ytop=z.threshold,	 col="#fe8081", border=NA)		# red
		rect(xleft=P.thresholds[2],	ybottom=min.y-margin.y,	xright=P.thresholds[3], ytop=z.threshold,	 col="#80fe80", border=NA)		# green
		rect(xleft=P.thresholds[3],	ybottom=min.y-margin.y,	xright=1+margin.x, 		ytop=z.threshold,	 col="#8989f5", border=NA)		# blue
		rect(xleft=0-margin.x,		ybottom=z.threshold, 	xright=P.thresholds[4],	ytop=max.y+margin.y, col="#feff80", border=NA)		# yellow
		rect(xleft=P.thresholds[4],	ybottom=z.threshold,  	xright=P.thresholds[5],	ytop=max.y+margin.y, col="#ddc5c5", border=NA)		# purple-ish
		rect(xleft=P.thresholds[5],	ybottom=z.threshold,  	xright=1+margin.x, 		ytop=max.y+margin.y, col="#f8f8f8", border=NA)		# light grey
		
		# plot nodes
		role.colors <- c("darkgrey","red","green","blue","yellow","magenta","grey")
		for(r in 1:length(role.colors))
		{	color <- role.colors[r]
			idx <- which(roles==r)
			points(data[sampled[idx],1],data[sampled[idx],2],pch=21,bg=color,cex=1)
		}
		
		# add legends
		text(x= 0.000, y=min.y,					labels="Ultra-peripheral Non-hubs",	cex=1.25, adj=c(0.00,NA),srt=90)
		text(x= (P.thresholds[1]+P.thresholds[2])/2, y=min.y+0.5*margin.y,		labels="Peripheral Non-hubs",		cex=1.25, adj=c(0.50,NA))
		text(x= P.thresholds[2]+margin.x, y=min.y,					labels="Connnector Non-hubs",		cex=1.25, adj=c(0.00,NA),srt=90)
		text(x= 1-0.25*margin.x, y=min.y,					labels="Kinless Non-hubs",			cex=1.25, adj=c(0.00,NA),srt=90)
		text(x= P.thresholds[4]/2, y=max.y-0.5*margin.y,		labels="Provincial Hubs",			cex=1.25, adj=c(0.50,NA))
		text(x= (P.thresholds[4]+P.thresholds[5])/2, y=max.y-0.5*margin.y,		labels="Connector Hubs",			cex=1.25, adj=c(0.50,NA))
		text(x= (P.thresholds[5]+1)/2, y=max.y-0.5*margin.y,		labels="Kinless Hubs",				cex=1.25, adj=c(0.50,NA))
	
		# draw black frame
		rect(xleft=0-margin.x, ybottom=min.y-margin.y, xright=1+margin.x, ytop=max.y+margin.y, border="black")	# border
		
		dev.off()
	end.time <- Sys.time();
	total.time <- end.time - start.time;
	cat("[",format(end.time,"%a %d %b %Y %H:%M:%S"),"] Plotting completed in ",format(total.time),"\n",sep="")
}



###############################################################################
# Plots the estimated clusters using the original layout.
#
# data: the role measure values.
# sampled: id of the sampled values.
# membership: cluster membership.
# zoom: whether the plot should be zoomed-in.
# plot.file: name of the generated PDF.
# z.threshold: value used for the z measure (default: G&A original value).
# P.threshold: values used for the P measure (default: G&A original values).
###############################################################################
plot.original.clusters <- function(data, sampled, membership, zoom, plot.file, z.threshold, P.thresholds)
{	start.time <- Sys.time();
	cat("[",format(start.time,"%a %d %b %Y %H:%M:%S"),"] Plot roles using original layout\n",sep="")
		k <- length(unique(membership))
		# get extreme values for y axis
		min.y <- min(data[sampled,2])
		if(is.na(zoom))
			max.y <- max(data[sampled,2])
		else
			max.y <- 20	# zoom
		
		# init plot
		margin.x <- 0.025
		margin.y <- (max.y-min.y)/40
		pdf(file=plot.file, bg="white")
		plot(0,cex.lab=1.5,cex.axis=1.5,
			xlim=c(0-margin.x,1+margin.x), ylim=c(min.y-margin.y,max.y+margin.y),
			xlab="Participation Coefficient (P)",ylab="Within Community Degree (Z)",
			xaxs="i", yaxs="i",		# remove internal margins, since we already deal with them manually
			main="Community Roles"
		)
		
		# draw rectangles corresponding to roles
		rect(xleft=0-margin.x, 		ybottom=min.y-margin.y, xright=P.thresholds[1],	ytop=z.threshold,	 col="#7f7f7f", border=NA)		# dark grey
		rect(xleft=P.thresholds[1],	ybottom=min.y-margin.y, xright=P.thresholds[2],	ytop=z.threshold,	 col="#fe8081", border=NA)		# red
		rect(xleft=P.thresholds[2],	ybottom=min.y-margin.y, xright=P.thresholds[3],	ytop=z.threshold,	 col="#80fe80", border=NA)		# green
		rect(xleft=P.thresholds[3],	ybottom=min.y-margin.y, xright=1+margin.x, 		ytop=z.threshold,	 col="#8989f5", border=NA)		# blue
		rect(xleft=0-margin.x, 		ybottom=z.threshold,  	xright=P.thresholds[4],	ytop=max.y+margin.y, col="#feff80", border=NA)		# yellow
		rect(xleft=P.thresholds[4],	ybottom=z.threshold,  	xright=P.thresholds[5],	ytop=max.y+margin.y, col="#ddc5c5", border=NA)		# purple-ish
		rect(xleft=P.thresholds[5],	ybottom=z.threshold,  	xright=1+margin.x, 		ytop=max.y+margin.y, col="#f8f8f8", border=NA)		# light grey
		
		# plot nodes
		for(c in 1:k)
		{	idx <- which(membership==(c-1))
			points(data[sampled[idx],1],data[sampled[idx],2],pch=21,bg=c,cex=2)
		}
		
		# add legends
		text(x= 0.000, y=min.y,				 labels="Ultra-peripheral Non-hubs", cex=1.25,adj=c(0.00,NA),srt=90)
		text(x= 0.330, y=min.y+1*margin.y,	 labels="Peripheral Non-hubs",		 cex=1.25,adj=c(0.50,NA))
		text(x= 0.660, y=min.y,				 labels="Connnector Non-hubs",		 cex=1.25,adj=c(0.00,NA),srt=90)
		text(x= 0.990, y=min.y,				 labels="Kinless Non-hubs",			 cex=1.25,adj=c(0.00,NA),srt=90)
		text(x= 0.125, y=max.y-1*margin.y,	 labels="Provincial Hubs",			 cex=1.25,adj=c(0.50,NA))
		text(x= 0.525, y=max.y-1*margin.y,	 labels="Connector Hubs",			 cex=1.25,adj=c(0.50,NA))
		text(x= 0.885, y=max.y-1*margin.y,	 labels="Kinless Hubs",				 cex=1.25,adj=c(0.50,NA))
		
		# draw black frame
		rect(xleft=0-margin.x, ybottom=min.y-margin.y, xright=1+margin.x, ytop=max.y+margin.y, border="black")	# border
		
		dev.off()
	end.time <- Sys.time();
	total.time <- end.time - start.time;
	cat("[",format(end.time,"%a %d %b %Y %H:%M:%S"),"] Plotting completed in ",format(total.time),"\n",sep="")
}
