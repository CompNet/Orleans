# Generates hive plots of the network.
# 
# Note: sampling the graph takes too long in R.
# So the sampling part was implemented in Java, in
# the ApplySampling class. So, you need to first
# generate the sample.txt file, containing the id
# of the nodes to be sampled, then apply the Java
# program, which will generate a new (much smaller)
# network file, then finally this script to generate
# the hive plots. Not very straightforward, I know.
#
# Note: the message " Error in calcCurveGrob(x, x$debug) : End points must not be identical"
# sometimes shows up when drawing the hive plots. It comes
# from the method drawing the links as splines, and means
# two nodes have the exact same position, which cannot be
# handled by this method.
# 
# version: 1
# Author: Vincent Labatut 09/2013
#
# setwd("~/eclipse/workspaces/Networks/Orleans/")
# setwd("C:/Eclipse/workspaces/Networks/Orleans/")
# source("src/p11-hive.R")
###############################################################################
library("HiveR")
library("grid")


###############################################################################
# setup files
###############################################################################
folder.data <- "data/"	
k <- 6											# TODO we work only on the clusters found for this k
file.input.degrees <- "degrees.txt"				# TODO you can possibly change that
file.input.soccap <- "soccapmeasures.txt"		# TODO you can possibly change that
file.input.rolemeas <- "rolemeasures.raw.txt"	# TODO you can possibly change that
file.input.network <- "links-anon.txt"			# TODO you can possibly change that
file.sample <- "sample.txt"						# TODO you can possibly change that
rolemeas.names <- c(							# TODO you might change that, if necessary
		"intensity-int-out","intensity-int-in","diversity-out","diversity-in","intensity-ext-out","intensity-ext-in","homogeneity-out","homogeneity-in")
soccap.names <- c(								# TODO you might change that, if necessary
		"ratio", "overlap")
sample.size <- 1000000							# TODO processing the whole dataset is to long, so the power-law distribution is tested only on a sample


###############################################################################
# sample a few objects
###############################################################################
cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] Process sample of size ",sample.size,"\n",sep="")
	file.sample <- paste(folder.data,file.sample,sep="")
	# if already processed, just used the file
	if(file.exists(file.sample))
	{	cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] Load sample file\n",sep="")
		sampled <- sort(as.matrix(read.table(file.sample)))
		cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] Sample size: ",length(sampled),"\n",sep="")
		cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] Min sample value: ",min(sampled),"\n",sep="")
	}else
	# otherwise, sample needs to be processed
	{	# first we load the out-degree
		cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] Load degrees\n",sep="")
		file.degrees <- paste(folder.data,file.input.degrees,sep="")
		degrees <- as.matrix(read.table(file.degrees))
		# then we sample amongst node with non-zero out degree (otherwise it's useless for a hiveplot)
		cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] Sample ",sample.size," objects\n",sep="")
		idx <- which(degrees[,1]>0 | degrees[,2]>0)
		sampled <- sort(sample(x=idx,size=sample.size))
		write.table(sampled,file.sample,row.names=FALSE,col.names=FALSE)
		cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] Min sample value: ",min(sampled),"\n",sep="")
	}


###############################################################################
# read the graph retain only sampled links
###############################################################################
# NOTE: We eventually did that part in Java, since the graph is to big
#		So we use the sample.txt file to perform the sampling and generate a subgraph.
#start.time <- Sys.time();
#cat("[",format(start.time,"%a %d %b %Y %H:%M:%S"),"] Load graph\n",sep="")
#	file.network <- paste(folder.data,file.input.network,sep="")
#	links <- as.matrix(read.table(file.network))
#	if(min(links)==0) links <- links + 1
#end.time <- Sys.time();
#total.time <- end.time - start.time;
#cat("[",format(end.time,"%a %d %b %Y %H:%M:%S"),"] Loading completed in ",total.time,"\n",sep="")
	
#start.time <- Sys.time();
#cat("[",format(start.time,"%a %d %b %Y %H:%M:%S"),"] Sample graph\n",sep="")
#	#idx1 <- match(sampled,links[,1])
#	#idx1 <- idx1[!is.na(idx1)]
#	idx1 <- which(links[,1] %in% sampled)
#	#idx2 <- match(sampled,links[,2])
#	#idx2 <- idx2[!is.na(idx2)]
#	idx2 <- which(links[,2] %in% sampled)
#	idx <- intersect(idx1, idx2)
#	links <- links[idx,]
#	idx1 <- NULL; idx2 <- NULL; idx <- NULL; gc()
#end.time <- Sys.time();
#total.time <- end.time - start.time;
#cat("[",format(end.time,"%a %d %b %Y %H:%M:%S"),"] Process completed in ",total.time,"\n",sep="")
	
	
###############################################################################
# directly read the sampled graph
###############################################################################
start.time <- Sys.time();
cat("[",format(start.time,"%a %d %b %Y %H:%M:%S"),"] Load graph\n",sep="")
	file.network <- paste(folder.data,file.input.network,".sampled",sep="")
	links <- as.matrix(read.table(file.network))
	#if(min(links)==0) links <- links + 1
	# not all sampled nodes might be in the link list,
	# since some of them might have no link with the other sampled nodes
	# so the sampled vector needs an update
	sampled <- sort(unique(c(links[,1],links[,2])))
	sample.size <- length(sampled)
end.time <- Sys.time();
total.time <- end.time - start.time;
cat("[",format(end.time,"%a %d %b %Y %H:%M:%S"),"] Loading completed in ",total.time,"\n",sep="")
	
	
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
# identify social capitalists
###############################################################################
start.time <- Sys.time();
cat("[",format(start.time,"%a %d %b %Y %H:%M:%S"),"] Detecting social capitalists\n",sep="")
	axis.names <- c("User","IFYFM","FMIFY")
	soccap.indices <- soccap.indices[sampled,]
	soccap.status <- rep(x=1,times=sample.size)
	cap.idx <- which(soccap.indices[,2]>0.8)
	temp.idx <- which(soccap.indices[cap.idx,1]>1)
	ifyfm.idx <- cap.idx[temp.idx]
	soccap.status[ifyfm.idx] <- 2
	fmify.idx <- cap.idx[-temp.idx]
	soccap.status[fmify.idx] <- 3
	soccap.indices <- NULL; gc()
end.time <- Sys.time();
total.time <- end.time - start.time;
cat("[",format(end.time,"%a %d %b %Y %H:%M:%S"),"] Process completed in ",total.time,"\n",sep="")


###############################################################################
# load raw data (i.e. role mesures)
###############################################################################
start.time <- Sys.time();
cat("[",format(start.time,"%a %d %b %Y %H:%M:%S"),"] Loading raw data\n",sep="")
	file.data <- paste(folder.data,file.input.rolemeas,sep="")
	data <- as.matrix(read.table(file.data))
	data <- data[sampled,]
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
# load membership vector
###############################################################################
start.time <- Sys.time();
cat("[",format(start.time,"%a %d %b %Y %H:%M:%S"),"] Loading membership vector\n",sep="")
	membership.file <- paste(folder.data,"cluster.k",k,".txt",sep="")
	membership <- as.matrix(read.table(membership.file))[,2] + 1	# the k-means implementation starts numbering clusters from 0
	membership <- membership[sampled]
end.time <- Sys.time();
total.time <- end.time - start.time;
cat("[",format(end.time,"%a %d %b %Y %H:%M:%S"),"] Load completed in ",total.time,"\n",sep="")


###############################################################################
# generate hive plots using social capitalist statuses as axes
###############################################################################
start.time <- Sys.time();
cat("[",format(start.time,"%a %d %b %Y %H:%M:%S"),"] Producing hive plots with statuses on axes\n",sep="")
	# re-number nodes in a consecutive way (not required by HiveR, but more convenient for filtering)
	links <- cbind(match(links[,1],sampled), match(links[,2],sampled))
	# filter nodes by axis (linked nodes located on the same axis)
	axis.filtered <- which(soccap.status[links[,1]]==soccap.status[links[,2]])
	# set up graphical parameters 
	node.colors <- c("#D1E365","#D6B7DD","#83E1A6","#EFA590","#87D8DB","#E1BD63","#BED888")
	plot.file <- paste(folder.data,"hiveplot.pdf",sep="")
	# generate hiveplots
	pdf(file=plot.file, bg="white")
	grid.newpage()
	pushViewport(viewport(layout=grid.layout(2, 4)))
	for(i in 1:length(rolemeas.names))
	{	# select data
		radii <- data[,i] #/ max(data[,i])
		axis <- as.integer(soccap.status)
		colors <- sapply(membership,function(c) node.colors[c])
		
		# remove links between nodes occupying the same position (same axis, same radius)
		radius.filtered <- which(radii[links[,1]]==radii[links[,2]])	# nodes with the same radius
		total.filtered <- intersect(axis.filtered,radius.filtered)		# nodes with the same radius and axis
		links.filtered <- links
		if(length(total.filtered)>0)
			links.filtered <- links[-total.filtered,]
		
		# detect resulting isolates
		t <- table(as.vector(links.filtered)) 
		connected.nodes <- as.integer(names(t))
		isolates <- setdiff(1:length(sampled),connected.nodes)
		# remove isolates from node and link lists
		if(length(isolates)>0)
		{	sampled.filtered <- (1:sample.size)[-isolates]
			links.filtered <- cbind(match(links.filtered[,1],sampled.filtered), match(links.filtered[,2],sampled.filtered))
			radii <- radii[-isolates]
			axis <- axis[-isolates]
			colors <- colors[-isolates]
		}
		
		# build the hiveplot object
		hpd.data <- data.frame(source=links.filtered[,1],target=links.filtered[,2], weight=1)
		hpd <- edge2HPD(edge_df=hpd.data, type="2D", axis.cols=rep("black",3))
		hpd$nodes$axis <- axis
		hpd$nodes$radius <- radii
		hpd$nodes$color <- colors
#		hpd$edges$color <- "#BEBEBE33"
		pushViewport(viewport(layout.pos.col=(i-1)%%4+1, layout.pos.row=(i-1)%/%4+1))
		plotHive(HPD=hpd, bkgnd="white",
			ch=0.1,							# size of the hole at the center 
			method="ranknorm", 					# how to position nodes on axes: "abs" "rank", "norm", "scale", "invert", "ranknorm"
			dr.nodes=TRUE, 					# whether nodes should be displayed
			axLabs=axis.names, 				# axis labels
#			axLab.pos = NULL, 				# axis label position
			axLab.gpar=gpar(col="black"),	# axis label color and others
#			anNodes = NULL, anNode.gpar = NULL,		# node label fine tuning
#			arrow = NULL,					# arrow fine tuning 
			np=FALSE						# open new device when plotting
		)
		grid.text(rolemeas.names[i], x=0.5, y=0.075)
		popViewport(2)
		# TODO remove border?
	}
	dev.off()
end.time <- Sys.time();
total.time <- end.time - start.time;
cat("[",format(end.time,"%a %d %b %Y %H:%M:%S"),"] Process completed in ",total.time,"\n",sep="")


###############################################################################
# alternatively: export network as DOT file, to be used in JHive (http://www.bcgsc.ca/wiki/display/jhive/home)
###############################################################################
#start.time <- Sys.time();
#cat("[",format(start.time,"%a %d %b %Y %H:%M:%S"),"] Exporting network as DOT files\n",sep="")
#	for(j in 1:ncol(data))
#	{	cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] ..Processing ",rolemeas.names[j]," infinite values by 0 in col.",c,")\n",sep="")
#		dot.file <- paste(folder.data,"network.",rolemeas.names[j],".dot",sep="")
#		con <- file(dot.file,"w")
#		writeLines("digraph ",rolemeas.names[j]," {", con)
#		# nodes
#		for(i in 1:sample.size)
#		{	line <- paste("node",i," [",sep="")
#				line <- paste(line," value=",data[i,j],sep="")
#			line <- paste(line," status=",axis.names[soccap.status[i]],sep="")
#			line <- paste(line," role=R",membership[i],sep="")
#			line <- paste(line," ]",sep="")
#			writeLines(line, con)
#		}
#		# links
#		for(i in 1:nrow(links))
#		{	line <- paste("node",links[i,1]," -> node",links[i,2],sep="")
#			writeLines(line, con)
#		}
#		writeLines("}", con)
#		close(con)
#	}
#end.time <- Sys.time();
#total.time <- end.time - start.time;
#cat("[",format(end.time,"%a %d %b %Y %H:%M:%S"),"] Process completed in ",total.time,"\n",sep="")
	