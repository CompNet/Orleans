# Generates hive plots of the network.
# 
# Note: the message " Error in calcCurveGrob(x, x$debug) : End points must not be identical"
# sometimes shows up when drawing the hive plots. It comes
# from the method drawing the links as splines, and means
# two nodes have the exact same position, which cannot be
# handled by this method.
# 
# version: 2
# Author: Vincent Labatut 09/2013, 01/2015
#
# setwd("~/eclipse/workspaces/Networks/Orleans/")
# setwd("C:/Eclipse/workspaces/Networks/Orleans/")
# source("PostProcessing/hive-plot.R")
###############################################################################
library("HiveR")	# generation of hive plots
library("grid")		# gpar


###############################################################################
# Returns the standard filename for the hive-plot.
#
# folder.data: folder containing all input and output files.
# role.meas: type of role measures.
# clust.algo: cluster analysis algorithm.
# comdet.algo: community detection algorithm.
# n.clust: number of detected clusters (optional).
# sample.size: size of the considered data sample.
# meas: measure represented in the plot.
###############################################################################
get.hiveplot.filename <- function(folder.data, role.meas, clust.algo, comdet.algo, n.clust=0, sample.size=NA, meas)
{	result <- paste(folder.data,"comdet=",comdet.algo,sep="")
	result <- paste(result,".rolemeas=",role.meas,sep="")
	result <- paste(result,".clust=",clust.algo,sep="")
	if(n.clust>0)
		result <- paste(result,".k=",n.clust,sep="")
	if(!is.na(sample.size))
		result <- paste(result,".smplsz=",sample.size,sep="")
	result <- paste(result,".hiveplot",sep="")
	result <- paste(result,".meas=",meas,sep="")
	result <- paste(result,".pdf",sep="")
	return(result)
}

###############################################################################
# Returns the standard filename for the node sample.
#
# folder.data: folder containing all input and output files.
# role.meas: type of role measures.
# clust.algo: cluster analysis algorithm.
# comdet.algo: community detection algorithm.
# n.clust: number of detected clusters (optional).
# sample.size: size of the considered data sample.
# graph: TRUE for the graph itself, FALSE for the sampled nodes.
###############################################################################
get.network.sample.filename <- function(folder.data, role.meas, clust.algo, comdet.algo, n.clust=0, sample.size=NA, graph)
{	result <- paste(folder.data,"comdet=",comdet.algo,sep="")
	result <- paste(result,".rolemeas=",role.meas,sep="")
	result <- paste(result,".clust=",clust.algo,sep="")
	if(n.clust>0)
		result <- paste(result,".k=",n.clust,sep="")
	if(!is.na(sample.size))
		result <- paste(result,".smplsz=",sample.size,sep="")
	result <- paste(result,".sample",sep="")
	if(graph)
		result <- paste(result,".edgelist",sep="")
	else
		result <- paste(result,".txt",sep="")
	return(result)
}


###############################################################################
#
#
# folder.data: folder containing all input and output files.
# role.meas: type of role measures.
# clust.algo: cluster analysis algorithm.
# comdet.algo: community detection algorithm.
# sample.size: if the plot generation takes too long, you can work on a sample of the data.
#			   this parameter specifies the size of this sample (NA to use the
#			   whole dataset).
# overlap.threshold: threshold used for the overlap index (determines if a 
#				     user is a social capitalist or not).
# force: whether or not to redo the calculation when the plot files already exist.
###############################################################################
draw.hiveplots <- function(folder.data, role.meas, clust.algo, comdet.algo, sample.size=NA, overlap.threshold, force)
{	cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] Generating hive plots\n",sep="")
	
	# init file names
	files <- c()
	for(meas in get.rolemeas.names(role.meas))
		files <- c(files, get.hiveplot.filename(folder.data, role.meas, clust.algo, comdet.algo, sample.size=sample.size, meas=meas))
	
	# check if the files already exist
	if(all(file.exists(files)) && !force)
	{	cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] All the plots already exist\n",sep="")
	}
	
	else
	{	# retrieve the network sample
		tmp <- sample.network.for.hiveplot(folder.data, role.meas, clust.algo, comdet.algo, sample.size, force)
		g <- tmp$g
		sample.nodes <- tmp$sample.nodes
		# get the corresponding edgelist
		links <- get.edgelist(graph=g, names=FALSE)
		
		# load community role measures
		rolemeas.vals <- retrieve.role.measures(folder.data,role.meas,comdet.algo)
		rolemeas.vals <- rolemeas.vals[sample.nodes,]
		
		# load cluster membership
		membership.file <- get.cluster.filename(folder.data, role.meas, clust.algo, comdet.algo)
		membership <- as.vector(as.matrix(read.table(membership.file)))
		membership <- membership[sample.nodes]
		
		# identify social capitalists
		socap.vals <- retrieve.socap.indices(folder.data,role.meas)
		socap.vals <- socap.vals[sample.nodes,]
		socap.types <- identify.social.capitalists(values=socap.vals,overlap.threshold)
		
		# generate the hive plots
		generate.hiveplots(folder.data, role.meas, clust.algo, comdet.algo, membership, rolemeas.vals, socap.types, links, sample.size, force)
	}
}

###############################################################################
# Draws a sample of the network nodes, to lighten the computational load when
# generating the hive plots.
#
# folder.data: folder containing all input and output files.
# role.meas: type of role measures.
# clust.algo: cluster analysis algorithm.
# comdet.algo: community detection algorithm.
# sample.size: size of the sample to draw from the network node set.
# force: whether or not to redo the calculation when the sample file already exists.
###############################################################################
sample.network.for.hiveplot <- function(folder.data, role.meas, clust.algo, comdet.algo, sample.size=NA, force)
{	cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] Draw a sample of size ",sample.size,"\n",sep="")
	graph.file <- get.network.filename(folder.data)
	dir <- is.directed.rolemeas(role.meas)
	
	if(is.na(sample.size))
	{	g <- read.graph(file=graph.file,format="edgelist",directed=dir)
		sampled.nodes <- 1:vcount(g)
	}	
	
	else
	{	sample.graph.file <- get.network.sample.filename(folder.data, role.meas, clust.algo, comdet.algo, sample.size=sample.size, graph=TRUE)
		sample.nodes.file <- get.network.sample.filename(folder.data, role.meas, clust.algo, comdet.algo, sample.size=sample.size, graph=FALSE)
		
		# if already processed, just load the file
		if(file.exists(sample.graph.file) && file.exists(sample.nodes.file) && !force)
		{	cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] Load sample file\n",sep="")
			g <- read.graph(file=sample.graph.file,format="edgelist",directed=dir)
			sampled.nodes <- as.vector(as.matrix(read.table(sample.nodes.file)))
			cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] Sample size: ",length(sampled),"\n",sep="")
			cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] Min sample value: ",min(sampled),"\n",sep="")
		}
		
		# otherwise, sample needs to be processed and recorded
		else
		{	# load the whole graph
			g <- read.graph(file=graph.file,format="edgelist",directed=dir)
			# process total degree
			d <- degree(graph=g,mode="all")
			# sample amongst nodes with non-zero degree (otherwise it's useless for a hiveplot)
			idx <- which(d>0)
			sample.size <- min(sample.size,length(idx))
			sampled.nodes <- sort(sample(x=idx,size=sample.size))
			g <- induced.subgraph(graph=g, vids=sampled.nodes)
			# record sample graph
			write.graph(graph=g,file=sample.graph.file,format="edgelist")
			write.table(x=sampled.nodes,file=sample.nodes.file,col.names=FALSE,row.names=FALSE)
		}
	}
	
	res <- list(g=g, sample.nodes=sampled.nodes)
	return(res)
}

###############################################################################
# Generates hive plots using social capitalist statuses as axes.
#
# folder.data: folder containing all input and output files.
# role.meas: type of role measures.
# clust.algo: cluster analysis algorithm.
# comdet.algo: community detection algorithm.
# membership: number of the clusters.
# rolemeas.vals: values of the community role measures.
# socap.types: types of social capitalists.
# links: edgelist (i.e. 2-columns matrix containing the sampled edges).
# sample.size: size of the sample to draw from the network node set.
# force: whether or not to redo the calculation when the sample file already exists.
###############################################################################
generate.hiveplots <- function(folder.data, role.meas, clust.algo, comdet.algo, membership, rolemeas.vals, socap.types, links, sample.size=NA, force)
{	start.time <- Sys.time()
	cat("[",format(start.time,"%a %d %b %Y %H:%M:%S"),"] Producing hive plots with statuses on axes\n",sep="")
		rolemeas.names <- get.rolemeas.names(role.meas)
		# set the names of the axis
		axis.names <- get.socap.types()
		# set up graphical parameters 
		node.colors <- c("#D1E365","#D6B7DD","#83E1A6","#EFA590","#87D8DB","#E1BD63","#BED888")
		
		# generate hiveplots
		for(i in 1:length(rolemeas.names))
		{	plot.file <- get.hiveplot.filename(folder.data, role.meas, clust.algo, comdet.algo, n.clust=0, sample.size=NA, meas=rolemeas.names[i])
			
			# select data
			radii <- rolemeas.vals[,i] #/ max(data[,i])
			axis <- socap.types
			#print(axis)			
			colors <- node.colors[membership+1]
			colors[is.na(colors)] <- "#AAAAAA" # grey for more than 7 clusters, this has to be manually tuned
			
			# build the hiveplot object
			hpd.data <- data.frame(source=links[,1],target=links[,2], weight=1)
			hpd <- edge2HPD(edge_df=hpd.data, type="2D", axis.cols=rep("black",3))
			
			# convert node ids
			norm.hpd <- sumHPD(hpd, plot.list=TRUE)
			new.ids <- c(norm.hpd[,1],norm.hpd[,5])
			old.ids <- c(as.integer(as.character(norm.hpd[,3])),as.integer(as.character(norm.hpd[,7])))
			node.nbr <- max(new.ids)
			idx <- match(1:node.nbr, new.ids)
			conv.table <- old.ids[idx]
			
			# setup the rest of the hiveplot information
			hpd$nodes$axis <- as.integer(axis[conv.table])
			hpd$nodes$radius <- radii[conv.table]
			hpd$nodes$color <- colors[conv.table]
			hpd$edges$color <- "#BEBEBE33"
			
			# deal with nodes occupying the exact same location (jiggle)
			prob.links <- which(norm.hpd[,2]==norm.hpd[,6] & norm.hpd[,4]==norm.hpd[,8])
			prob.nodes <- unique(c(norm.hpd[prob.links,1],norm.hpd[prob.links,5]))
			hpd$nodes$radius[prob.nodes] <- hpd$nodes$radius[prob.nodes] + runif(length(prob.nodes),0,0.01)
			#sumHPD(hpd, chk.sm.pt=TRUE) # check hive plot
			
			# generate the plot
			pdf(file=plot.file, bg="white")
			plotHive(HPD=hpd, bkgnd="white",
				ch=0.1,							# size of the hole at the center 
				method="norm", 					# how to position nodes on axes: "abs" "rank", "norm", "scale", "invert", "ranknorm"
				dr.nodes=TRUE, 					# whether nodes should be displayed
				axLabs=axis.names, 				# axis labels
#				axLab.pos = NULL, 				# axis label position
				axLab.gpar=gpar(col="black"),	# axis label color and others
#				anNodes = NULL, anNode.gpar = NULL,		# node label fine tuning
#				arrow = NULL,					# arrow fine tuning 
				np=FALSE						# open new device when plotting
			)
			dev.off()
		}
	end.time <- Sys.time()
	total.time <- end.time - start.time
	cat("[",format(end.time,"%a %d %b %Y %H:%M:%S"),"] Process completed in ",format(total.time),"\n",sep="")
}




###############################################################################
# alternatively: export network as DOT file, to be used in JHive (http://www.bcgsc.ca/wiki/display/jhive/home)
###############################################################################
#start.time <- Sys.time()
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
#end.time <- Sys.time()
#total.time <- end.time - start.time
#cat("[",format(end.time,"%a %d %b %Y %H:%M:%S"),"] Process completed in ",format(total.time),"\n",sep="")
	