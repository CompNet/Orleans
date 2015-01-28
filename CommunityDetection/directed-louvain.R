# Adapter allowing to apply the directed version of the Louvain
# community algorithm we obtained by modifying the original source code.
# 
# Version: 1
# Author: Vincent Labatut 07/2014
#
# setwd("~/eclipse/workspaces/Networks/Orleans/")
# setwd("C:/Eclipse/workspaces/Networks/Orleans/")
# source("CommunityDetection/directed-louvain.R")
###############################################################################

###############################################################################
# Applies the directed version of the Louvain community detection algorithm.
#
# folder.data: folder containing the network file, and will contain the community file.
# comdet.algo: community detection method.
###############################################################################
apply.directed.louvain <- function(folder.data, comdet.algo)
{	compiled.file <- get.network.clean.filename(folder.data)
	
	# compile to a binary file if necessary
	if(!file.exists(compiled.file))
		compile.graph.file(folder.data)
	
	# set up the command, including all required parameters
	cmd <- paste("blablabla ",compiled.file,sep="") # TODO @Nico complete with appropriate parameters
	
	# execute the command from R
	system(command=cmd) 
	
	# possibly rename the outputted file (to fit the R scripts requirements)
	# TODO @Nico
	
	# convert the 2 column files (node,com) to a single column one (com)
	# TODO @Nico
}
