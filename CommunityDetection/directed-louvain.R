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
# algo: community detection method.
###############################################################################
apply.directed.louvain <- function(folder.data, algo)
{	# set up the command, including all required parameters
	net.file <- get.network.filename()
	in.file <- paste(folder.data,net.file,sep="")
	cmd <- paste("blablabla ",in.filesep="") # TODO
	
	# execute the command from R
	system(command=cmd) 
	
	# possibly rename the outputted file (to fit the R scripts requirements)
	# TODO
}
