# Adapter allowing to apply the Oslom algorithm from Lancichinetti et al.
# A. Lancichinetti, F. Radicchi, J.J. Ramasco and S. Fortunato, PLoS ONE 6, e18961 (2011).
# http://www.oslom.org/
#
# Version: 1
# Author: Vincent Labatut 07/2014
#
# setwd("~/eclipse/workspaces/Networks/Orleans/")
# setwd("C:/Eclipse/workspaces/Networks/Orleans/")
# source("CommunityDetection/oslom.R")
###############################################################################

###############################################################################
# Applies the Oslom algorithm.
#
# folder.data: folder containing the network file, and will contain the community file.
# comdet.algo: community detection method.
###############################################################################
apply.oslom <- function(folder.data, comdet.algo)
{	# set up paths
	net.file <- get.network.filename(folder.data)
	app.folder <- "CommunityDetection/Oslom/"
	
	# set up the command, including all required parameters
	if(comdet.algo %in% c("OSLOM-undir-p", "OSLOM-undir-o"))
		cmd <- paste(app.folder,"oslom_undir -f ",net.file," -uw",sep="")
	else if(comdet.algo %in% c("OSLOM-dir-p", "OSLOM-dir-o"))
		cmd <- paste(app.folder,"oslom_dir -f ",net.file," -uw",sep="")
	
	# execute the command from R
	system(command=cmd) 
	
	# possibly rename the outputted file (to fit the R scripts requirements)
	# TODO
}
