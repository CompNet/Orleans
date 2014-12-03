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
{	# set up path
	app.folder <- "CommunityDetection/Oslom/"
	
	# renumber the original edgelist, if necessary
	clean.file <- get.network.clean.filename(folder.data)
	if(!file.exists(clean.file))
		clean.graph.file(folder.data)
	
	# TODO check which input format oslom actually wants
	# might be necessary to move the call to clean.file in the concerned functions 
	# if oslom doesn't need them 
	
	# set up the command, including all required parameters
	if(comdet.algo %in% c("OSLOM-undir-p", "OSLOM-undir-o"))
		cmd <- paste(app.folder,"oslom_undir -f ",clean.file," -uw",sep="")
	else if(comdet.algo %in% c("OSLOM-dir-p", "OSLOM-dir-o"))
		cmd <- paste(app.folder,"oslom_dir -f ",clean.file," -uw",sep="")
	
	# execute the command from R
	system(command=cmd) 
	
	# possibly rename the outputted file (to fit the R scripts requirements)
	# also possibly clean or normalize the outputted file
	# the community structure file must just be a vector of community ids starting from 0
	# TODO
}
