# Processes various calculations on the social
# capitalists indices.
# 
# Version: 2
# Author: Vincent Labatut 06/2013,07/2014
#
# setwd("~/eclipse/workspaces/Networks/Orleans/")
# setwd("C:/Eclipse/workspaces/Networks/Orleans/")
# source("RoleMeasures/role-measures.R")
###############################################################################

###############################################################################
# Returns the list of names for the community role measures
# corresponding to the specified code.
# role.meas: role measure variant:
#				- "GA": original Guimera-Amaral measures (2 measures)
#				- "GAdir": our directd extension of GA (2x2 measures)
#				- "DLPp": our own measures for partition-based communities (2x4 measures)
#				- "DLPo": variant for overlapping communities  (2x6 measures)
###############################################################################
get.rolemeas.names <- function(role.meas)
{	if(role.meas=="GA")
		result <- c("P","z")
	else if(role.meas=="GAdir")
		result <- c("P-in", "P-out", "z-in", "z-out")
	else if(role.meas=="DLPp")
		result <- c("I-int-in","I-int-out","I-ext-in","I-ext-out","H-in","H-out","D-in","D-out")
	else if(role.meas=="DLPo")
		result <- c("I-int-in","I-int-out","H-int-in","H-int-out","D-int-in","D-int-out","I-ext-in","I-ext-out","H-ext-in","H-ext-out","D-ext-in","D-ext-out")
	
	return(result)
}

###############################################################################
# Returns the name of the file containing the role measures, 
# for the specified code.
# role.meas: role measure variant:
#				- "GA": original Guimera-Amaral measures (2 measures)
#				- "GAdir": our directd extension of GA (2x2 measures)
#				- "DLPp": our own measures for partition-based communities (2x4 measures)
#				- "DLPo": variant for overlapping communities  (2x6 measures)
###############################################################################
get.rolemeas.filename <- function(role.meas)
{	result <- paste("rolemeas.",role.meas,".txt",sep="")
	return(result)
}

###############################################################################
# Returns the name of the file containing the role measures, 
# for the specified code.
# folder.data: folder containing all input and output files.
# role.meas: role measure variant to process:
#				- "GA": original Guimera-Amaral measures (2 measures)
#				- "GAdir": our directd extension of GA (2x2 measures)
#				- "DLPp": our own measures for partition-based communities (2x4 measures)
#				- "DLPo": variant for overlapping communities  (2x6 measures)
# comdet.algo: algorithm used for community detection (needed to infer the file name)
###############################################################################
process.rolemeas <- function(folder.data, role.meas, comdet.algo)
{	# get file paths
	net.file <- paste(folder.data,get.network.filename(),sep="")
	com.file <- paste(folder.data,get.communities.filename(algo),sep="")
	
	# set up the appropriate command
	if(role.meas=="GA")
		cmd <- ""	# TODO
	else if(role.meas=="GAdir")
		cmd <- ""	# TODO
	else if(role.meas=="DLPp")
		cmd <- ""	# TODO
	else if(role.meas=="DLPo")
		cmd <- ""	# TODO
	
	# execute the command from R
	system(command=cmd) 
	
	# possibly post-process
	# TODO
}

