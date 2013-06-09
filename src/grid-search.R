# Estimates best parameter values using a grid search.
# v1.1
# 
# Author: Vincent Labatut 06/2013
# source("C:/Eclipse/workspaces/Networks/Orleans/src/grid-search.R")
###############################################################################
library("plyr")

###############################################################################
# Iteratively applies the grid search method, in order to improve the
# estimated parameter values.
#
# foo: 
#	Objective function
# parameter.list: 
#	Named list of parameters. Each element in the list is
#	a vector of values for the parameter whose name is specified.
# maximize: 
#	TRUE to maximize (default), FALSE to minimize
# iterations:
#	Number of iterations to perform.
#
# returns:
#	A named list, containing the combination of parameters
#	corresponding to the optimal value for the considered function.
###############################################################################
iterative.grid.search <- function(foo, parameter.list, maximize=TRUE, iterations=1)
{	start.time <- Sys.time();
	cat("[",format(start.time,"%a %d %b %Y %X"),"] Applying grid search\n",sep="")
	
	par.names <- names(parameter.list)
	params <- parameter.list
	breaks <- list()
	initial.from <- list()
	initial.to <- list()
	for(par in par.names)
	{	val <- parameter.list[[par]]
		breaks[[par]] <- length(val)
		initial.from[[par]] <- val[1]
		initial.to[[par]] <- val[length(val)]
	}
#	print("breaks");print(breaks)			

	
	optimal.value <- NULL
	optimal.parameters <- NULL
	
	for(i in 1:iterations)
	{	
#		print("params");print(params)
		# process optimal parameters for grid
		temp <- grid.search(foo,params,maximize)
#		print("temp");print(temp)		
		
		# compare with existing parameters
		if(is.null(optimal.value) || temp$V1>optimal.value)
			optimal.parameters <- temp[-ncol(temp)]
		
		# iterate on smaller grid
		new.params <- list()
		for(p in par.names)
		{	values <- params[[p]]
#			print("values");print(values)
			opt.value <-  temp[[p]]
			idx <- which(values==opt.value)
#			print("idx");print(idx)
			if(is.integer(values))
			{	step <- max(1,(values[2]-values[1])%/%2)
#				print("step");print(step)
				from <- values[idx] - step
				from <- max(from,initial.from[[p]])
#				print("from");print(from)
				to <- values[idx] + step
				to <- min(to,initial.to[[p]])
#				print("to");print(to)
				by <- (to-from) / (breaks[[p]]-2)
				by <- max(1,floor(by))
#				print("by");print(by)
				new.values <- as.integer(seq(from,to,by))
				new.params[[p]] <- new.values
			}
			else
			{	step <- (values[2]-values[1]) / 2
#				print("step");print(step)
				from <- values[idx] - step
				from <- max(from,initial.from[[p]])
#				print("from");print(from)
				to <- values[idx] + step
				to <- min(to,initial.to[[p]])
#				print("to");print(to)
				by <- (to-from) / (breaks[[p]]-2)
#				print("by");print(by)
				new.values <- seq(from,to,by)
				new.params[[p]] <- new.values
			}
		}
		params <- new.params
	}
	
	end.time <- Sys.time();
	total.time <- end.time - start.time;
	cat("[",format(end.time,"%a %d %b %Y %X"),"] grid search completed in ",total.time,"\n",sep="")
	
	optimal.parameters <- as.list(optimal.parameters)
	return(optimal.parameters)
}

###############################################################################
# Optimizes the specified objective function, using the grid search method.
#
# First, the function is applied to every combination of parameters built
# using the specified list. Then, the combination of parameters corresponding
# to the optimal value is returned.
#
# foo: 
#	objective function
# parameter.list: 
#	named list of parameters. Each element in the list is
#	a vector of values for the parameter whose name is specified.
# maximize: 
#	TRUE to maximize (default), FALSE to minimize
#
# returns:
#	a named list, containing the combination of parameters
#	corresponding to the optimal value for the considered function.
###############################################################################
grid.search <- function(foo, parameter.list, maximize=TRUE)
{	cat("[",format(Sys.time(),"%a %d %b %Y %X"),"] Applying grid search for parameters:\n",sep="")
#	print(parameter.list)
	
	# generate a data frame containing all combinations of parameters
	param.df = do.call(expand.grid, parameter.list)
	#print("param.df");print(param.df)	
	
	# apply the function to all combinations of parameters
	results <- mdply(param.df, foo)
	
	# identify the optimal result
	if(maximize)
		index <-  which.max(results$V1)
	else
		index <-  which.min(results$V1)
	
	# retrieve the corresponding parameters
	optimal <- results[index,]
	#print(optimal)	
	
	return(optimal)
}

###############################################################################
# test
###############################################################################
#foo <- function(x,y){sin(x/y)}
#parameter.list <- list(x=1:10,y=58:63)
#grid.search(foo, parameter.list)
#iterative.grid.search(foo, parameter.list, maximize=TRUE, iterations=10)
