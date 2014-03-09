# Performs an Anova on the detected clusters.
# One Anova is performed on each measure taken separately.
# The goal is to understand which measures are discriminant
# for the detected cluster, in order to identify their associated
# role.
#
# version: 1
# Author: Vincent Labatut 09/2013
#  
# Use: this script should not be used like the others. For memory reasons,
# we could not process all measures at once. It is necessary to restart R
# to completely reset the memory (for some reason, gc() is not sufficient).
# So, you should run this script from the console, passing the number of the measure
# to be tested, as a parameter of the command. Chain several commands
# to execute the script as many times as you have measure. In our case, we
# have 2 measures, so we just do (from the OS console): 
# 	cd ~/eclipse/workspaces/Networks/Orleans/src/old_undir
# 	Rscript p09-anova.R 1;Rscript p09-anova.R 2
###############################################################################
library("car")
#library("multcomp")


###############################################################################
# setup files
###############################################################################
#setwd("C:/Eclipse/workspaces/Networks/Orleans/")
setwd("~/eclipse/workspaces/Networks/Orleans/")
folder.data <- "data/"	
file.input <- "rolemeasures.raw.txt"
k <- 2												# TODO we work only on the clusters found for this k


###############################################################################
# command parameters
###############################################################################
print(commandArgs())
measure.nbr <- as.integer(commandArgs()[6])
cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] role measure processed: #",measure.nbr,"\n",sep="")


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
			cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] ....WARNING: ",length(idx.na)," NA values found in the raw data (col.",c,")\n")
		}
		
		# get rid of infinite values
		idx.inf <- which(is.infinite(data[,c]) | data[,c]==1.79769e+308)
		if(length(idx.inf)>0)
		{	data[idx.inf,c] <- 0
			cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] ....Replacing ",length(idx.inf)," infinite values by 0 in col.",c,")\n")
		}
	}
end.time <- Sys.time();
total.time <- end.time - start.time;
cat("[",format(end.time,"%a %d %b %Y %H:%M:%S"),"] Cleaning completed in ",format(total.time),"\n",sep="")


###############################################################################
# create factors
###############################################################################
start.time <- Sys.time();
cat("[",format(start.time,"%a %d %b %Y %H:%M:%S"),"] Creating factors\n",sep="")
	clusters <- factor(membership)
	membership <- NULL; gc()
end.time <- Sys.time();
total.time <- end.time - start.time;
cat("[",format(end.time,"%a %d %b %Y %H:%M:%S"),"] Factors created in ",format(total.time),"\n",sep="")
	

###############################################################################
# process only the measure specified when calling the command
###############################################################################
start.time <- Sys.time();
cat("[",format(start.time,"%a %d %b %Y %H:%M:%S"),"] Performing Anova for role measure #",measure.nbr,"\n",sep="")
	values <- data[,measure.nbr]
	
	# test for homogeneity of variances (homoskedasticity): p>0.05 means ok
	#print(bartlett.test(values,clusters))		# primary test
	#print(fligner.test(values,clusters))		# alternative test
	
	# process anova: p>0.05 means we can't say one mean is significantly different
	# this is the usual anova function
	# however, it complains about the factors being unbalanced (for our data)
	# i.e.: clusters have much different sizes 
#	options(contrasts = c("contr.helmert", "contr.poly"))
#	av <- aov(formula= values~clusters)
	# so we use this alternate anova function, which takes the unbalance into account
	model <- lm(values ~ clusters)
	av <- anova(model)
	# display the anova results
	print(av)
	print(summary(av))
	
	# post-hoc tests to identify which means are differents
	# Tukey test for the aov (usual) anova function (we can't use it because of the imbalance, remember?)
#	tk <- TukeyHSD(model)										# Tukey's test
	# Tukey test for the Anova function, it takes too much memory
#	tk <- glht(model, linfct=mcp(clusters="Tukey"))
#	print(tk)
#	summary(tk)
	# so in the end, we used classic t-test, with a specific correction for Anova
	#tt <- pairwise.t.test(values,clusters,p.adj="none")	# t-test without adjustment
	tt <- pairwise.t.test(values,clusters,p.adj="bonf")	# bonferroni adjustment
	#tt <- pairwise.t.test(values,clusters,p.adj="holm")	# holm adjustment
	print(tt)

	anova.file <- paste(folder.data,"anova.",measure.nbr,".txt",sep="")
	sink(anova.file)
		print(av)
		print(summary(av))
		print(tt)
		#summary(tk)
	sink()

end.time <- Sys.time();
total.time <- end.time - start.time;
cat("[",format(end.time,"%a %d %b %Y %H:%M:%S"),"] Anova completed in ",format(total.time),"\n",sep="")


# force quitting R (to reset memory)
quit(save="no")
