# Perform an Anova on the detected clusters.
# v1
# 
# Author: Vincent Labatut 06/2013
# source("C:/Eclipse/workspaces/Networks/Orleans/src/main-anova.R")
# source("/home/vlabatut/eclipse/workspaces/Networks/Orleans/src/main-anova.R")
###############################################################################

library("car")
library("multcomp")
#print(commandArgs())
#print(commandArgs()[6])

# data folder 					#TODO update depending on local file system
#folder.data <- "C:/Eclipse/workspaces/Networks/Orleans/data/"	
folder.data <- "/home/vlabatut/eclipse/workspaces/Networks/Orleans/data/"	

# load membership vector
cat("[",format(Sys.time(),"%a %d %b %Y %X"),"] Load membership vector\n",sep="")
membership.file <- paste(folder.data,"normalized.numbered.txt.membership",sep="")
t <- read.table(membership.file)
membership <- t[,2] + 1
t <- NULL; gc()

# load data
cat("[",format(Sys.time(),"%a %d %b %Y %X"),"] Load raw data\n",sep="")
data.file <- paste(folder.data,"data.txt",sep="")
data <- as.matrix(read.table(data.file))
data <- data[,-(1:2)]

# clean data
cat("[",format(Sys.time(),"%a %d %b %Y %X"),"] Clean data\n",sep="")
for(c in 1:ncol(data))
{	# get rid of NA (?)
	cat("[",format(Sys.time(),"%a %d %b %Y %X"),"] ..",c,": Remove NA symbols\n",sep="")
	idx.na <- which(is.na(data[,c]))
	if(length(idx.na)>0)
		data[idx.na,c] <- 0
	
	# get rid of infinite values
	cat("[",format(Sys.time(),"%a %d %b %Y %X"),"] ..",c,": Remove infinite symbols\n",sep="")
	idx.inf <- which(is.infinite(data[,c]))
	if(length(idx.inf)>0)
		data[idx.inf,c] <- 0
}

# create factors
cat("[",format(Sys.time(),"%a %d %b %Y %X"),"] Create factors\n",sep="")
clusters <- factor(membership)
membership <- NULL; gc()

# process each measure separately
#i <- as.integer(commandArgs()[6])
for(i in 1:ncol(data))
{	cat("[",format(Sys.time(),"%a %d %b %Y %X"),"] ************** #",i," **************\n",sep="")
	gc()
	
	# reload data? (to avoid using too much memory at once)
#	data.file <- paste(folder.data,"normalized.txt",sep="")
#	data <- as.matrix(read.table(data.file))
	values <- data[,i]
	
	# test for homogeneity of variances (homoskedasticity): p>0.05 means ok
	#print(bartlett.test(values,clusters))		# primary test
	#print(fligner.test(values,clusters))		# alternative test
	
	# process anova: p>0.05 means can't say one mean is significantly different
	# this is the usual anova function
	# however, it complains about the factors being unbalanced
	# ie: clusters have much different sizes 
#	options(contrasts = c("contr.helmert", "contr.poly"))
#	av <- aov(formula= values~clusters)
	# alternate anova function, takes the unbalance into account
	model <- lm(values ~ clusters)
	av <- anova(model)
	# display the anova results
	print(av)
	summary(av)
	#av <- NULL;gc()
	
	# post-hoc tests to identify which means are differents
	#tt <- pairwise.t.test(values,clusters,p.adj="none")	# t-test without adjustment
	tt <- pairwise.t.test(values,clusters,p.adj="bonf")	# bonferroni adjustment
	#tt <- pairwise.t.test(values,clusters,p.adj="holm")	# holm adjustment
	print(tt)
	# Tukey test for the aov (usual) anova function
#	tk <- TukeyHSD(model)										# Tukey's test
	# Tukey test for the Anova function, takes too much memory
#	tk <- glht(model, linfct=mcp(clusters="Tukey"))
#	print(tk)
#	summary(tk)
	
	anova.file <- paste(folder.data,"anova.",i,".txt",sep="")
	sink(anova.file)
		print(av)
		summary(av)
		print(tt)
		#summary(tt)
	sink()

	cat("[",format(Sys.time(),"%a %d %b %Y %X"),"] Done\n",sep="")
}

#quit(save="no")
# Rscript main-anova.R 1;Rscript main-anova.R 2;Rscript main-anova.R 3;Rscript main-anova.R 4;Rscript main-anova.R 5;Rscript main-anova.R 6;Rscript main-anova.R 7;Rscript main-anova.R 8