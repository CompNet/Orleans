# Performs an ANOVA (analysis of variance) on each role measure,
# considering the estimated clusters as groups. The goal is to
# check whether the cluster are significantly differnet.
# Various files corresponsing to stastical tests are output,
# as well as the the result of the ANOVA itself.
#
# version: 2
# Author: Vincent Labatut 06/2013, 01/2015
#  
# setwd("~/eclipse/workspaces/Networks/Orleans/")
# setwd("C:/Eclipse/workspaces/Networks/Orleans/")
# source("PostProcessing/cluster-distribution.R")
###############################################################################
library("fBasics")		# Homoskedasticity test
#library("car")
library("multcomp")		# Tukey's post-hoc test


###############################################################################
# Returns the standard filename for the qq-plots of the lm residuals.
#
# folder.data: folder containing all input and output files.
# role.meas: type of role measures.
# clust.algo: cluster analysis algorithm.
# comdet.algo: community detection algorithm.
# n.clust: number of detected clusters (optional).
# meas: the role measure considered here.
###############################################################################
get.anova.qq.filename <- function(folder.data, role.meas, clust.algo, comdet.algo, n.clust=0, meas)
{	result <- paste(folder.data,"comdet=",comdet.algo,sep="")
	result <- paste(result,".rolemeas=",role.meas,sep="")
	result <- paste(result,".clust=",clust.algo,sep="")
	if(n.clust>0)
		result <- paste(result,".k=",n.clust,sep="")
	result <- paste(result,".anova",sep="")
	result <- paste(result,".meas=",meas,sep="")
	result <- paste(result,".qqplot",sep="")
	result <- paste(result,".pdf",sep="")
	return(result)
}

###############################################################################
# Returns the standard filename for the results fof the homogeneity of variances 
# (homoskedasticity) test.
#
# folder.data: folder containing all input and output files.
# role.meas: type of role measures.
# clust.algo: cluster analysis algorithm.
# comdet.algo: community detection algorithm.
# n.clust: number of detected clusters (optional).
# meas: the role measure considered here.
###############################################################################
get.anova.homosked.filename <- function(folder.data, role.meas, clust.algo, comdet.algo, n.clust=0, meas)
{	result <- paste(folder.data,"comdet=",comdet.algo,sep="")
	result <- paste(result,".rolemeas=",role.meas,sep="")
	result <- paste(result,".clust=",clust.algo,sep="")
	if(n.clust>0)
		result <- paste(result,".k=",n.clust,sep="")
	result <- paste(result,".anova",sep="")
	result <- paste(result,".meas=",meas,sep="")
	result <- paste(result,".homosked",sep="")
	result <- paste(result,".txt",sep="")
	return(result)
}

###############################################################################
# Returns the standard filename for the results of the normality test of 
# the residuals.
#
# folder.data: folder containing all input and output files.
# role.meas: type of role measures.
# clust.algo: cluster analysis algorithm.
# comdet.algo: community detection algorithm.
# n.clust: number of detected clusters (optional).
# meas: the role measure considered here.
###############################################################################
get.anova.resnorm.filename <- function(folder.data, role.meas, clust.algo, comdet.algo, n.clust=0, meas)
{	result <- paste(folder.data,"comdet=",comdet.algo,sep="")
	result <- paste(result,".rolemeas=",role.meas,sep="")
	result <- paste(result,".clust=",clust.algo,sep="")
	if(n.clust>0)
		result <- paste(result,".k=",n.clust,sep="")
	result <- paste(result,".anova",sep="")
	result <- paste(result,".meas=",meas,sep="")
	result <- paste(result,".resnorm",sep="")
	result <- paste(result,".txt",sep="")
	return(result)
}

###############################################################################
# Returns the standard filename for the results of the ANOVA.
#
# folder.data: folder containing all input and output files.
# role.meas: type of role measures.
# clust.algo: cluster analysis algorithm.
# comdet.algo: community detection algorithm.
# n.clust: number of detected clusters (optional).
# meas: the role measure considered here.
###############################################################################
get.anova.result.filename <- function(folder.data, role.meas, clust.algo, comdet.algo, n.clust=0, meas)
{	result <- paste(folder.data,"comdet=",comdet.algo,sep="")
	result <- paste(result,".rolemeas=",role.meas,sep="")
	result <- paste(result,".clust=",clust.algo,sep="")
	if(n.clust>0)
		result <- paste(result,".k=",n.clust,sep="")
	result <- paste(result,".anova",sep="")
	result <- paste(result,".meas=",meas,sep="")
	result <- paste(result,".result",sep="")
	result <- paste(result,".txt",sep="")
	return(result)
}

###############################################################################
# Returns the standard filename for the results of the post-hoc test conducted
# after the ANOVA.
#
# folder.data: folder containing all input and output files.
# role.meas: type of role measures.
# clust.algo: cluster analysis algorithm.
# comdet.algo: community detection algorithm.
# n.clust: number of detected clusters (optional).
# meas: the role measure considered here.
###############################################################################
get.anova.posthoc.filename <- function(folder.data, role.meas, clust.algo, comdet.algo, n.clust=0, meas)
{	result <- paste(folder.data,"comdet=",comdet.algo,sep="")
	result <- paste(result,".rolemeas=",role.meas,sep="")
	result <- paste(result,".clust=",clust.algo,sep="")
	if(n.clust>0)
		result <- paste(result,".k=",n.clust,sep="")
	result <- paste(result,".anova",sep="")
	result <- paste(result,".meas=",meas,sep="")
	result <- paste(result,".posthoc",sep="")
	result <- paste(result,".txt",sep="")
	return(result)
}

###############################################################################
# Performs an ANOVA on each role measure, considering the estimated clusters
# as roles.
# 
# folder.data: folder containing all input and output files.
# role.meas: type of role measures.
# clust.algo: cluster analysis algorithm.
# comdet.algo: community detection algorithm.
###############################################################################
process.cluster.anonva <- function(folder.data, role.meas, clust.algo, comdet.algo)
{	# load membership vector
	start.time <- Sys.time();
	cat("[",format(start.time,"%a %d %b %Y %H:%M:%S"),"] Loading membership vector\n",sep="")
		membership.file <- get.cluster.filename(folder.data, role.meas, clust.algo, comdet.algo)
		membership <- as.vector(as.matrix(read.table(membership.file)))
		clusters <- factor(membership)
		k <- max(membership)
	end.time <- Sys.time();
	total.time <- end.time - start.time;
	cat("[",format(end.time,"%a %d %b %Y %H:%M:%S"),"] Load completed in ",format(total.time),"\n",sep="")
	
	# load raw data
	start.time <- Sys.time();
	cat("[",format(start.time,"%a %d %b %Y %H:%M:%S"),"] Loading raw data\n",sep="")
		file.data <- get.rolemeas.filename(folder.data, role.meas, norm=FALSE, comdet.algo)
		data <- as.matrix(read.table(file.data,header=TRUE))
	end.time <- Sys.time();
	total.time <- end.time - start.time;
	cat("[",format(end.time,"%a %d %b %Y %H:%M:%S"),"] Load completed in ",format(total.time),"\n",sep="")
	
	# process each measure
	rolemeas.names <- get.rolemeas.names(role.meas)
	for(m in 1:length(rolemeas.names))
	{	start.time <- Sys.time();
		cat("[",format(start.time,"%a %d %b %Y %H:%M:%S"),"] Processing role measure ",rolemeas.names[m],"\n",sep="")
			# test for homogeneity of variances (homoskedasticity)
			# p>0.05 means the null hypothesis could not be rejected
			cat("[",format(start.time,"%a %d %b %Y %H:%M:%S"),"] ..Testing homogeneity of variances (homoskedasticity)\n",sep="")
				cat("[",format(start.time,"%a %d %b %Y %H:%M:%S"),"] ....Barlett's test: \n",sep="")
				bt <- bartlett.test(data[,m],clusters)		# primary test
				print(bt)
				cat("[",format(start.time,"%a %d %b %Y %H:%M:%S"),"] ....Fligner's test: \n",sep="")
				ft <- fligner.test(data[,m],clusters)		# alternative test
				print(ft)
				# record results
				out.file <- get.anova.homosked.filename(folder.data, role.meas, clust.algo, comdet.algo, n.clust=0, meas=rolemeas.names[m])
				sink(out.file)
					print(bt)
					print(ft)
				sink()
	
			# process anova: p>0.05 means we can't say one mean is significantly different
			cat("[",format(start.time,"%a %d %b %Y %H:%M:%S"),"] ..Performing ANOVA\n",sep="")
				# this is the usual anova function
				# however, it complains about the factors being unbalanced (for our data)
				# i.e.: clusters have much different sizes 
				#	options(contrasts = c("contr.helmert", "contr.poly"))
				#	av <- aov(formula= values~clusters)
			# so we use this alternate anova function, which takes the unbalance into account
			model <- lm(data[,m] ~ clusters)
			av <- anova(model)
			print(av)
			print(summary(av))
			# record the results
			out.file <- get.anova.result.filename(folder.data, role.meas, clust.algo, comdet.algo, n.clust=0, meas=rolemeas.names[m])
			sink(out.file)
				print(av)
				print(summary(av))
			sink()
		
			# check if the residuals are normally distributed
			cat("[",format(start.time,"%a %d %b %Y %H:%M:%S"),"] ..Checking the normality of residuals\n",sep="")
				# generate quantile-quantile plot (QQ-plot)
				plot.file <- get.anova.qq.filename(folder.data, role.meas, clust.algo, comdet.algo, n.clust=0, meas=rolemeas.names[m])
				cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] ....Generating qq-plot ",plot.file,"\n",sep="")
				res <- residuals(model)
				pdf(file=plot.file,bg="white")
					qqnorm(res)
					qqline(res)
				dev.off()
				# objective test
				cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] ....Testing for normality\n",sep="")
				cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] ......Note: these tests are useless for large samples, cf. http://stackoverflow.com/questions/15427692/perform-a-shapiro-wilk-normality-test\n",sep="")
				# interpretation: not normal if p<0.05 (null hypothesis rejected)
				cat("[",format(start.time,"%a %d %b %Y %H:%M:%S"),"] ....Kolmogorov-Smirnov's test: \n",sep="")
					ks <- ksnormTest(res)
					print(ks)
				cat("[",format(start.time,"%a %d %b %Y %H:%M:%S"),"] ....Shapiro-Wilk's test: \n",sep="") 
					sw <- shapiroTest(res)
					print(sw)
				# record results
				out.file <- get.anova.resnorm.filename(folder.data, role.meas, clust.algo, comdet.algo, n.clust=0, meas=rolemeas.names[m])
				sink(out.file)
					print(ks)
					print(sw)
				sink()
				
			# post-hoc tests to identify which means are differents
			# Tukey test for the aov (usual) anova function (we can't use it because of the imbalance, remember?)
			#	tk <- TukeyHSD(model)										# Tukey's test
			# Tukey test for the Anova function (it sometimes takes too much memory...)
				tk <- glht(model, linfct=mcp(clusters="Tukey"))
			#	print(tk)
			#	summary(tk)
			# ...so one can use the classic t-test instead, with a specific correction for Anova
			#	tt <- pairwise.t.test(values,clusters,p.adj="none")	# t-test without adjustment
			#	tt <- pairwise.t.test(values,clusters,p.adj="bonf")	# bonferroni adjustment
			#	tt <- pairwise.t.test(values,clusters,p.adj="holm")	# holm adjustment
			#	print(tt)
			# record result				
			out.file <- get.anova.posthoc.filename(folder.data, role.meas, clust.algo, comdet.algo, n.clust=0, meas=rolemeas.names[m])
			sink(out.file)
				print(tk)
				summary(tk)
			sink()
				
		end.time <- Sys.time();
		total.time <- end.time - start.time;
		cat("[",format(end.time,"%a %d %b %Y %H:%M:%S"),"] Process completed in ",format(total.time),"\n",sep="")
	}
}
