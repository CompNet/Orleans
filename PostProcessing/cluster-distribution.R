## Generates plots regarding the distributions of the role measures
## over the whole data and over each cluster.
##
## version: 2
## Author: Vincent Labatut 06/2013, 01/2015
##  
## setwd("~/eclipse/workspaces/Networks/Orleans/")
## setwd("C:/Eclipse/workspaces/Networks/Orleans/")
## source("PostProcessing/cluster-distribution.R")
################################################################################
#library("fBasics")
#
#
################################################################################
## Returns the standard filename for the qq-plots of the lm residuals.
##
## folder.data: folder containing all input and output files.
## role.meas: type of role measures.
## clust.algo: cluster analysis algorithm.
## comdet.algo: community detection algorithm.
## n.clust: number of detected clusters (optional).
## cluster: the considered cluster.
## meas: the role measure considered here.
################################################################################
#get.anova.histo.filename <- function(folder.data, role.meas, clust.algo, comdet.algo, n.clust=0, meas, cluster)
#{	result <- paste(folder.data,"comdet=",comdet.algo,sep="")
#	result <- paste(result,".rolemeas=",role.meas,sep="")
#	result <- paste(result,".clust=",clust.algo,sep="")
#	if(n.clust>0)
#		result <- paste(result,".k=",n.clust,sep="")
#	result <- paste(result,".meas=",meas,sep="")
#	result <- paste(result,".c=",cluster,sep="")
#	result <- paste(result,".histo",sep="")
#	result <- paste(result,".pdf",sep="")
#	return(result)
#}
#
################################################################################
################################################################################
#get.anova.qq.filename <- function(folder.data, role.meas, clust.algo, comdet.algo, n.clust=0, meas, cluster)
#{	result <- paste(folder.data,"comdet=",comdet.algo,sep="")
#	result <- paste(result,".rolemeas=",role.meas,sep="")
#	result <- paste(result,".clust=",clust.algo,sep="")
#	if(n.clust>0)
#		result <- paste(result,".k=",n.clust,sep="")
#	result <- paste(result,".meas=",meas,sep="")
#	result <- paste(result,".c=",cluster,sep="")
#	result <- paste(result,".qq",sep="")
#	result <- paste(result,".pdf",sep="")
#	return(result)
#}
#
################################################################################
################################################################################
#get.anova.homosked.filename <- function(folder.data, role.meas, clust.algo, comdet.algo, n.clust=0, meas)
#{	result <- paste(folder.data,"comdet=",comdet.algo,sep="")
#	result <- paste(result,".rolemeas=",role.meas,sep="")
#	result <- paste(result,".clust=",clust.algo,sep="")
#	if(n.clust>0)
#		result <- paste(result,".k=",n.clust,sep="")
#	result <- paste(result,".meas=",meas,sep="")
#	result <- paste(result,".homosked",sep="")
#	result <- paste(result,".txt",sep="")
#	return(result)
#}
#
################################################################################
################################################################################
#analyze.cluster.rolemeas.distrib <- function(folder.data, role.meas, clust.algo, comdet.algo)
#{	# load membership vector
#	start.time <- Sys.time();
#	cat("[",format(start.time,"%a %d %b %Y %H:%M:%S"),"] Loading membership vector\n",sep="")
#		membership.file <- get.cluster.filename(folder.data, role.meas, clust.algo, comdet.algo)
#		membership <- as.vector(as.matrix(read.table(membership.file)))
#		clusters <- factor(membership)
#		k <- max(membership)
#	end.time <- Sys.time();
#	total.time <- end.time - start.time;
#	cat("[",format(end.time,"%a %d %b %Y %H:%M:%S"),"] Load completed in ",format(total.time),"\n",sep="")
#	
#	# load raw data
#	start.time <- Sys.time();
#	cat("[",format(start.time,"%a %d %b %Y %H:%M:%S"),"] Loading raw data\n",sep="")
#		file.data <- get.rolemeas.filename(folder.data, role.meas, norm=FALSE, comdet.algo)
#		data <- as.matrix(read.table(file.data,header=TRUE))
#	end.time <- Sys.time();
#	total.time <- end.time - start.time;
#	cat("[",format(end.time,"%a %d %b %Y %H:%M:%S"),"] Load completed in ",format(total.time),"\n",sep="")
#	
#	# process each measure
#	rolemeas.names <- get.rolemeas.names(role.meas)
#	for(m in 1:length(rolemeas.names))
#	{	cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] Processing role measure ",rolemeas.names[m],"\n",sep="")
#		
#		# test for homogeneity of variances (homoskedasticity)
#		# p>0.05 means the null hypothesis could not be rejected
#		cat("[",format(start.time,"%a %d %b %Y %H:%M:%S"),"] ..Testing homogeneity of variances (homoskedasticity)\n",sep="")
#			cat("[",format(start.time,"%a %d %b %Y %H:%M:%S"),"] ....Barlett's test: \n",sep="")
#			bt <- bartlett.test(data[,m],clusters)		# primary test
#			print(bt)
#			cat("[",format(start.time,"%a %d %b %Y %H:%M:%S"),"] ....Fligner's test: \n",sep="")
#			ft <- fligner.test(data[,m],clusters)		# alternative test
#			print(ft)
#			# record results
#			out.file <- get.anova.homosked.filename(folder.data, role.meas, clust.algo, comdet.algo, n.clust=0, meas=rolemeas.names[m])
#			sink(out.file)
#				print(bt)
#				print(summary(ft))
#			sink()
#
#		# test each cluster
#		for(c in 1:k)
#		{	cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] ..Processing cluster ",c-1,"\n",sep="")
#			idx <- which(membership==(c-1))
#		
#			# plot histogram
#			plot.file <- get.anova.histo.filename(folder.data, role.meas, clust.algo, comdet.algo, n.clust=0, meas=rolemeas.names[m], cluster=c)
#			cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] ....Generating histogram ",plot.file,"\n",sep="")
#			pdf(file=plot.file,bg="white")
#				h <- hist(data[idx,m],
#						#breaks=20,
#						freq=FALSE)
#				h$counts <- h$counts/sum(h$counts)
#				plot(h,col="RED",main=NULL,xlab=rolemeas.names[m],ylab="Frequency")
#			dev.off()
#			
#			# generate quantile-quantile plot (QQ-plot)
#			plot.file <- get.anova.qq.filename(folder.data, role.meas, clust.algo, comdet.algo, n.clust=0, meas=rolemeas.names[m], cluster=c)
#			cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] ....Generating qq-plot ",plot.file,"\n",sep="")
#			pdf(file=plot.file,bg="white")
#				qqnorm(data[idx,m])
#				qqline(data[idx,m])
#			dev.off()
#			
#			# test for normality
#			cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] ....Testing for normality\n",sep="")
#			cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] ......Note: these tests are useless for large samples, cf. http://stackoverflow.com/questions/15427692/perform-a-shapiro-wilk-normality-test\n",sep="")
#			# interpretation: not normal if p<0.05 (null hypothesis rejected)
#			res <- ksnormTest(data[idx,m])
#			print(res)
#			res <- shapiroTest(data[idx,m])
#			print(res)
#			
#		}
#	}
#}
#
#
