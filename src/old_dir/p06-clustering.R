# Applies the disytibuted k-means algorithm
# and process the Davies-Bouldin measure for
# the specified interval of k. Of course,
# the k-means program must have been unzipped
# and compiled before.
#
# distributed k-means available at:
# http://users.eecs.northwestern.edu/~wkliao/Kmeans/
#
# version: 1
# Author: Vincent Labatut 06/2013
#
# setwd("~/eclipse/workspaces/Networks/Orleans/")
# setwd("C:/Eclipse/workspaces/Networks/Orleans/")
# source("src/old_dir/p06-clustering.R")
###############################################################################
library("clusterSim")


###############################################################################
# setup files
###############################################################################
folder.data <- "data/"	
file.kmeans <- "~/Downloads/Simple_Kmeans/omp_main"
ks <- c(2:15)


###############################################################################
# load normalized data
###############################################################################
start.time <- Sys.time();
cat("[",format(start.time,"%a %d %b %Y %H:%M:%S"),"] Loading normalized data\n",sep="")
	file.data <- paste(folder.data,"rolemeasures.normalized.txt",sep="")
	data <- as.matrix(read.table(file.data))
end.time <- Sys.time();
total.time <- end.time - start.time;
cat("[",format(end.time,"%a %d %b %Y %H:%M:%S"),"] Load completed in ",total.time,"\n",sep="")


###############################################################################
# apply algorithm for each specified k
###############################################################################
quality <- matrix(NA,ncol=2,nrow=length(ks))
quality[,1] <- ks
for(i in 1:length(ks))
{	# apply k-means
	k <- ks[i]
	cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] ..Processing k=",k,"\n",sep="")
	
	start.time <- Sys.time();
	cat("[",format(start.time,"%a %d %b %Y %H:%M:%S"),"] ....Applying k-means for k=",k,"\n",sep="")
		# define command
		kmeans.command <- paste(file.kmeans,
			" -i ", getwd(), "/", file.data,
			" -n ", k,
			sep="")
			# ./omp_main -i ../../eclipse/workspaces/Networks/Orleans/data/normalized.numbered.txt -n 13
		# perform clustering
		cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] ....Executing command ",kmeans.command,"\n",sep="")
		system(command=kmeans.command)
		# move produced membership file (not the others, we don't care)
		file.member <- paste(folder.data,"rolemeasures.normalized.txt.membership",sep="")
		file.new <- paste(folder.data,"cluster.k",k,".txt",sep="")
		cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] ......Moving file ",file.member," to ",file.new,"\n",sep="")
		if(file.exists(file.new))
			file.remove(file.new)
		file.rename(from=file.member,to=file.new)
	end.time <- Sys.time();
	total.time <- end.time - start.time;
	cat("[",format(end.time,"%a %d %b %Y %H:%M:%S"),"] ....Process completed in ",total.time,"\n",sep="")
	
	# load membership vector
	start.time <- Sys.time();
	cat("[",format(start.time,"%a %d %b %Y %H:%M:%S"),"] ....Load membership vector (",file.new,")\n",sep="")
		membership <- as.matrix(read.table(file.new))[,2] + 1	# the clusters are numbered from zero 
	end.time <- Sys.time();
	total.time <- end.time - start.time;
	cat("[",format(end.time,"%a %d %b %Y %H:%M:%S"),"] ....Load completed in ",total.time,"\n",sep="")
	
	# process quality measure
	start.time <- Sys.time();
	cat("[",format(start.time,"%a %d %b %Y %H:%M:%S"),"] ....Process Davies-Bouldin measure for k=",k,"\n",sep="")
		db.value <- index.DB(x=data, cl=membership, centrotypes="centroids")$DB
		quality[i,2] <- db.value
	end.time <- Sys.time();
	total.time <- end.time - start.time;
	cat("[",format(end.time,"%a %d %b %Y %H:%M:%S"),"] ....Processing completed in ",total.time,", DB(",k,")=",db.value,"\n",sep="")

	gc()
	cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] ..Process completed for k=",k,"\n",sep="")
	print(quality)
}


###############################################################################
# record quality values
###############################################################################
values.file <- paste(folder.data,"clusters.quality.txt",sep="")
cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] ..Record all quality values in ",values.file,"\n",sep="")
write.table(quality,file=values.file,row.names=FALSE,col.names=FALSE)


###############################################################################
# plot quality values
###############################################################################
plot.file <- paste(folder.data,"clusters.quality.pdf",sep="")
cat("[",format(Sys.time(),"%a %d %b %Y %H:%M:%S"),"] ..Plot quality values in ",plot.file,"\n",sep="")
pdf(file=plot.file,bg="white")
plot(quality[,1],quality[,2],type="l",xlab="Clusters",ylab="Davies-Bouldin index",col="RED")
dev.off()
