# Author: Tsunenori Ishioka
#
# Taken from http://www.rd.dnc.ac.jp/~tunenori/xmeans_e.html
#
# Related papers: 
#	- Extended k-means with an efficient estimation of the number of clusters
#	  http://www.rd.dnc.ac.jp/~tunenori/doc/xmeans_ideal2000.pdf
#	- X-means- Extending k-means with efficient estimation of the number of clusters
#	  http://www.cs.cmu.edu/~dpelleg/download/xmeans.pdf
###############################################################################

###############################################################################
# Adapter function allowing to use xmeans from our own scripts.
#
# folder.data: folder containing all input and output files.
# role.meas: type of role measures.
# clust.algo: cluster analysis method.
# comdet.algo: community detection algorithm.
###############################################################################
apply.xmeans <- function(folder.data, role.meas, clust.algo, comdet.algo)
{	# load the normalized data
	in.file <- get.rolemeas.filename(folder.data,role.meas,norm=TRUE,comdet.algo)
	data <- as.matrix(read.table(in.file))
	
	# apply x-means	
	temp <- xmeans(x=data, ik=2, iter.max=15, pr.proc=TRUE, ignore.covar=TRUE, merge.cls=FALSE)
	membership <- temp$cluster
		
	# record result
	out.file <- get.cluster.filename(folder.data,role.meas,0,clust.algo,comdet.algo)
	write.table(x=membership, file=out.file, row.names=FALSE, col.names=FALSE)
}


# $Id: xmeans.prog,v 1.23 2012/04/12 11:21:12 tunenori Exp tunenori $
#
# X-MEANS Clustering
#
# Description:
#
#      Perform x-means non-hierarchal clustering on a data matrix.
# 
# Usage:
# 
#      xmeans(x, ik = 2, iter.max = 10, pr.proc = F, 
#		ignore.covar = T, merge.cls = F)
# 
# Arguments:
#	x: A numeric matrix of data, or an object that can be coerced to
#          such a matrix (such as a numeric vector or a data frame with
#          all numeric columns).
#
#	ik: The initial  number of clusters applied to kmeans().
#	   As xmeans calls kmeans recursively, `ik' should be sufficient
#	   small.
#
# 	iter.max: The maximum of iterations allowed.
#
#	pr.proc: logical: If 'TRUE' the system outputs the processing status.
#
# 	ignore.covar: logical: If 'TRUE', covariances of cluster data are
#	   ignored. For saving of the time, 'TRUE' is set as the defalut.
#
#	merge.cls: logical: If 'TRUE', some clusters may be merged into another
#	   clusters after iterative division. 
#
# Value:
#
#     An object of class 'xmeans' which is a list with components:
#
#	cluster: A vector of integers indicating the cluster to which each
#          point is allocated.
#
#	centers: A matrix of cluster centres.
#
#	size: The number of points in each cluster. When `merge.cls' is TRUE,
#	   some elements may be zero.
#
# References:
#
#	Ishioka, T. (2005): ``An Expansion of X-means for Automatically
#	Determining the Optimal Number of Clusters," The Fourth IASTED
#	International Conference on Computational Intelligence (CI 2005),
#	Calgary Canada, July 4-6, pp.91-96. 
#	http://www.rd.dnc.ac.jp/%7Etunenori/doc/487-053.pdf
#
#	Ishioka, T. (2000): ``Extended K-means with an Efficient Estimation
#	of the number of Clusters,'' Intelligent Data Engineering and
#	Automated Learning --- IDEAL 2000, Second International Conference,
#	Shatin, N.T., Hong Kong, China, December 2000, proceedings 17--22.
#	(Lecture Notes in Computer Science 1983, Kwong Sak Leung, Lai-Wan
#	Chan, Helen Meng (Eds.), Springer, 17--22, 2000) 
#	http://www.rd.dnc.ac.jp/%7Etunenori/doc/xmeans_ideal2000.pdf
#
# Examples:
#
#	xmeans(iris[,-5],  merge.cls=T)
#	plot(cmdscale(dist(iris[,-5])), cex=2, pch=as.numeric(iris[,5]))
#

xmeans <- function(x, ik = 2, iter.max = 10, pr.proc = F, ignore.covar = T, merge.cls = F){
	if (ik < 2) 
		ik <- 2
	x <- as.matrix(x)
	p <- ncol(x) # p-dimensional multivariate
	if (ignore.covar){
		q <- 2 * p # number of parameters; mean and var for each "p"
	}else{
		q <- p * (p+3) / 2	# integer
	}
	cl<- kmeans(x,ik,iter.max)
	cl.sub <- list()
	
	for (i in 1:ik){ # for each cluster
		y.ok <- (cl$cluster == i) 	# i-th cluster or not
		yi <- matrix(x[y.ok], ncol=p) 	# extract i-th cluster
		zi <- yi 	# save the data for graphics
		yi.centers <- cl$centers[i,]
		zi.centers <- yi.centers
		yi.cluster <- cl$cluster[(cl$cluster == i)]
		yi.cluster <- rep(1, length(yi.cluster)) 
			# sub-cluster number should begin from 1

		k1 <- 1		# cluster number
		k2 <- k1 + 1
		bic.prior <- NULL
		stack <- list()	# divided and unproceeded data are stacked
		lnL0 <- lnL(yi, yi.centers, ignore.covar)
		yi.lnL <- lnL0$lnL
		yi.detVx <- lnL0$detVx

		repeat{

		# go through at least 1 time; 
		# y$subcluster exist...
		if (pr.proc)	cat (paste("k1 =", k1, ", k2 =", k2,"\n"))
		if (nrow(yi) == 1){ # sample size is 1
			break
		}
		y <- split2cls(yi, yi.centers, q, bic.prior, lnL.prior, detVx.prior, iter.max, ignore.covar)
		if (y$continue){ # splitting continue 
		  yi.cluster <-
		   updtCrusterNum(y$continue, yi.cluster, k1, k2, y$subcluster)
		  zi.centers <-
		   updtCenters(y$continue, zi.centers, k1, k2, y$centers)
		  yi.lnL <-
		   updtlnL(y$continue, yi.lnL, k1, k2, y$lnL.post)
		  yi.detVx <-
		   updtdetVx(y$continue, yi.detVx, k1, k2, y$detVx.post)
		}
		
		if (pr.proc) print(y$subcluster)
		if (pr.proc){ print(y$bic.prior)
				print(y$bic.post)
				# print(y$lnL.prior)	# for debug
				# print(y$lnL.post)	# for debug
				print(y$continue) }
		# cat("zi.centers=\n")	# for debug
		# print(zi.centers)	# for debug
		if (!y$continue){	# no-node
			if ((nstack <- length(stack))){ # there are stacked data
				# extract the stacked data
				if (pr.proc)
				  cat(paste("extract the stacked data (", nstack, ")...\n"))
				yi <- stack[[nstack]]$data
				yi.centers <- stack[[nstack]]$centers
				bic.prior <- stack[[nstack]]$bic
				lnL.prior <- stack[[nstack]]$lnL
				detVx.prior <- stack[[nstack]]$detVx
				k1 <- stack[[nstack]]$cls
				k2 <- k2 # unchanged
				# delete the data set
				if (nstack > 1){
					stack <- stack[1:(nstack-1)]
				}else{
					stack <- list() # no stacked data
				}
				next;
			}
			# no node and no stack
			if (pr.proc)	cat ("no node and no stack...\n")
			break;
		}
		# splitting continues...
		y1 <- y$clj1	# data
		y2 <- y$clj2
		yi.ctr1 <- y$centers[1,]	# centers
		yi.ctr2 <- y$centers[2,]
		bic.prior1 <- y$bic.post[1]	# bic
		bic.prior2 <- y$bic.post[2]
		lnL.prior1 <- y$lnL.post[1]	# lnL
		lnL.prior2 <- y$lnL.post[2]
		detVx.prior1 <- y$detVx.post[1]	# detVx
		detVx.prior2 <- y$detVx.post[2]

		# one-hand repeats recursively...
		yi <- y1
		yi.centers <- yi.ctr1
		bic.prior <- bic.prior1
		lnL.prior <- lnL.prior1
		detVx.prior <- detVx.prior1
		# other-hand is stacked... 
		if (pr.proc)	cat ("stacking ...\n")
		stack <- c(stack,
		  list(list(data=y2, centers=yi.ctr2,
		   bic=bic.prior2, lnL=lnL.prior2, detVx=detVx.prior2, cls=k2)))
		# inclement the cluster number 
		k2 <- k2 + 1

		} # end of repeat

		# splitting done ...
		if (pr.proc){
			cat ("splitting done...\n")
			cat (paste("main cluster =",i,"*******\n"))
		}
		cl.sub <- c(cl.sub, list(list(cluster = yi.cluster,
			 centers = zi.centers, lnL = yi.lnL, detVx = yi.detVx,
			 size = tabulate(yi.cluster))))
		if (pr.proc){
			print(cl.sub[[i]])
			plot(zi, col=yi.cluster)
			if (is.vector(zi.centers))
			  points(zi.centers[1], zi.centers[2], pch=8)
			else # array
			  points(zi.centers,col=1:(length(zi.centers)/p),pch=8)
		}
	}
	if (pr.proc)	print(cl.sub)
	xcl <- mergeResult(cl, cl.sub, ik)

	if (merge.cls == F) {
		return(list(cluster = xcl$cluster, centers = xcl$centers, size = xcl$size))
	}

	# merge after progressive dividing
	#
	if (pr.proc) cat("merging after progressive dividing ...\n")

	k <- length(xcl$size)	# final cluster number
	if (k <= 2){	# minimum cluster number should be 2
		if (pr.proc) cat("merging skipped ...\n")
		return(list(cluster = xcl$cluster, centers = xcl$centers, size = xcl$size))
	}
	if (pr.proc){
		cat("xcl$detVx=")
		print(xcl$detVx)
		cat("xcl$size=")
		print(xcl$size)
	}
	
	klist <- sort.list(xcl$size) # "small" to "large" order of xcl$detVx list
	if (pr.proc) print(klist)
	for (i in 1:(k-1)){ 
	    for (j in (i+1):k){ 
		k1 = klist[i]
		k2 = klist[j]
		if (pr.proc) cat(paste("inspecting the clusters", k1,"and", k2,"\n"))

		z <- mergedBIC(x, xcl, k1, k2, q, ignore.covar, pr.proc)
		if (z$ret == F){
			# k1 or k2 has been merged.
			# skip this roop
			if (pr.proc) cat("skipping... k1=", k1, "k2=", k2,"\n")
			next
		}
		if (z$bicdiv > z$bicmgd){
			# we prefer merged model.
			# replace larger cls. number to smaller cls. number
			if (pr.proc) cat("replace cls.", k2, "to", k1,"\n")
			xcl$cluster <- replace(xcl$cluster, (xcl$cluster == k2), k1)
			xcl$size[k1] <- xcl$size[k1] + xcl$size[k2]
			xcl$size[k2] <- 0
			xcl$lnL[k1] <- z$lnLmgd
			xcl$lnL[k2] <- 0
			xcl$detVx[k1] <- z$detVxmgd
			xcl$detVx[k2] <- 0
			xcl$centers[k1,] <- z$ctrmgd
			xcl$centers[k2,] <- 0

		}
	    }
	}
	list(cluster = xcl$cluster, centers = xcl$centers, size = xcl$size)
}




# marge the result of sub-clustering;
# cluster numbers by first kmeans should be renumbered;
# the other centers and sizes are simply added.
# cl: the result of first kmeans
# cl.sub: the result of subclustering
# ik: cluster number adopted to kmeans.
mergeResult <- function(cl, cl.sub, ik){
	cluster <- cl$cluster	# main cluster
	centers <- NULL
	size <- NULL
	lnL <- NULL
	detVx <- NULL

	k <- 0	# uniq cluster numbers; k should be decremental. 
	for (i in 1:ik)
		k <- k + length(cl.sub[[i]]$size)
	kk <- k

	for (i in ik:1){	# loop for main clusters obtained by kmeans
		xsub <- cl.sub[[i]]$cluster
		iki <- ik -i +1 
		centers <- rbind(centers, cl.sub[[iki]]$centers)
		size <- c(size, cl.sub[[iki]]$size)
		lnL <- c(lnL, cl.sub[[iki]]$lnL)
		detVx <- c(detVx, cl.sub[[iki]]$detVx)
		
		for (j in length(cl.sub[[i]]$size):1){ # loop for subclusters
			xsub <- replace(xsub, (xsub == j), k)
			k <- k -1
		}
		cluster <- replace(cluster, (cluster == i), xsub)
	}
	if (k != 0) stop("mergeResult: assertion failed (k = 0)...")
	dimnames(centers) <- list(1:kk, NULL)
	list(cluster = cluster, centers = centers, lnL = lnL, detVx = detVx, size = size)
}


# update the cluster number by using the result of "split2cls()"
# continue: no splitting
# v: cluster numbers vector for initial cluster.
# k1: cluster numbers should be updated; "k1" becomes "k1" and "k2"
# xsub: sub-cluster numbers vector of "v" whose value is "k1";
#	given "xsub" have 1 or 2.
updtCrusterNum <- function(continue, v, k1, k2, xsub){
	if (!is.vector(v)) 
		return(xsub)
	if (!continue)
		return(v)
	if (k1 == k2)
		stop("updtCrusterNum() : k1 and k2 should differ.")

	# below is same algorithm; explicit array operation is slow in R.
	# j <- 1
	# for (i in 1:length(v)){
 	#	if (v[i] == k1){
	#		if (xsub[j] == 2)
	#			v[i] <- k2
	#		j <- j + 1
	#	}
	# }
	# end of algorithm
	xsub <- replace(xsub, (xsub == 2), k2) # changed
	xsub <- replace(xsub, (xsub == 1), k1) # unchanged
	v <- replace(v, (v == k1), xsub)
}


# update the cluster centers by using the result of "split2cls()"
# continue: no update
# org.centers: original centers matrix
# divided.centers: divided centers matrix; it has 2 rows.
updtCenters <- function(continue, org.centers, k1, k2, divided.centers){
	if (!is.matrix(org.centers)) 
		return(divided.centers)
	if (!continue)
		return(org.centers)
	if (k1 == k2)
		stop("updtCenters() : k1 and k2 should differ.")
	
	z <- NULL
	for (i in 1:max(k2, nrow(org.centers))){
		if (i == k1)
			z <- rbind(z, divided.centers[1,])
		else if (i == k2)
			z <- rbind(z, divided.centers[2,])
		else
			z <- rbind(z, org.centers[i,])
	}
	z
}

# update the lnL by using the result of "split2cls()"
# continue: no update
# org.lnL: original lnL vector
# divided.lnL: divided lnL vector having 2 elements.
updtlnL <- function(continue, org.lnL, k1, k2, divided.lnL){
        if (!is.vector(org.lnL))
                return(divided.lnL)
        if (!continue)
                return(org.lnL)
        if (k1 == k2)
                stop("updtlnL() : k1 and k2 should differ.")

        z <- NULL
        for (i in 1:max(k2, length(org.lnL))){
                if (i == k1)
                        z <- c(z, divided.lnL[1])
                else if (i == k2)
                        z <- c(z, divided.lnL[2])
                else
                        z <- c(z, org.lnL[i])
        }
        z
}

# update the detVx by using the result of "split2cls()"
# continue: no update
# org.detVx: original detVx vector
# divided.detVx: divided detVx vector having 2 elements.
updtdetVx <- function(continue, org.detVx, k1, k2, divided.detVx){
        if (!is.vector(org.detVx))
                return(divided.detVx)
        if (!continue)
                return(org.detVx)
        if (k1 == k2)
                stop("updtdetVx() : k1 and k2 should differ.")

        z <- NULL
        for (i in 1:max(k2, length(org.detVx))){
                if (i == k1)
                        z <- c(z, divided.detVx[1])
                else if (i == k2)
                        z <- c(z, divided.detVx[2])
                else
                        z <- c(z, org.detVx[i])
        }
        z
}

# split 2 clusters if we would prefer it based on BIC
# q: a number of parameters
# bic.prior: BIC which x is given; if bic.prior=NULL then we calculate
# lnL.prior: lnL which x is given; if bic.prior=NULL then we calculate
# detVx.prior: detVx which x is given; if bic.prior=NULL then we calculate
split2cls <- function(x, centers, q, bic.prior, lnL.prior, detVx.prior, iter.max, ignore.covar){
	if (is.null(bic.prior)){
		pb <- priorBIC(x, centers, q, ignore.covar)
		bic.prior <- pb$bic
		lnL.prior <- pb$lnL
		detVx.prior <- pb$detVx
	}
	bic.post <- postBICs(x, centers, q, iter.max, ignore.covar)

	subcluster <- bic.post$clsub$cluster
	#
	# compare whether if we should split
	if (is.na(bic.post$bic[3])){
		# BIC may has NA because of few data 
		continue <- FALSE
	}else if (bic.post$bic[3] < bic.prior){
		# splitting ...
		# replace the cluster number to cl$cluster
		continue <- TRUE
	}else{
		# not splitting...
		# return "subcluster" stored k1 
		continue <- FALSE
	}
	# note that "subcluster" gives 1 or 2 
	list(continue = continue, subcluster = subcluster, 
		bic.prior = bic.prior, bic.post = bic.post$bic,
		lnL.prior = lnL.prior, lnL.post = bic.post$lnL,
		detVx.prior = detVx.prior, detVx.post = bic.post$detVx,
		centers = bic.post$clsub$centers,
		clj1 = bic.post$clj1, clj2 = bic.post$clj2)
}




# return BIC (prior BIC)
priorBIC <- function(x, centers, q, ignore.covar){
	lnL0 <- lnL(x, centers, ignore.covar)
	bic <- -2 * lnL0$lnL + q * log(nrow(x)) # BIC
	# bic <- -2 * lnL0$lnL + q  # AIC
	list(lnL = lnL0$lnL, detVx = lnL0$detVx, bic = bic)
}


# return BICs (two posterior BICs)
postBICs <- function(x, centers, q, iter.max, ignore.covar){
	#
	# split to 2 clusters
	clsub <- kmeans(x, 2, iter.max)
	y.ok1 <- lapply(clsub$cluster, "==", 1) # 1st sub-cluster or not
	y.ok2 <- lapply(clsub$cluster, "==", 2) # 2nd sub-cluster or not
	# extract sub data
	p <- ncol(x)
	clj1 <- matrix(x[as.logical(y.ok1)], ncol=p)
	clj2 <- matrix(x[as.logical(y.ok2)], ncol=p)
	# ratio for pdf.
	r1 <- clsub$size[1] / sum(clsub$size)	# [0,1]
	r2 <- 1 - r1 	# [0,1]
	# two later BICs
	# print(clsub$centers[1,])	# for debug
	# print(apply(clj1,2,mean))	# for debug
	# print(sqrt(apply(clj1,2,var)))	# for debug
	# print(r1)	# for debug
	lnL1 <-  lnL(clj1, clsub$centers[1,], ignore.covar)
	# print(clsub$centers[2,])	# for debug
	# print(apply(clj2,2,mean))	# for debug
	# print(sqrt(apply(clj2,2,var)))	# for debug
	# print(r2)	# for debug
	lnL2 <-  lnL(clj2, clsub$centers[2,], ignore.covar)
	n1 <- nrow(clj1)
	n2 <- nrow(clj2)
	# normalizing factor; dist() is in library(mva)
	if (is.na(lnL1$detVx) || is.na(lnL2$detVx))
		beta <- 0
	else
		beta <- dist(clsub$center) / (sqrt(lnL1$detVx + lnL2$detVx))
	alpha <- 0.5 / pnorm(beta)
	BIC1 <- -2 * lnL1$lnL +q * log(n1)
	BIC2 <- -2 * lnL2$lnL +q * log(n2) 
	# BIC1 <- -2 * lnL1$lnL +q # AIC
        # BIC2 <- -2 * lnL2$lnL +q # AIC

	# cat (paste("alpha =",alpha,"\n"))	# for debug
	# cat (paste("beta =",beta,"\n"))	# for debug
	
	# BIC is not (BIC1 + BIC2)
	BIC <- -2 * lnL1$lnL  -2 * lnL2$lnL + 2 * q * log(n1 + n2) - 2 * (n1 + n2) * log(alpha)
	# BIC <- -2 * lnL1$lnL  -2 * lnL2$lnL + 2 * q  - 2 * (n1 + n2) * log(alpha) # AIC
	list(bic = c(BIC1, BIC2, BIC), 
		lnL = c(lnL1$lnL, lnL2$lnL),
		detVx = c(lnL1$detVx, lnL2$detVx),
		clsub = clsub, clj1 = clj1, clj2 = clj2)
}



# return BICs for Two-merged clusters model and devided clusters model
# k1/k2: marged cluster ID
mergedBIC <- function(x, xcl, k1, k2, q, ignore.covar, pr.proc){
	# sample size
	# check for input data
	n1 <- xcl$size[k1]
	n2 <- xcl$size[k2]
	if (n1 == 0 || n2 == 0){
		# already had been merged
		cat(paste("already had been merged\n"))
		ret <- F
        	return( list (ret = ret))
	}
	if (is.null(xcl$lnL[k1]) || is.null(xcl$lnL[k2])){
		# lnL may be null because of few data
		cat(paste("lnL may be null because of few data\n"))
		ret <- F
        	return( list (ret = ret))
	}

	# divided clusters model
	lnL1 = xcl$lnL[k1]
	lnL2 = xcl$lnL[k2]
	ctrextrt <- rbind(xcl$centers[k1,], xcl$centers[k2,])
	beta <- dist(ctrextrt) / (sqrt(xcl$detVx[k1] + xcl$detVx[k2]))
	if (pr.proc) cat(paste("beta=", round (beta, digit=2), "\n"))

	# if (beta > 10){
	# 	# 2 clusters far apart
	# 	ret <- F
        # 	return( list (ret = ret))
	# }

	alpha <- 0.5 / as.numeric(pnorm(beta))
	bicdiv <- -2 * lnL1  -2 * lnL2 + 2 * q * log(n1 + n2) - 2 * (n1 + n2) * log(alpha)
        # bicdiv <- -2 * lnL1 -2 * lnL2 + 2 * q - 2 * (n1 + n2) * log(alpha) # AIC

	# extract 2 clusters data
	y.ok1 <- lapply(xcl$cluster, "==", k1) # 1st sub-cluster or not
	y.ok2 <- lapply(xcl$cluster, "==", k2) # 2nd sub-cluster or not

	# extract sub data
	p = ncol(x)
	clj1 <- matrix(x[as.logical(y.ok1)], ncol=p)
	clj2 <- matrix(x[as.logical(y.ok2)], ncol=p)
	xmgd <- rbind(clj1, clj2)

	# merged cluster center
	ctrmgd <- (n1 * xcl$centers[k1,] + n2 * xcl$centers[k2,]) / (n1 + n2)
	lnLmgd <- lnL(xmgd, ctrmgd, ignore.covar)
	bicmgd <- -2 * lnLmgd$lnL + q * log(nrow(xmgd)) # BIC
	# bicmgd <- -2 * lnLmgd$lnL + q  # AIC

	ret <- T
	list (ret = ret, ctrmgd = ctrmgd, lnLmgd = lnLmgd$lnL, detVxmgd = lnLmgd$detVx, bicmgd = bicmgd, bicdiv = bicdiv)
}





# log-likelihood under the assumption of 
# p-dimensional multivariate normal distribution.
# ignore.covar: ignore the covariance 
lnL <- function(x, centers, ignore.covar=T){
	x <- as.matrix(x)
	p <- ncol(x)	# p-dimensional multivariate
	n <- nrow(x)	# sample size
	if (missing(centers)) 
		stop("centers must be a number or a matrix")
	if (n <= 2)	# few data
		return(list(lnL=NA, detVx=NA))
	vx <- var(x)	# var-co.var matrix
	# print(x)	# for debug
	if (p == 1){ # x is vector 
		invVx <- 1 / as.vector(vx)
		detVx <- as.vector(vx)
	}else{ 
	  if (ignore.covar){
		invVx <- diag(1/diag(vx)) # inv. matrix when assuming diag.  
		detVx <- prod(diag(vx)) # det. when assuming diag. 
	  }else{
		invVx <- solve(vx) # inverse matrix of "vx"
		y <- chol(vx) # Cholesky decomposition
		detVx <- prod(diag(y)) # vx = t(y) %*% y, where y is triangular,
					  # then, det(vx) = det(t(y)) * det(y)
	  }
	}
	t1 <- -p/2 * 1.837877066 # 1.837... = log(2 * 3.1415...)
	t2 <- -log(detVx) / 2
	xmu <- t(apply(x, 1, "-", centers))
	# print(centers)	# for debug
	# print(xmu)	# for debug
	# s <- 0
	# for (i in 1:n)
	#	s <- s + t(xmu[i,]) %*% invVx %*% xmu[i,]
	if (p == 1){
		s <- sum(xmu^2 * invVx)
	}else{
		s <- sum(apply(xmu, 1, txInvVxX, invVx=invVx))
	}
	t3 <- -s / 2
	ll <- (t1 + t2) * n + as.numeric(t3)	# log likelihood
	list(lnL=ll, detVx=detVx)
}

# function for calculation of 
# t(xmu[i,]) %*% invVx %*% xmu[i,]
txInvVxX <- function(x, invVx){
	t(x) %*% invVx %*% x
}




# res <- xmeans(x=data)
# plot(x=data, col=res$cluster); points(res$centers, col=1:(length(ks)+1), pch = 8)
