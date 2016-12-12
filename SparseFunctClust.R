GetWCSS <- function(x, Cs, ws=NULL){
  wcss.perfeature <- numeric(ncol(x))
  for(k in unique(Cs)){
    whichers <- (Cs==k)
    if(sum(whichers)>1) wcss.perfeature <- wcss.perfeature + apply(scale(x[whichers,],center=TRUE, scale=FALSE)^2, 2, sum)
  }
  bcss.perfeature <- apply(scale(x, center=TRUE, scale=FALSE)^2, 2, sum)-wcss.perfeature
  if(!is.null(ws)) return(list(wcss.perfeature=wcss.perfeature, wcss=sum(wcss.perfeature), wcss.ws=sum(wcss.perfeature*ws),
                               bcss.perfeature=bcss.perfeature))
  if(is.null(ws)) return(list(wcss.perfeature=wcss.perfeature, wcss=sum(wcss.perfeature), bcss.perfeature=bcss.perfeature))
}

GetOptimalW <- function(b, c_star){
	# b is the function b(x)
	# c_star is the parameter c* to decide what component is to be set zero
	b_star <- b
	b_star[which(b <= c_star)] <- 0
	norm_b_star <- sqrt(sum((b_star)^2))
	w <- (1/norm_b_star)*b_star
	return(w)
	}
	
GetOptimalClusters <- function(data, K, w, method){
	# data is the matrix representing the functions
	# K is the number of clusters and w the function w(x)
	qualim=c('kmea','pam','hier')
	qualem <- pmatch(method,qualim)
	weighted_data <- scale(data,center=FALSE,scale=1/w)
	switch(qualem,{
	 km <- kmeans(weighted_data, K)$cluster
	},{
	 km <- pam(weighted_data, K)$cluster
	},{
	 km <- cutree(hclust(dist(weighted_data)),K)
	})
	return(km)
	}
	
FKMSparseClustering <- function(data, x, K, m, method=c('kmea','pam','hier'), maxiter = 50){
	# data is the matrix representing the functions
	# K is the number of clusters
	# x the common domain of the functions
	# m is the sparsity parameter
	# method is the chosen clustering method
	# maxiter is the maximum number of iteration (50 default)
	mu <- x[length(x)] - x[1]
	if(m > mu){
		stop("m has to be less than the measure of the domain")
		}
	qualim=c('kmea','pam','hier')
	qualem <- pmatch(method,qualim)
	w_old <- rep(1, length(x)) 
	switch(qualem,{
	 k_old <- kmeans(data, K,iter.max=maxiter)$cluster
	},{
	 k_old <- pam(data, K)$cluster
	},{
	 k_old <- cutree(hclust(dist(data)),K)
	})
	b_old <- GetWCSS(data, k_old)$bcss.perfeature
	perc <- m/mu
	b_ord <- sort(b_old)
	c_star <- b_ord[ceiling(length(b_ord)*perc)]
	niter <- 1
	w <- rep(0, length(x)) 
	k <- rep(0, length(k_old))
	b <- rep(0, length(x))
	cluster_difference <- sum(abs(k_old - k))
	epsilon <- 1e-6
	w_difference <- sqrt(sum((w - w_old)^2))
	out <- list()
	while(w_difference >= epsilon && cluster_difference > 0 && niter < maxiter){
		niter <- niter + 1
		w_old <- w
		k_old <- k
		w <- GetOptimalW(b_old, c_star) 
		k <- GetOptimalClusters(data, K, w, method)
		b <- GetWCSS(data, k)$bcss.perfeature
		b_old <- b
		b_ord <- sort(b_old)
		c_star <- b_ord[ceiling(length(b_ord)*perc)]
		}
	obj <- sum(w*b)
	out <- list(W = w, CLUSTER = k, OBJ=obj, ITERATION = niter)
	return(out)
	}


FKMSparseClustering.permute <- function(data, x, K, mbound = NULL, method=c('kmea','pam','hier'), nperm = 20, maxiter = 50){
	# data is the matrix representing the functions
	# K is the number of clusters
	# x the common domain of the functions
	# mbound is the sparsity parameter bound
	# method is the chosen clustering method
	# nperm is the number of permutations for the gap statistics (20 default)
	# maxiter is the maximum number of iteration (50 default)
	mu <- x[length(x)] - x[1]
	n <- dim(data)[1]
	p <- dim(data)[2]
	if(length(mbound)>0){if(mbound > mu){
		stop("m has to be less than the measure of the domain")
		}
	}else{
		mbound <- .5*mu
	}
	qualim <- seq(2*min(diff(x)),mbound,len=.1*length(x))
	GAP <- numeric(length(qualim))
	iter <- 1
	for(m in qualim){
		resTRUE <- FKMSparseClustering(data, x, K, m, method)$OBJ
		resPERM <- NULL
		for(perm in 1:nperm){
			dataperm <- data
			for(j in 1:p)dataperm[,j] <- data[sample(1:n),j]
			resPERM <- c(resPERM, FKMSparseClustering(dataperm, x, K, m, method)$OBJ)
		}
		GAP[iter] <- log(resTRUE) - mean(log(resPERM))
		iter <- iter + 1
	}
	return(list(GAP = max(GAP), m = qualim[which.max(GAP)]))
	}
######################################################################################
plotGroupMeans <- function(df, Cs)
{
  cluster <- unique(Cs)
  M <- matrix(0, nrow= length(cluster), ncol = 24)
  
  
  for(k in 1:length(cluster))
  {
    M[k,] <- as.matrix(colMeans(df[which(Cs == k),]))
  }
  matplot(t(M), type = "l", lwd = 2)
}
#####################################################################################
GetGroupVariance <- function(df, Cs)
{
  cluster <- unique(Cs)
  M <- matrix(0, nrow= length(cluster), ncol = 24)
  
  for(k in 1:length(cluster))
  {
    M[k,] <- as.matrix(apply(df[which(Cs == k),], 2, var))
  }
#  matplot(t(M), type = "l", lwd = 2)
  return(M)
}
	
	
	
	
	