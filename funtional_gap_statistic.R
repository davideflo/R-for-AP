# An implementation of the (FUNCTIONAL) gap statistic algorithm from Tibshirani, Walther, and Hastie's "Estimating the number of clusters in a data set via the gap statistic".

library(plyr)
library(ggplot2)
source("SparseFunctClust.R")

# Given a matrix `data`, where rows are observations and columns are individual dimensions, compute and plot the gap statistic (according to a uniform reference distribution).
fun_gap_statistic = function(data, x, m, min_num_clusters = 2, max_num_clusters = 10, num_reference_bootstraps = 10) {
  num_clusters = min_num_clusters:(max_num_clusters)
  actual_dispersions = maply(num_clusters, function(n) dispersion(data, x, m, n))
  mean_ref_dispersions <- stddev_ref_dispersions <- c()
  for(n in num_clusters)
  {
    ref_dispersions = fun_reference_dispersion(data, n, num_reference_bootstraps)
    mean_ref_dispersions <- c(mean_ref_dispersions, ref_dispersions[1])
    stddev_ref_dispersions <- c(stddev_ref_dispersions, ref_dispersions[2])
  }
  
  
  gaps = mean_ref_dispersions - actual_dispersions
  
  print(plot_gap_statistic(gaps, stddev_ref_dispersions, num_clusters))
  
  print(paste("The estimated number of clusters is ", num_clusters[which.max(gaps)], ".", sep = ""))
  
  list(gaps = gaps, gap_stddevs = stddev_ref_dispersions)
}

# Plot the gaps along with error bars.
plot_gap_statistic = function(gaps, stddevs, num_clusters) {
  qplot(num_clusters, gaps, xlab = "# clusters", ylab = "gap", geom = "line", main = "Estimating the number of clusters via the gap statistic") + geom_errorbar(aes(num_clusters, ymin = gaps - stddevs, ymax = gaps + stddevs), size = 0.3, width = 0.2, colour = "darkblue")
}

# Calculate log(sum_i(within-cluster_i sum of squares around cluster_i mean)).
dispersion = function(data, x, m, num_clusters) {
  # R's k-means algorithm doesn't work when there is only one cluster.
  if (num_clusters == 1) {
    cluster_mean = aaply(data, 2, mean)
    distances_from_mean = aaply((data - cluster_mean)^2, 1, sum)
    log(sum(distances_from_mean))
  } else {	
    # Run the k-means algorithm `nstart` times. Each run uses at most `iter.max` iterations.
    k = FKMSparseClustering(data, x, K = num_clusters, m, method = "kmea")
    wcss <- GetWCSS(data, k$CLUSTER, k$W)
    # Take the sum, over each cluster, of the within-cluster sum of squares around the cluster mean. Then take the log. This is `W_k` in TWH's notation.
    log(wcss$wcss)
  }
}

# For an appropriate reference distribution (in this case, uniform points in the same range as `data`), simulate the mean and standard deviation of the dispersion.
reference_dispersion = function(data, x, m, num_clusters, num_clusters2, num_reference_bootstraps) {
  dispersions = maply(1:num_reference_bootstraps, function(i) dispersion(generate_uniform_points(data, x, num_clusters2), x, m, num_clusters))
  mean_dispersion = mean(dispersions)
  stddev_dispersion = sd(dispersions) / sqrt(1 + 1 / num_reference_bootstraps) # the extra factor accounts for simulation error
  c(mean_dispersion, stddev_dispersion)
}

# Generate uniform points within the range of `data`.
generate_uniform_points = function(data, x, K) {
  # Find the min/max values in each dimension, so that we can generate uniform numbers in these ranges.
  #mins = aaply(data, 2, min)
  #maxs = apply(data, 2, max)
  mins <- min(data)
  maxs <- max(data)
  num_datapoints = K
  up <- matrix(0, nrow = K, ncol = length(x))
  # For each dimension, generate `num_datapoints` points uniformly in the min/max range.
  for(i in 1:K) up[i,] <- runif(length(x), min = mins, max = maxs)
  return(up)
}
# My own reference dispersion on the base of the heuristics thought about randomly assigning sampled functions to random data
fun_reference_dispersion <- function(data, num_clusters, num_reference_bootstraps)
{
  n <- nrow(data)
  sam <- floor(n/num_clusters)
  dispersion <- c()
  for(i in 1:num_reference_bootstraps)
  {
    sample_list <- split(data, sample(1:num_clusters))
    disp <- c()
    for(j in 1:num_clusters)
    {
      disp <- c(disp,apply(scale(sample_list[[j]],center=TRUE, scale=FALSE)^2, 2, sum))
      print(disp)
    }
    dispersion <- c(dispersion,sum(disp))
  }
  mean_dispersion = mean(dispersion)
  print(dispersion)
  stddev_dispersion = sd(dispersion) / sqrt(1 + 1 / num_reference_bootstraps)
  return(c(mean_dispersion, stddev_dispersion))
}

