# TODO: Add comment
# 
# Author: Ben
###############################################################################

require(parallel) || install.packages("parallel")
source("basic-mcmc/core.R")

# Compare performance of 1 projection, no optimization
compare.no.optimization = function(R = 100) {
	cl = makeCluster(detectCores() - 1)
	clusterEvalQ(cl, source("basic-mcmc/core.R"))
	
	results = parSapply(cl, 1:R, function(i) {
				data = basic.model()$train
				m = generate_random_matrix()
				return(run_classifier(project(data, m)))
			})
	
	stopCluster(cl)
	
	cat(paste0("No optimization\nMean: ", mean(results),
		" pm ", sd(results)/sqrt(R), "\n\n"))
}

# Compare performance of 1 projection, filtered out of n_filter
compare.filter = function(R = 100, n_filter = 10000) {
	cl = makeCluster(detectCores() - 1)
	clusterEvalQ(cl, source("basic-mcmc/core.R"))

	results = parSapply(cl, 1:R, function(i) {
				data = basic.model()$train
				err = replicate(n_filter,
						run_classifier(project(data, generate_random_matrix())))
				return(min(err))
			})

	stopCluster(cl)

	cat(paste0("Filtering optimization\nMean: ", mean(results),
			" pm ", sd(results)/sqrt(R), "\n\n"))
}


# Compare performance of 1 projection, MCMC optimization
compare.mcmc = function(R = 100) {
	cl = makeCluster(detectCores() - 1)
	clusterEvalQ(cl, source("basic-mcmc/core.R"))
	
	results = parSapply(cl, 1:R, function(i) {
				data = basic.model()$train
				m = mcmc_optimize(data)
				return(run_classifier(project(data, m)))
			})
	
	stopCluster(cl)
	
	cat(paste0("MCMC\nMean: ", mean(results),
		" pm ", sd(results)/sqrt(R), "\n\n"))
}