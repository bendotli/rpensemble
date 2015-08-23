# TODO: Add comment
# 
# Author: Ben
###############################################################################

require(parallel) || install.packages("parallel")
source("basic-mcmc/core.R")

# Compare performance of 1 projection, no optimization
summarize.no.optimization = function(R = 100) {
	cl = makeCluster(detectCores() - 1)
	clusterEvalQ(cl, source("basic-mcmc/core.R"))
	
	results = parSapply(cl, 1:R, function(i) {
				dte = basic.model()$test
				m = generate_random_matrix()
				return(run_classifier(project(dte, m)))
			})
	
	stopCluster(cl)
	
	cat(paste0("Single-projection, no optimization\nMean: ", mean(results),
		" pm ", sd(results)/sqrt(R), "\n\n"))
}

# Compare performance of 1 projection, filtered out of B2
summarize.bruteforce = function(R = 100, B2 = 100) {
	cl = makeCluster(detectCores() - 1)
	clusterEvalQ(cl, source("basic-mcmc/core.R"))

	results = parSapply(cl, 1:R, function(i) {
				data = basic.model()
				mats = lapply(1:B2, function(...)
						return(generate_random_matrix()))
				errs = lapply(mats, function(mat)
						return(run_classifier(project(data$train, mat))))
				mat = mats[[which.min(errs)]]
				return(run_classifier(project(data$test, mat)))
			})

	stopCluster(cl)

	cat(paste0("Single-projection, bruteforce optimization\nMean: ", mean(results),
			" pm ", sd(results)/sqrt(R), "\n\n"))
}


# Compare performance of 1 projection, MCMC optimization
summarize.mcmc = function(R = 100, B2 = 100) {
	cl = makeCluster(detectCores() - 1)
	clusterEvalQ(cl, source("basic-mcmc/core.R"))
	
	results = parSapply(cl, 1:R, function(i) {
				data = basic.model()
				m = mcmc_optimize(data$train, B2 = B2)
				return(run_classifier(project(data$test, m)))
			})
	
	stopCluster(cl)
	
	cat(paste0("Single-projection, MCMC posterior sampling\nMean: ", mean(results),
		" pm ", sd(results)/sqrt(R), "\n\n"))
}


# Compare performance of B1 projections, MCMC optimization
summarize.ensemble.mcmc = function(R = 100, B1 = 100, B2 = 100) {
	cl = makeCluster(detectCores() - 1)
	clusterEvalQ(cl, source("basic-mcmc/core.R"))
	
	cat("Ensemble, MCMC posterior sampling\n")
	
	info = function(modelnum) {
		results = parSapply(cl, 1:R, function(i) {
					data = match.fun(paste0("model.", modelnum))()
					
					return(ensemble.mcmc(data, B1=B1, B2=B2))
				})
		cat(paste0("Model ", modelnum, ": ", mean(results),
						" pm ", sd(results)/sqrt(R), "\n"))
	}
	lapply(1:5, info)
	
	cat("\n")
	
	stopCluster(cl)
}

summarize.ensemble.bruteforce = function(R = 100) {
	cl = makeCluster(detectCores() - 1)
	clusterEvalQ(cl, source("basic-mcmc/core.R"))
	
	cat("Ensemble, bruteforce optimization\n")
	
	info = function(modelnum) {
		results = parSapply(cl, 1:R, function(i) {
					data = match.fun(paste0("model.", modelnum))()
					return(compare.haar.rpe.lda(data))
				})
		cat(paste0("Model ", modelnum, ": ", mean(results),
						" pm ", sd(results)/sqrt(R), "\n"))
	}
	lapply(1:5, info)
	
	cat("\n")
	
	stopCluster(cl)
}