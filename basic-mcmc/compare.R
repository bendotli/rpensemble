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
	
	cat(paste0("Single-projection, MCMC posterior predictive sampling\nMean: ", mean(results),
		" pm ", sd(results)/sqrt(R), "\n\n"))
}


# Small utility functions
class.labels.to.integers = function(label) {
	if(label == "class.1") return(1); return(2);
}
class.labels.to.strings = function(label) {
	if(label == 1) return("class.1"); return("class.2");
}


# Compare performance of B1 projections, MCMC optimization
summarize.ensemble.mcmc = function(R = 100, B1 = 100, B2 = 100) {
	cl = makeCluster(detectCores() - 1)
	clusterEvalQ(cl, source("basic-mcmc/core.R"))
	clusterExport(cl, "class.labels.to.integers")
	clusterExport(cl, "class.labels.to.strings")
	
	cat("Ensemble, MCMC posterior predictive sampling\n")
	
	info = function(modelnum) {
		results = parSapply(1:R, function(i) {
					data = match.fun(paste0("model.", modelnum))()
					
					# Get B1 (independently) optimized projections over training set
					mats = lapply(1:B1, function(...) mcmc_optimize(data$train, B2 = B2))
					
					# Fit B1 LDA models
					models = lapply(mats, function(mat) {
							return(lda(y ~ ., data = project(data$train, mat)))
						})
				
					# Obtain B1 class predictions on training set
					# (we transform the class labels into integers {1, 2} for use
					# with RPalpha)
					tr.pred = mapply(function(model, mat) {
							class = predict(model)$class
							return(sapply(class, class.labels.to.integers))
						}, models, mats)
					
					# Compute estimate of alpha (well, alpha+1)
					Y = sapply(data$train$y, class.labels.to.integers)
					alpha.hat = RPalpha(tr.pred, Y, 2 - mean(Y))
					
					# Obtain B1 class predictions on test set
					# (again, transforming class labels into integers)
					te.pred = mapply(function(model, mat) {
							class = predict(model, newdata = project(data$test, mat))$class
							return(sapply(class, class.labels.to.integers))
						}, models, mats)
					
					# Consolidate into ensemble predictions using alpha
					vote = 1 + as.numeric(rowMeans(te.pred) > alpha.hat)
					out = sapply(vote, class.labels.to.strings)
					
					# Return misclassification rate on test set
					return(out != data$test$y)
				})
		cat(paste0("Model ", modelnum, ": ", mean(results),
						" pm ", sd(results)/sqrt(R), "\n"))
	}
	lapply(1:5, info)
	
	cat("\n")
	
	stopCluster(cl)
}

summarize.ensemble.bruteforce = function(R = 100, B1 = 100, B2 = 100) {
	
}