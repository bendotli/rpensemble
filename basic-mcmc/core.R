# Main routine for MCMC projection matrix optimization.
# 
# Author: Ben Li <bjli@college.harvard.edu>
###############################################################################

# Internal libraries
source("rpecompare/all_models_and_classifiers.R")

# External libraries
require(RPEnsemble) || install.packages("RPEnsemble")
require(memoise) || install.packages("memoise")


#' Initial matrix generation (ie. prior on projection matrix space).
#' For now, just a wrapper for RPGenerate.
#' 
generate_random_matrix = function(p = 50, d = 5, method="Haar") {
	return(RPGenerate(p, d, method))
}


#' Wrapper for running the classifier and returning the in-sample error.
#' Later: add option to use k-fold CV or bootstrap.
#' 
run_classifier = function(data, classifier = "lda", estmethod="insample") {
	# TODO: don't ignore estmethod
	if(classifier == "lda") return(compare.lda(list(train = data, test = data)))
	if(classifier == "qda") return(compare.qda(list(train = data, test = data)))
	if(classifier == "knn") return(compare.knn(list(train = data, test = data)))
}


#' Quick utility function for performing projection on our data structure.
#' IMPORTANT: THIS ASSUMES m IS SELF-ADJOINT (SYMMETRIC)!
#' 
project = function(data, m) {
	x = data.matrix(data[-length(data)])
	return(data.frame(x = x %*% m, y = data$y))
}


#' Calculate Hastings ratio & accept/reject the new state based on it.
#' 
mcmc_accept = function(oldcost, newcost, progress) {
	# Proposal kernel is symmetric, so Hastings ratio is just the ratio of
	# the probabilities. Here, our cost functions are misclassification rates
	# so we take their difference from 1 to obtain the state probabilities.
	hastings_ratio = (1-newcost)/(1-oldcost)
	
	# Try using "logit link"
	hastings_ratio = hastings_ratio*(oldcost/newcost)
	
#	# Temperature schedule
	hastings_ratio = hastings_ratio^100
#	if(progress > 0.25) {
#		hastings_ratio = hastings_ratio^2
#	}
#	if(progress > 0.75) {
#		hastings_ratio = hastings_ratio^2
#	}
	
#	# Try using "Boltzmann distribution" -loglik = risk
#	hastings_ratio = exp(oldcost-newcost)
	
	cat(paste0("MCR: ", oldcost, " -> ", newcost, "\n"))
	cat(paste0("HST: ", hastings_ratio, "\n"))
	
	if(is.nan(hastings_ratio)) hastings_ratio=0.5;
	
	return(runif(1) < hastings_ratio)
}


#' Generate rots rotations through adjacent-coordinate hyperplanes.
#' 
get_standard_rotation = memoize(
		function(p, theta, rots=floor(p/2)) {
	std_rot = matrix(0, nrow = p, ncol = p)
#	std_rot[1,2] = cos(theta*pi/180)
#	std_rot[1,2] = -sin(theta*pi/180)
#	std_rot[2,1] = sin(theta*pi/180)
#	std_rot[2,2] = cos(theta*pi/180)
	for(i in 1:rots) {
		std_rot[2*i-1,2*i-1] = cos(theta*pi/180)
		std_rot[2*i-1,2*i] = -sin(theta*pi/180)
		std_rot[2*i,2*i-1] = sin(theta*pi/180)
		std_rot[2*i,2*i] = cos(theta*pi/180)
	}
	if(2*rots < p) for(i in (2*rots+1):p) {
		std_rot[i,i] = 1;
	}
	return(std_rot)
})


#' Generate rots rotations through random (pairwise-orthogonal)
#' hyperplanes.
#' 
generate_random_rotation = function(p, theta, rots=floor(p/2)) {
	random_basis = generate_random_matrix(p, p, "Haar")
	random_basis_inv = solve(random_basis)
	return(random_basis_inv %*% get_standard_rotation(p, theta, rots) %*% random_basis)
}

#' Perturb a projection matrix slightly.
#' 
mcmc_step = function(m, progress) {
	p = dim(m)[1]
	theta = 30; rots = floor(p/2);
	if(progress > 0.25) {
		theta = 5; rots = floor(p/4);
	}
#	if(progress > 0.75) {
#		theta = 2; rots = floor(p/10);
#	}
	
	if(rots < 1) rots = 1;
	
	return(generate_random_rotation(p, theta, rots) %*% m)
}


#' Main routine
#' 
mcmc_optimize = function(data, B2 = 100, p = 50, d = 5, initialmethod = "Haar",
		classifier = "lda", estmethod = "insample") {
	
	# Generate random matrix (ie. from prior specified by initialmethod)
	m = generate_random_matrix(p, d, initialmethod)
	
	# Run classifier & get in-sample error
	m_err = run_classifier(project(data, m), classifier, estmethod)
	cat(paste0("Initial misclassification rate: ", m_err, "\n"))
	
	# Run Markov chain for a total of R steps
	accept_ctr = 0
	m_err_plot = vector("list", B2+1)
	m_err_plot[1] = m_err;
	for(i in 1:B2) {
		#cat(paste0("Iteration ", i, "\n"))
		
		# Make proposal
		proposal = mcmc_step(m, i/B2)
		
		# Run classifier with new projection matrix & get in-sample error
		proposal_err = run_classifier(project(data, proposal), classifier, estmethod)
		
		# Accept/reject
		if(mcmc_accept(m_err, proposal_err, i/B2)) {
			m = proposal
			m_err = proposal_err
			accept_ctr = accept_ctr + 1
		}
		m_err_plot[i+1] = m_err
	}
	cat(paste0("Final misclassification rate: ", m_err, "\n"))
	cat(paste0("Acceptance rate: ", accept_ctr/B2, "\n"))
	plot(0:B2, m_err_plot, type="b")
	
	return(m)
}



# Small utility functions
class.labels.to.integers = function(label) {
	if(label == "class.1") return(1); return(2);
}
class.labels.to.strings = function(label) {
	if(label == 1) return("class.1"); return("class.2");
}

ensemble.mcmc = function(data, B1=100, B2=100, p=50, d=5, classifier="lda", estmethod="insample") {
	# Get B1 (independently) optimized projections over training set
	mats = lapply(1:B1, function(...) mcmc_optimize(data$train, B2 = B2, p, d,
						classifier = classifier, estmethod = estmethod))
	
	# Fit B1 models
	models = lapply(mats, function(mat) {
				return(match.fun(classifier)(y ~ ., data = project(data$train, mat)))
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
	return(mean(out != data$test$y))
}


run.experiment = function(task, R=100, title="Experiment") {
	task = substitute(task)
	cl = makeCluster(detectCores() - 1)
	clusterEvalQ(cl, source("basic-mcmc/core.R"))
	
	print(system.time({
			results = parSapply(cl, 1:R, function(i) eval(task))
		}))
	cat(paste0(title, ": ", mean(results),
					" pm ", sd(results)/sqrt(R), "\n"))
	
	stopCluster(cl)
}


# Unit test
is.projection.matrix = function(mat) {
	cat(colSums(mat^2)) # check normalization: should be vector of ones
	cat("\n")
	print(crossprod(mat)) # check orthogonality: should be identity matrix
	cat("\n")
}