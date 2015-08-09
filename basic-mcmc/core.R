# Main routine for MCMC projection matrix optimization.
# 
# Author: Ben Li <jiachengli@college.harvard.edu>
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
	# TODO: don't ignore arguments
	return(compare.lda(list(train = data, test = data)))
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
mcmc_accept = function(oldcost, newcost) {
	# Proposal kernel is symmetric, so Hastings ratio is just the ratio of
	# the probabilities. Here, our cost functions are misclassification rates
	# so we take their difference from 1 to obtain the state probabilities.
	hastings_ratio = (1-newcost)/(1-oldcost)
	# Let's make it really cold
	hastings_ratio = hastings_ratio^100
	return(runif(1) < hastings_ratio)
}


#' Generate floor(p/2) rotations through adjacent-coordinate hyperplanes.
#' 
get_standard_rotation = memoize(function(p, theta) {
	std_rot = matrix(0, nrow = p, ncol = p)
#	std_rot[1,2] = cos(theta*pi/180)
#	std_rot[1,2] = -sin(theta*pi/180)
#	std_rot[2,1] = sin(theta*pi/180)
#	std_rot[2,2] = cos(theta*pi/180)
	for(i in 1:floor(p/2)) {
		std_rot[2*i-1,2*i-1] = cos(theta*pi/180)
		std_rot[2*i-1,2*i] = -sin(theta*pi/180)
		std_rot[2*i,2*i-1] = sin(theta*pi/180)
		std_rot[2*i,2*i] = cos(theta*pi/180)
	}
	if(p %% 2 == 1) std_rot[p,p] = 1
	return(std_rot)
})


#' Generate floor(p/2) rotations through random (pairwise-orthogonal)
#' hyperplanes.
#' 
generate_random_rotation = function(p, theta) {
	random_basis = generate_random_matrix(p, p, "Haar")
	random_basis_inv = solve(random_basis)
	return(random_basis_inv %*% get_standard_rotation(p, theta) %*% random_basis)
}

#' Perturb a projection matrix slightly.
#' 
mcmc_step = function(m) {
	p = dim(m)[1]
	return(generate_random_rotation(p, 5) %*% m)
}


#' Main routine
#' 
mcmc_optimize = function(data, B2 = 1000, p = 50, d = 5, initialmethod = "Haar",
		classifier = "lda", estmethod = "insample") {
	
	# Generate random matrix (ie. from prior specified by initialmethod)
	m = generate_random_matrix(p, d, initialmethod)
	
	# Run classifier & get in-sample error
	m_err = run_classifier(project(data, m), classifier, estmethod)
	cat(paste0("Initial misclassification rate: ", m_err, "\n"))
	
	# Run Markov chain for a total of R steps
	for(i in 1:B2) {
		#cat(paste0("Iteration ", i, "\n"))
		
		# Make proposal
		proposal = mcmc_step(m)
		
		# Run classifier with new projection matrix & get in-sample error
		proposal_err = run_classifier(project(data, proposal), classifier, estmethod)
		
		# Accept/reject
		if(mcmc_accept(m_err, proposal_err)) {
			m = proposal
			m_err = proposal_err
		}
	}
	cat(paste0("Final misclassification rate: ", m_err, "\n"))
	
	return(m)
}

