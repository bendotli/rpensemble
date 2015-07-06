# TODO: Add comment
# 
# Author: Ben
###############################################################################

source("rpecompare/ldaqda.R")
source("rpecompare/rpelda.R")
source("rpecompare/caret.R")
source("rpecompare/basicmodel.R")

require(plyr) || install.packages("plyr")

default.classifiers = c(compare.lda, compare.haar.rpe.lda, compare.axis.rpe.lda,
		compare.rf, compare.pls, compare.linear.svm)

default.models = c(
		basic.model,
		function() { return(basic.model(n_train=100)) },
		function() { return(basic.model(n_train=200)) })


evaluate.misclassification.rates = function(
		model = basic.model,
		compare.with = default.classifiers,
		r = 100) {
	
	# Make r datasets
	data = replicate(r, model(), simplify=FALSE)
	
	# Run each classifier on each dataset
	runs = sapply(data, each(compare.with))
	
	misclass.rates = apply(runs, 1, mean)
	std.deviations = apply(runs, 1, sd)
	
	return(list(misclass=misclass.rates, misclass.sd=std.deviations))
}

# New version that doesn't deal with lists & optimized for feeding into sapply
misclassification.rates = function(params) {
	model = params$model
	classifier = params$classifier
	r = 100
	
	# Load model & classifier definitions
	source("rpecompare/all_models_and_classifiers.R")
	
	# Make r datasets
	data = replicate(r, model(), simplify=FALSE)
	
	# Run classifier on each dataset
	runs = sapply(data, classifier)
	
	return(list(misclass=mean(runs), misclass.sd=sd(runs)))
	
}