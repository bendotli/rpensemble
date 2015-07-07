# TODO: Add comment
# 
# Author: Ben
###############################################################################

source("rpecompare/ldaqda.R")
source("rpecompare/rpelda.R")
source("rpecompare/caret.R")
source("rpecompare/basicmodel.R")

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

# Input:
#	models - list (preferably with labels!) of models
#	classifiers - list (preferably with labels!) of classifiers
# Output:
# 	out$misclass - with rows as models, cols as classifiers, using labels
# 	out$misclass.sd - with same
rpecompare = function(models, classifiers, cores=36) {
	# Initialize parallel processing library
	require(snow) || install.packages("snow")
	if(cores > length(models)*length(classifiers))
		cores = length(models)*length(classifiers)
	cl = makeCluster(cores)
	
	# Expand out all combinations of model+classifier as a list of lists
	runs = expand.grid(model = models, classifier = classifiers)
	runs.list = apply(runs, 1, as.list)
	
	# Run misclassification.rates on each combination
	out = parSapply(cl, runs.list, misclassification.rates)
	raw.misclass = out[1,]
	raw.misclass.sd = out[2,]
	
	# Format into grid
	misclass = matrix(raw.misclass, nrow=length(models),
			dimnames=list(names(models), names(classifiers)))
	misclass.sd = matrix(raw.misclass.sd, nrow=length(models),
			dimnames=list(names(models), names(classifiers)))
	
	# Stop cluster
	stopCluster(cl)
	
	return(list(misclass=misclass, misclass.sd=misclass.sd))
}