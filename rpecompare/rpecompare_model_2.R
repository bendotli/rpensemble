# TODO: Add comment
# 
# Author: Ben
###############################################################################

# Initialize parallel processing library
library(snow)
cl = makeCluster(3)

# Define our worker thread
misclassification.rate.evaulation.thread = function(model) {
	source("rpecompare/evaluate_misclassification_rates.R")
	source("rpecompare/builtin_models.R")
	source("rpecompare/rpeknn.R")
	return(evaluate.misclassification.rates(model,
					compare.with = list(compare.haar.rpe.knn,
										compare.axis.rpe.knn,
										compare.rbf.svm,
										compare.knn)))
}

# Define our models
source("rpecompare/builtin_models.R")
models = list(
		function() model.2(n_train = 50),
		function() model.2(n_train = 100),
		function() model.2(n_train = 200)
	)

source("rpecompare/evaluate_misclassification_rates.R")
out = parSapply(cl, models, misclassification.rate.evaulation.thread)
colnames(out) = c("50", "100", "200")

stopCluster(cl)