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
	source("rpecompare/rpeqda.R")
	return(evaluate.misclassification.rates(model,
					compare.with = list(compare.haar.rpe.lda2,
										compare.haar.rpe.lda,
										compare.lda,
										compare.haar.rpe.qda2,
										compare.haar.rpe.qda,
										compare.qda,
										compare.haar.rpe.knn2,
										compare.haar.rpe.knn)))
}

# Define our models
source("rpecompare/basicmodel.R")
models = list(
		function() basic.model(n_train = 50),
		function() basic.model(n_train = 100),
		function() basic.model(n_train = 200)
	)

source("rpecompare/evaluate_misclassification_rates.R")
out = parSapply(cl, models, misclassification.rate.evaulation.thread)
colnames(out) = c("50", "100", "200")

stopCluster(cl)