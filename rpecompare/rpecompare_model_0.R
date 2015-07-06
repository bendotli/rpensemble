# TODO: Add comment
# 
# Author: Ben
###############################################################################

# Initialize parallel processing library
require(snow) || install.packages("snow")
cl = makeCluster(36)

# Define our models & classifiers
source("rpecompare/all_models_and_classifiers.R")

models = list(
			function() basic.model(n_train = 50),
			function() basic.model(n_train = 100),
			function() basic.model(n_train = 200)
		)

classifiers = list(
			compare.haar.rpe.lda2,
			compare.haar.rpe.lda,
			compare.lda,
			compare.haar.rpe.qda2,
			compare.haar.rpe.qda,
			compare.qda,
			compare.haar.rpe.knn2,
			compare.haar.rpe.knn
		)

runs = expand.grid(model = models, classifier = classifiers)
runs.list = apply(runs, 1, as.list)

source("rpecompare/evaluate_misclassification_rates.R")
out = parSapply(cl, runs.list, misclassification.rates)
out = as.matrix(out, ncol=length(classifiers))

stopCluster(cl)