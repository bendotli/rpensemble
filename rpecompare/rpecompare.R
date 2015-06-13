# TODO: Add comment
# 
# Author: Ben
###############################################################################

# Initialize parallel processing library
library(snow)
cl = makeCluster(1)

# Define our worker thread
misclassification.rate.evaulation.thread = function(model) {
	source("rpecompare/evaluate_misclassification_rates.R")
	return(evaluate.misclassification.rates(model))
}

source("rpecompare/evaluate_misclassification_rates.R")
out = parSapply(cl, default.models, misclassification.rate.evaulation.thread)
colnames(out) = c("50", "100", "200")