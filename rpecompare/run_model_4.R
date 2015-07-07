# TODO: Add comment
# 
# Author: Ben
###############################################################################

# Define our models & classifiers
source("rpecompare/all_models_and_classifiers.R")

models = list("ntr=50 pi=0.5" = function() model.4(n_train = 50),
		"ntr=100 pi=0.5" = function() model.4(n_train = 100),
		"ntr=200 pi=0.5" = function() model.4(n_train = 200),
		"ntr=50 pi=0.75" = function() model.4(n_train = 50, pi = 0.75),
		"ntr=100 pi=0.75" = function() model.4(n_train = 100, pi = 0.75),
		"ntr=200 pi=0.75" = function() model.4(n_train = 200, pi = 0.75))

classifiers = list(compare.haar.rpe.lda, compare.axis.rpe.lda, compare.rf,
		compare.rbf.svm, compare.boosted.dtrees)


source("rpecompare/core.R")
print(rpecompare(models, classifiers))