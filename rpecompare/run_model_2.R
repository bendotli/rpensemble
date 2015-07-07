# TODO: Add comment
# 
# Author: Ben
###############################################################################

# Define our models & classifiers
source("rpecompare/all_models_and_classifiers.R")

models = list("ntr=50 pi=0.5" = function() model.2(n_train = 50),
		"ntr=100 pi=0.5" = function() model.2(n_train = 100),
		"ntr=200 pi=0.5" = function() model.2(n_train = 200),
		"ntr=50 pi=0.33" = function() model.2(n_train = 50, pi = 0.75),
		"ntr=100 pi=0.33" = function() model.2(n_train = 100, pi = 0.75),
		"ntr=200 pi=0.33" = function() model.2(n_train = 200, pi = 0.75))

classifiers = list(compare.haar.rpe.knn, compare.axis.rpe.knn, compare.rf, compare.rbf.svm)


source("rpecompare/core.R")
print(rpecompare(models, classifiers))