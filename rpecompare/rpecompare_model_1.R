# TODO: Add comment
# 
# Author: Ben
###############################################################################

# Define our models & classifiers
source("rpecompare/all_models_and_classifiers.R")

models = list("ntr=50 pi=0.5" = function() model.1(n_train = 50),
		"ntr=100 pi=0.5" = function() model.1(n_train = 100),
		"ntr=200 pi=0.5" = function() model.1(n_train = 200),
		"ntr=50 pi=0.33" = function() model.1(n_train = 50, pi = 0.33),
		"ntr=100 pi=0.33" = function() model.1(n_train = 100, pi = 0.33),
		"ntr=200 pi=0.33" = function() model.1(n_train = 200, pi = 0.33),)

classifiers = list(compare.haar.rpe.qda, compare.axis.rpe.qda, compare.rbf.svm)


source("rpecompare/core.R")
rpecompare(models, classifiers)