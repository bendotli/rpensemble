# TODO: Add comment
# 
# Author: Ben
###############################################################################

# Define our models & classifiers
source("rpecompare/all_models_and_classifiers.R")

models = list("ntr=50 pi=0.5" = function() basic.model(n_train = 50),
		"ntr=100 pi=0.5" = function() basic.model(n_train = 100),
		"ntr=200 pi=0.5" = function() basic.model(n_train = 200))

classifiers = list(
			"RPE-H(d=2) LDA" = compare.haar.rpe.lda2,
			"RPE-H(d=5) LDA" = compare.haar.rpe.lda,
			#"LDA" = compare.lda,
			"RPE-H(d=2) QDA" = compare.haar.rpe.qda2,
			"RPE-H(d=5) QDA" = compare.haar.rpe.qda,
			#"QDA" = compare.qda,
			"RPE-H(d=2) KNN" = compare.haar.rpe.knn2,
			"RPE-H(d=5) KNN" = compare.haar.rpe.knn)


source("rpecompare/core.R")
rpecompare(models, classifiers)