# TODO: Add comment
# 
# Author: Ben
###############################################################################

require(RPEnsemble) || install.packages("RPEnsemble")

#' RPEnsemble package expects class labels to be integers in {1, 2}

format.class.labels = function(train) {
	train$y = sapply(train$y, function(label) {
				if(label == "class.1") return(1); return(2); })
	return(train)
}

#' Compare RPE LDA with Haar measure-drawn random projections

compare.haar.rpe.lda = function(data) {
	train = format.class.labels(data$train)
	test = format.class.labels(data$test)
	n_train = length(train$y)
	n_test = length(test$y)
	p = ncol(train)-1
	d = 5 ###############################
	
	# Run RPE LDA with Haar measure-drawn random projections
	rpelda.out = RPParallel(XTrain = data.matrix(train[-(p+1)]), YTrain = train$y,
			XTest = data.matrix(test[-(p+1)]), d = d, B1 = 100, B2 = 100,
			cores = 1)
	
	# Estimate the class 1 prior probability
	rpelda.p_hat = sum(train$y == 1)/n_train
	
	# Choose the best value for alpha (the voting threshold) emperically
	rpelda.alpha_hat = RPalpha(RP.out = rpelda.out, Y = train$y, p1 = rpelda.p_hat)
	
	# Ensemble the base classifiers
	rpelda.class = RPEnsembleClass(RP.out = rpelda.out, n = n_train, n.test = n_test,
			p1 = rpelda.p_hat, alpha = rpelda.alpha_hat)
	
	# Calculate error
	return(mean(rpelda.class != test$y))
}

compare.axis.rpe.lda = function(data) {
	train = format.class.labels(data$train)
	test = format.class.labels(data$test)
	n_train = length(train$y)
	n_test = length(test$y)
	p = ncol(train)-1
	d = 5 ###############################
	
	# Run RPE LDA with axis-aligned random projections
	rpelda.out = RPParallel(XTrain = data.matrix(train[-(p+1)]), YTrain = train$y,
			XTest = data.matrix(test[-(p+1)]), d = d, B1 = 100, B2 = 100,
			projmethod = "axis",
			cores = 1)
	
	# Estimate the class 1 prior probability
	rpelda.p_hat = sum(train$y == 1)/n_train
	
	# Choose the best value for alpha (the voting threshold) emperically
	rpelda.alpha_hat = RPalpha(RP.out = rpelda.out, Y = train$y, p1 = rpelda.p_hat)
	
	# Ensemble the base classifiers
	rpelda.class = RPEnsembleClass(RP.out = rpelda.out, n = n_train, n.test = n_test,
			p1 = rpelda.p_hat, alpha = rpelda.alpha_hat)
	
	# Calculate error
	return(mean(rpelda.class != test$y))
}

compare.haar.rpe.lda2 = function(data) {
	train = format.class.labels(data$train)
	test = format.class.labels(data$test)
	n_train = length(train$y)
	n_test = length(test$y)
	p = ncol(train)-1
	d = 2 ###############################
	
	# Run RPE LDA with Haar measure-drawn random projections
	rpelda.out = RPParallel(XTrain = data.matrix(train[-(p+1)]), YTrain = train$y,
			XTest = data.matrix(test[-(p+1)]), d = d, B1 = 100, B2 = 100,
			cores = 1)
	
	# Estimate the class 1 prior probability
	rpelda.p_hat = sum(train$y == 1)/n_train
	
	# Choose the best value for alpha (the voting threshold) emperically
	rpelda.alpha_hat = RPalpha(RP.out = rpelda.out, Y = train$y, p1 = rpelda.p_hat)
	
	# Ensemble the base classifiers
	rpelda.class = RPEnsembleClass(RP.out = rpelda.out, n = n_train, n.test = n_test,
			p1 = rpelda.p_hat, alpha = rpelda.alpha_hat)
	
	# Calculate error
	return(mean(rpelda.class != test$y))
}

compare.axis.rpe.lda2 = function(data) {
	train = format.class.labels(data$train)
	test = format.class.labels(data$test)
	n_train = length(train$y)
	n_test = length(test$y)
	p = ncol(train)-1
	d = 2 ###############################
	
	# Run RPE LDA with axis-aligned random projections
	rpelda.out = RPParallel(XTrain = data.matrix(train[-(p+1)]), YTrain = train$y,
			XTest = data.matrix(test[-(p+1)]), d = d, B1 = 100, B2 = 100,
			projmethod = "axis",
			cores = 1)
	
	# Estimate the class 1 prior probability
	rpelda.p_hat = sum(train$y == 1)/n_train
	
	# Choose the best value for alpha (the voting threshold) emperically
	rpelda.alpha_hat = RPalpha(RP.out = rpelda.out, Y = train$y, p1 = rpelda.p_hat)
	
	# Ensemble the base classifiers
	rpelda.class = RPEnsembleClass(RP.out = rpelda.out, n = n_train, n.test = n_test,
			p1 = rpelda.p_hat, alpha = rpelda.alpha_hat)
	
	# Calculate error
	return(mean(rpelda.class != test$y))
}