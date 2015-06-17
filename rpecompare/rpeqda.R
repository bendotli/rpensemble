library(RPEnsemble)

compare.haar.rpe.qda = function(data) {
	train = data$train
	test = data$test
	n_train = length(train$y)
	n_test = length(test$y)
	p = ncol(train)-1
	d = 5 ###############################
	
	# Run RPE LDA with Haar measure-drawn random projections
	rpelda.out = RPParallel(XTrain = data.matrix(train[-(p+1)]), YTrain = train$y,
			XTest = data.matrix(test[-(p+1)]), d = d, B1 = 100, B2 = 100,
			base = "QDA", estmethod = "loo",
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

compare.axis.rpe.qda = function(data) {
	train = data$train
	test = data$test
	n_train = length(train$y)
	n_test = length(test$y)
	p = ncol(train)-1
	d = 5 ###############################
	
	# Run RPE QDA with axis-aligned random projections
	rpelda.out = RPParallel(XTrain = data.matrix(train[-(p+1)]), YTrain = train$y,
			XTest = data.matrix(test[-(p+1)]), d = d, B1 = 100, B2 = 100,
			base = "QDA", estmethod = "loo",
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