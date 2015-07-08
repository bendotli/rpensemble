## Basic model: two p-dimensional Gaussians with the same identity covariance matrix,
## shifted apart from each other in d of the p dimensions by 2*delta

basic.model = function(
		p = 50,
		d = 5,
		delta = 1,
		n_train = 50, # must be even
		n_test = 1000 # must be even
		) {
	
	# Mean vectors and lists of response vectors
	mean1 = c(rep(delta, times = d), rep(0, times = p-d))
	mean2 = c(rep(-delta, times = d), rep(0, times = p-d))
	
	# Generate classes
	train.class1 = replicate(n_train/2, rnorm(p, mean1))
	train.class2 = replicate(n_train/2, rnorm(p, mean2))
	test.class1 = replicate(n_test/2, rnorm(p, mean1))
	test.class2 = replicate(n_test/2, rnorm(p, mean2))
	
	# Generate test and training data
	train = data.frame(x = t(cbind(train.class1, train.class2)),
			y = factor(c(rep(1, n_train/2), rep(2, n_train/2), c("Class 1", "Class 2"))))
	test = data.frame(x = t(cbind(test.class1, test.class2)),
			y = factor(c(rep(1, n_test/2), rep(2, n_test/2), c("Class 1", "Class 2"))))
	
	return(list(train=train, test=test))
}