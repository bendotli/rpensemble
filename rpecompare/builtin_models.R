# TODO: Add comment
# 
# Author: Ben
###############################################################################

require(RPEnsemble) || install.packages("RPEnsemble")

model.1 = function(p = 50, n_train = 50, n_test = 100, pi = 0.5) { 
	train.data = RPModel(1, n_train, p, pi)
	train = data.frame(x = train.data$x, y = factor(train.data$y))
	
	test.data = RPModel(1, n_test, p, pi)
	test = data.frame(x = test.data$x, y = factor(test.data$y))
	
	return(list(train = train, test = test))
}


model.2 = function(p = 50, n_train = 50, n_test = 100, pi = 0.5) {  
	train.data = RPModel(2, n_train, p, pi)
	train = data.frame(x = train.data$x, y = factor(train.data$y))
	
	test.data = RPModel(2, n_test, p, pi)
	test = data.frame(x = test.data$x, y = factor(test.data$y))
	
	return(list(train = train, test = test))
}


model.3 = function(p = 50, n_train = 50, n_test = 100, pi = 0.5) { 
	train.data = RPModel(3, n_train, p, pi)
	train = data.frame(x = train.data$x, y = factor(train.data$y))
	
	test.data = RPModel(3, n_test, p, pi)
	test = data.frame(x = test.data$x, y = factor(test.data$y))
	
	return(list(train = train, test = test))
}

model.4 = function(p = 50, n_train = 50, n_test = 100, pi = 0.5) {  
	train.data = RPModel(4, n_train, p, pi)
	train = data.frame(x = train.data$x, y = factor(train.data$y))
	
	test.data = RPModel(4, n_test, p, pi)
	test = data.frame(x = test.data$x, y = factor(test.data$y))
	
	return(list(train = train, test = test))
}

model.5 = function(n_train = 50) { 
	data("Ozone")
	indicators = sample(1:512, size = n_train)
	train = data.frame(x = Ozone$x[indicators,], y = Ozone$y[indicators])
	test = data.frame(x = Ozone$x[-indicators,], y = Ozone$y[-indicators])
	
	return(list(train = train, test = test))
}


