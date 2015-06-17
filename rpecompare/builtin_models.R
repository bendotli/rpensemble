# TODO: Add comment
# 
# Author: Ben
###############################################################################

library(RPEnsemble)

model.1 = function(p = 50, n_train = 50, n_test = 100) { 
	train.data = RPModel(1, n_train, p)
	train = data.frame(x = train.data$x, y = factor(train.data$y))
	
	test.data = RPModel(1, n_test, p)
	test = data.frame(x = test.data$x, y = factor(test.data$y))
	
	return(list(train = train, test = test))
}


model.2 = function(p = 50, n_train = 50, n_test = 100) { 
	train.data = RPModel(2, n_train, p)
	train = data.frame(x = train.data$x, y = factor(train.data$y))
	
	test.data = RPModel(2, n_test, p)
	test = data.frame(x = test.data$x, y = factor(test.data$y))
	
	return(list(train = train, test = test))
}


model.3 = function(p = 50, n_train = 50, n_test = 100) { 
	train.data = RPModel(3, n_train, p)
	train = data.frame(x = train.data$x, y = factor(train.data$y))
	
	test.data = RPModel(3, n_test, p)
	test = data.frame(x = test.data$x, y = factor(test.data$y))
	
	return(list(train = train, test = test))
}

model.4 = function(p = 50, n_train = 50, n_test = 100) { 
	train.data = RPModel(4, n_train, p)
	train = data.frame(x = train.data$x, y = factor(train.data$y))
	
	test.data = RPModel(4, n_test, p)
	test = data.frame(x = test.data$x, y = factor(test.data$y))
	
	return(list(train = train, test = test))
}
