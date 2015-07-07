# TODO: Add comment
# 
# Author: Ben
###############################################################################

require(e1071) || install.packages("e1071")
require(caret) || install.packages("caret")

compare.rf = function(data) {
	train = data$train
	test = data$test
	tuneGrid <- data.frame(.mtry=(2:10))
	
	# Run random forest
	model <- train(y ~ ., data=train,
			ntree=1000,
			method='rf',
			preProcess = c("center", "scale"),
			tuneGrid = tuneGrid,
			trControl=trainControl(method='oob', verboseIter=T))
	class = predict(model, newdata = test)
	
	# Calculate error
	return(mean(class != test$y))
}


compare.pls = function(data) {
	train = data$train
	test = data$test
	tuneGrid <- data.frame(.ncomp=1:5)
	
	# Run partial least squares
	model <- train(y ~ ., data=train,
			method='pls',
			preProcess = c("center", "scale"),
			tuneGrid = tuneGrid,
			trControl=trainControl(method="boot", number=10, verboseIter=T))
	class = predict(model, newdata = test)
	
	# Calculate error
	return(mean(class != test$y))
}

compare.linear.svm = function(data) {
	train = data$train
	test = data$test
	tuneGrid <- data.frame(.C = c(0.005, 0.01, 0.05, 0.1))
	
	# Run linear SVM
	model <- train(y ~ .,
			data=train,
			method='svmLinear',
			preProcess = c("center", "scale"),
			tuneGrid = tuneGrid,
			trControl=trainControl(method="boot", number=10, verboseIter=T))
	class = predict(model, newdata = test)
	
	# Calculate error
	return(mean(class != test$y))
}

compare.rbf.svm = function(data) {
	train = data$train
	test = data$test
	tuneGrid <- data.frame(.C = 1:5)
	
	# Run kernel SVM
	model <- train(y ~ .,
			data=train,
			method='svmRadialCost',
			preProcess = c("center", "scale"),
			tuneGrid = tuneGrid,
			trControl=trainControl(method="boot", number=10, verboseIter=T))
	class = predict(model, newdata = test)
	
	# Calculate error
	return(mean(class != test$y))
}

compare.knn = function(data) {
	train = data$train
	test = data$test
	tuneGrid <- expand.grid(k=c(2, 5, 10, 17, 25))
	
	# Run KNN
	model <- train(y ~ .,
			data=train,
			method='knn',
			preProcess = c("center", "scale"),
			tuneGrid = tuneGrid,
			trControl=trainControl(method="boot", number=10, verboseIter=T))
	class = predict(model, newdata = test)
	
	# Calculate error
	return(mean(class != test$y))
}

compare.boosted.dtrees = function(data) {
	train = data$train
	test = data$test
	tuneGrid <- expand.grid(
			iter = c(10, 20, 30),
			maxdepth = c(5, 10),
			nu = c(0.5))
	
	# Run boosted trees
	model <- train(y ~ .,
			data=train,
			method='ada',
			preProcess = c("center", "scale"),
			tuneGrid = tuneGrid,
			trControl=trainControl(method="boot", number=10, verboseIter=T))
	class = predict(model, newdata = test)
	
	# Calculate error
	return(mean(class != test$y))
}

compare.penlda = function(data) {
	train = data$train
	test = data$test
	tuneGrid <- expand.grid(
			lambda = c(0.1, 0.2, 0.5), K = c(5))
	
	# Run Penalized LDA
	model <- train(y ~ .,
			data=train,
			method='PenalizedLDA',
			preProcess = c("center", "scale"),
			tuneGrid = tuneGrid,
			trControl=trainControl(method="boot", number=10, verboseIter=T))
	class = predict(model, newdata = test)
	
	# Calculate error
	return(mean(class != test$y))
}

compare.linear.gp = function(data) {
	train = data$train
	test = data$test
	tuneGrid <- expand.grid()
	
	# Run linear Gaussian process classifier
	model <- train(y ~ .,
			data=train,
			method='gaussprLinear',
			preProcess = c("center", "scale"),
			tuneGrid = tuneGrid,
			trControl=trainControl(method="none"))
	class = predict(model, newdata = test)
	
	# Calculate error
	return(mean(class != test$y))
}