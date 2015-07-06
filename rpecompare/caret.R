# TODO: Add comment
# 
# Author: Ben
###############################################################################

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
			trControl=trainControl(verboseIter=T))
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
			trControl=trainControl(verboseIter=T))
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
			trControl=trainControl(verboseIter=T))
	class = predict(model, newdata = test)
	
	# Calculate error
	return(mean(class != test$y))
}

compare.knn = function(data) {
	train = data$train
	test = data$test
	tuneGrid <- expand.grid(k=1:25)
	
	# Run KNN
	model <- train(y ~ .,
			data=train,
			method='knn',
			preProcess = c("center", "scale"),
			tuneGrid = tuneGrid,
			trControl=trainControl(verboseIter=T))
	class = predict(model, newdata = test)
	
	# Calculate error
	return(mean(class != test$y))
}

compare.boosted.dtrees = function(data) {
	train = data$train
	test = data$test
	tuneGrid <- expand.grid(
			iter=c(32, 128),
			maxdepth=c(4, 8, 16),
			nu = c(0.5, 0.8))
	
	# Run boosted trees
	model <- train(y ~ .,
			data=train,
			method='ada',
			preProcess = c("center", "scale"),
			tuneGrid = tuneGrid,
			trControl=trainControl(verboseIter=T))
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
			trControl=trainControl(verboseIter=T))
	class = predict(model, newdata = test)
	
	# Calculate error
	return(mean(class != test$y))
}