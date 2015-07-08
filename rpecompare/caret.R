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
			iter = c(5, 10, 25, 40),
			maxdepth = c(16),
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

require(boot) || install.packages("boot") # inv.logit

calibrate = function(y, y_predicted_probabilities, new_predicted_probabilities) {
	data = data.frame(y = y, x = inv.logit(y_predicted_probabilities))
	newdata = data.frame(x = inv.logit(new_predicted_probabilities))
	cal.model = glm(y ~ x, data = data, family = "binomial")

	class = factor(predict(cal.model, newdata =  newdata, type = "response") > 0.5)
	levels(class)[levels(class) == "FALSE"] = "class.1"
	levels(class)[levels(class) == "TRUE"] = "class.2"
	return(class)
}

compare.gbm = function(data) {
	train = data$train
	test = data$test
	tuneGrid <- expand.grid(
			n.trees = c(20, 50, 100, 250, 500, 750, 1000, 1250),
			interaction.depth = c(2, 5, 10),
			shrinkage = c(0.2, 0.05, 0.01),
			n.minobsinnode = c(1, 2, 4)
	)
	
	# Run calibrated gradient boosted trees
	model <- train(y ~ .,
			data=train,
			method='gbm',
			#metric="accuracy",
			preProcess = c("center", "scale"),
			tuneGrid = tuneGrid,
			trControl = trainControl(
				method="boot", number=10,
				classProbs=T, verboseIter=T))
	train_predictions = predict(model, type = "prob")$class.2
	predictions = predict(model, type = "prob", newdata = test)$class.2
	class = calibrate(train$y, train_predictions, predictions)
	
	# Calculate error
	return(mean(class != test$y))
}

compare.penlda = function(data) {
	train = data$train
	test = data$test
	tuneGrid <- expand.grid(
			lambda = c(0.2), K = c(1))
	
	# Run Penalized LDA
	model <- train(y ~ .,
			data=train,
			method='PenalizedLDA',
			#preProcess = c("center", "scale"),
			tuneGrid = tuneGrid,
			trControl=trainControl(method="none"))#"boot", number=10, verboseIter=T))
	class = predict(model, newdata = test)
	
	# Calculate error
	return(mean(class != test$y))
}
