# TODO: Add comment
# 
# Author: Ben
###############################################################################

source("rpecompare/evaluate_misclassification_rates.R")

rfasdf = function(data) {
	train = data$train
	test = data$test
	tuneGrid <- data.frame(.mtry=(2:10))
	
	# Run random forest
	rf.model <- train(y ~ ., data=train,
			ntree=100,
			method='rf',
			preProcess = c("center", "scale"),
			tuneGrid = tuneGrid,
			trControl=trainControl(method='oob', verboseIter=T))
	rf.class = predict(rf.model, newdata = test)
	
	# Calculate error
	return(mean(rf.class != test$y))
}

threadasdf = function(model) {
	return(evaluate.misclassification.rates(model, c(function(x) { return(1)}, rfasdf)))
}

out = sapply(default.models, threadasdf)
colnames(out) = c("50", "100", "200")

stopCluster(cl)