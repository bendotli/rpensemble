# TODO: Add comment
# 
# Author: Ben
###############################################################################

require(MASS) || install.packages("MASS")

compare.lda = function(data) {
	train = data$train
	test = data$test
	
	# Run LDA
	lda.model = lda(y ~ ., data = train)
	lda.out = predict(lda.model, newdata = test)
	lda.class = lda.out$class
	
	# Calculate error
	return(mean(lda.class != test$y))
}
