#install.packages("RPEnsemble", type="source")
library(RPEnsemble)

## Basic model: two p-dimensional Gaussians with the same identity covariance matrix,
## shifted apart from each other in d of the p dimensions by 2*delta

# Parameters (change me)
p = 50
d = 5
delta = 1
n_train = 50 # must be even
n_test = 100 # must be even

# Mean vectors and lists of response vectors
mean1 = c(rep(delta, times = d), rep(0, times = p-d))
mean2 = c(rep(-delta, times = d), rep(0, times = p-d))

train.class1 = replicate(n_train/2, rnorm(p, mean1), simplify = "array")
train.class2 = replicate(n_train/2, rnorm(p, mean2), simplify = "array")
test.class1 = replicate(n_test/2, rnorm(p, mean1), simplify = "array")
test.class2 = replicate(n_test/2, rnorm(p, mean2), simplify = "array")

# Generate test and training data
train = data.frame(x = t(cbind(train.class1, train.class2)),
                   y = factor(matrix(c(rep(1, n_train/2), rep(2, n_train/2)), ncol = 1)))
test = data.frame(x = t(cbind(test.class1, test.class2)),
                  y = factor(matrix(c(rep(1, n_test/2), rep(2, n_test/2)), ncol = 1)))

########################################

# Run LDA
lda.model = lda(y ~ ., data = train)
lda.out = predict(lda.model, newdata = test)
lda.class = lda.out$class

# Calculate error
print(mean(lda.class != test$y))

########################################

# Run RPE-LDA
#Train.Class = train$class # workaround for bug in RPEnsemble 0.2 that directly references global variable
#Test.Class = test$class # workaround for bug in RPEnsemble 0.2 that directly references global variable
#rpelda.out = RPParallel(XTrain = data.matrix(train[-(p+1)]), YTrain = train$y,
#                        XTest = data.matrix(test[-(p+1)]), d = d, B1 = 10, B2 = 10,
#                        base = "knn", k = seq(1, 25, by = 2),
#                        projmethod = "Haar",
#                        estmethod = "loo",
#                        splitsample = FALSE, cores = 1)

rpelda.out = RPParallel(XTrain = data.matrix(train[-(p+1)]), YTrain = train$y,
                        XTest = data.matrix(test[-(p+1)]), d = d, B1 = 10, B2 = 10,
                        #projmethod = "axis",
                        cores = 1)

# Estimate the class 1 prior probability
rpelda.p_hat = sum(train$y == 1)/n_train

# Choose the best value for alpha (the voting threshold) emperically
rpelda.alpha_hat = RPalpha(RP.out = rpelda.out, Y = train$y, p1 = rpelda.p_hat)

# Ensemble the base classifiers
rpelda.class = RPEnsembleClass(RP.out = rpelda.out, n = n_train, n.test = n_test,
                               p1 = rpelda.p_hat, alpha = rpelda.alpha_hat)

# Calculate error
print(mean(rpelda.class != test$y))

#######################################

library(caret)

# Run random forest

tuneGrid <- data.frame(.mtry=(2:10))

rf.model <- train(y ~ ., data=train,
               ntree=1000,
               method='rf',
               preProcess = c("center", "scale"),
               tuneGrid = tuneGrid,
               trControl=trainControl(method='oob', verboseIter=T))

rf.class = predict(rf.model, newdata = test)

# Calculate error
print(mean(rf.class != test$y))

#######################################

library(caret)

# Run partial least squares

tuneGrid <- data.frame(.ncomp=1:5)

pls.model <- train(y ~ ., data=train,
               method='pls',
               preProcess = c("center", "scale"),
               tuneGrid = tuneGrid,
               trControl=trainControl(verboseIter=T))

pls.class = predict(pls.model, newdata = test)

# Calculate error
print(mean(pls.class != test$y))

