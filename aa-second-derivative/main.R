# TODO: Add comment
# 
# Author: Ben
###############################################################################

source("rpecompare/builtin_models.R")
source("rpecompare/basicmodel.R")

# Build a histogram of G_{n,1}(p)=P(P(Chat_n(AX)=1|X)<=p)

# Sample X, P(Chat_n(AX)=1|X) (by sampling)

sample.g = function() {
	data = model.1()#basic.model(p = 5, d = 2)
	train = data$train
	test = data$test
	n_train = length(train$y)
	n_test = length(test$y)
	p = ncol(train)-1
	d = 5 ###############################
	
	# Run RPE LDA with Haar measure-drawn/axis-aligned random projections
	rpelda.out = RPParallel(XTrain = data.matrix(train[-(p+1)]), YTrain = train$y,
			XTest = data.matrix(test[-(p+1)]), d = d, B1 = 100, B2 = 100,
			#projmethod = "axis",
			cores = 1)
	
	# Estimate G_{n,i}
	y.pred = rpelda.out[1:n_train, ]
	G.n.1.data = 2 - rowMeans(y.pred[train$y == "class.1", ], na.rm = TRUE)
	G.n.2.data = 2 - rowMeans(y.pred[train$y == "class.2", ], na.rm = TRUE)
	
	# Return them as data, not as ecdfs
	return(list(G.n.1.data, G.n.2.data))
}

replicate.sample.g = function(r = 10) {
	library(parallel)
	cl = makeCluster(detectCores() - 1)
	clusterExport(cl, c("sample.g"))
	clusterEvalQ(cl, source("rpecompare/builtin_models.R"))
	clusterEvalQ(cl, source("rpecompare/basicmodel.R"))
	out = parSapply(cl, 1:r, function(i, ...) { return(sample.g()) } )
	stopCluster(cl)
	
	return(out)
}

#out = replicate(10, sample.g())
out = replicate.sample.g()
G.n.1 = ecdf(do.call(c, out[1,]))
G.n.2 = ecdf(do.call(c, out[2,]))

#png("G_ni_mini.png")
#plot(G.n.1, col="red", xval=(1:100)/100)
#lines(G.n.2, col="green", xval=(1:100)/100)
#dev.off()

R1 = do.call(c, out[1,])
png("r1_dist.png")
hist(R1)
dev.off()