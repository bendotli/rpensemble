# Calculate Bayes risk of basic model:

## Basic model: two p-dimensional Gaussians with the same identity covariance matrix,
## shifted apart from each other in d of the p dimensions by 2*delta

# Parameters (change me)
d = 5
delta = 1
r = 100000

# Mean vectors
# (first d only, as the other p-d entries are ignored by the Bayes classifier)
means = list(rep(delta, times = d), rep(-delta, times = d))

# Bayes classifier
bayes_classifier = function(v) {
  if(sum(v) > 0) return(1)
  return(2)
}

# Generate one random vector, classify it with the Bayes classifier (a p-plane), see if it was right or not
estimate_bayes_risk = function() {
  class = sample(c(1, 2), 1)
  one = rnorm(d, means[[class]])
  return(bayes_classifier(one) != class)
}

# Answer
print(mean(replicate(r, estimate_bayes_risk())))
