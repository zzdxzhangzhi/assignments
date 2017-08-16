mysigmoid = function(x) {
  exp(x) / (1 + exp(x))
}

neural.net = function(x, alpha, beta, alpha0) {
  inputs_sum = alpha %*% c(1, x)
  #print(inputs_sum)
  blackbox = beta * mysigmoid(inputs_sum)
  #print(blackbox)
  sum(blackbox) + alpha0
}

mynnet = function(x, npredict) {
  alpha = matrix(rep(runif(length(x) + 1), npredict), nrow = npredict)
  alpha0 = runif(1)
  beta = runif(npredict)
  
  neural.net(x, alpha, beta, alpha0)
}

x = runif(100)
mynnet(x, 13)

library(MASS)
data(Boston)
head(Boston)

samples = data.matrix(Boston[,-ncol(Boston)])
head(samples)

mynnet(samples[1,], 13)

apply(samples, 1, function(x) mynnet(x, 13))


