mysigmoid = function(x) {
  exp(x) / (1 + exp(x))
}

neural.net = function(x, alpha, beta, alpha0) {
  inputs = alpha %*% c(1, x)
  print(inputs)
  sum(beta * mysigmoid(inputs)) + alpha0
}

x = runif(100) * 1000
alpha = runif(101)
alpha0 = 1
beta = runif(6)

x
alpha
alpha0
beta

neural.net(x, alpha, beta, alpha0)
