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

samples = data.matrix(Boston[,-ncol(Boston)])
head(samples)
head(apply(samples, 1, function(x) mynnet(x, 13)))

URL = "https://www.stat.auckland.ac.nz/~lee/784/Assignments/kc_house_data.csv"
input.df = read.csv(URL)
names(input.df) = c("id", "date", "price", "bedrooms", "bathrooms",
                    "sqftlv", "sqftlot", "floors", "waterfr", "view",
                    "cond", "grade", "sqfta", "sqftb", "yrb", "yrr",
                    "zip", "lat", "long", "sqftlv15", "sqftlot15")
head(input.df)

input.df$waterfr = factor(input.df$waterfr)
input.df$view = factor(input.df$view)
str(input.df)

origInput.df = data.frame(input.df)

library(caret)
trans = BoxCoxTrans(input.df$price)
trans

transPrice = predict(trans, input.df$price)
par(mfrow = c(1, 2))
hist(input.df$price, main = "Untransformed", nclass = 30)
hist(transPrice, main = "Transformed", nclass = 30)

input.df$price = transPrice

trans = BoxCoxTrans(input.df$sqftlv)
transSqftlv = predict(trans, input.df$sqftlv)
input.df$sqftlv = transSqftlv
trans = BoxCoxTrans(input.df$sqftlot)
transSqftlot = predict(trans, input.df$sqftlot)
input.df$sqftlot = transSqftlot
trans = BoxCoxTrans(input.df$sqftlv15)
transSqftlv15 = predict(trans, input.df$sqftlv15)
input.df$sqftlv15 = transSqftlv15
trans = BoxCoxTrans(input.df$sqftlot15)
transSqftlot15 = predict(trans, input.df$sqftlot15)
input.df$sqftlot15 = transSqftlot15

library(nnet)
nnet.fit = nnet(log(price) ~ bedrooms + bathrooms
                + sqftlv + sqftlot
                + floors + waterfr + view + cond + grade 
                + sqfta + sqftb + yrb + yrr + lat + long
                + sqftlv15 + sqftlot15, 
                data = input.df, size = 13, 
                linout = TRUE, decay = 0.01, maxit = 500)

summary(nnet.fit)

RSS = sum(residuals(nnet.fit)^2)
RSS.null = sum((log(input.df$price) - mean(log(input.df$price)))^2)
R2 = 1 - RSS/RSS.null

RSS
RSS.null
R2
