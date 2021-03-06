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
mynnet(x, 6)

library(MASS)
data(Boston)

samples = data.matrix(Boston[,-ncol(Boston)])
head(samples)
head(apply(samples, 1, function(x) mynnet(x, 6)))

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

used = sample(21613, 10000)
kingCounty.df = input.df[used,]
rownames(kingCounty.df) = 1:10000

#using caret nnet function
library(caret)
my.grid = expand.grid(.decay = c(0.01, 0.001), .size = c(4, 6, 8))
nn.CV = train(log(price) ~ bedrooms + bathrooms
              + sqftlv + sqftlot
              + floors + waterfr + view + cond + grade 
              + sqfta + sqftb + yrb + yrr + lat + long
              + sqftlv15 + sqftlot15, 
              data = kingCounty.df, 
              method = "nnet", 
              maxit = 1000,
              tuneGrid = my.grid,
              trace = FALSE,
              linout = TRUE,
              trControl = trainControl(method = "cv",
                                       number = 5,
                                       repeats = 100))

nn.CV

library(nnet)
nnet.fit = nnet(log(price) ~ bedrooms + bathrooms
                + sqftlv + sqftlot
                + floors + waterfr + view + cond + grade 
                + sqfta + sqftb + yrb + yrr + lat + long
                + sqftlv15 + sqftlot15, 
                data = kingCounty.df, size = 8, 
                linout = TRUE, decay = 0.001, maxit = 1000)

summary(nnet.fit)

RSS = sum(residuals(nnet.fit)^2)
RSS.null = sum((log(input.df$price) - mean(log(input.df$price)))^2)
R2 = 1 - RSS/RSS.null

RSS
RSS.null
R2

#apparent error and the test error
mean(residuals(nnet.fit)^2)

newKingCounty.df = input.df[-used,]
rownames(newKingCounty.df) = 1:11613
predictions = predict(nnet.fit, newdata = newKingCounty.df)

actuals = log(newKingCounty.df$price)
mean((predictions - actuals)^2)

# change to other choices of the parameter
my.grid = expand.grid(.decay = c(0.01, 0.001), .size = c(6, 8, 10))
CV10.NN = train(log(price)~ bedrooms + bathrooms
             + sqftlv + sqftlot + floors 
             + waterfr + view + cond + grade 
             + sqfta + sqftb + yrb + yrr + lat + long
             + sqftlv15 + sqftlot15,
             data = kingCounty.df,
             method = "nnet", 
             maxit = 1000,
             tuneGrid = my.grid,
             trace = FALSE,
             linout = TRUE,
             trControl = trainControl(method = "cv", 
                                      number = 10, 
                                      repeats = 100))
CV10.NN

boot.NN = train(log(price)~ bedrooms + bathrooms
             + sqftlv + sqftlot + floors 
             + waterfr + view + cond + grade 
             + sqfta + sqftb + yrb + yrr + lat + long
             + sqftlv15 + sqftlot15,
             data = kingCounty.df,
             method = "nnet", 
             maxit = 1000,
             tuneGrid = my.grid,
             trace = FALSE,
             linout = TRUE,
             trControl = trainControl(method = "boot", 
                                      repeats = 200))
boot.NN

library(rpart)
CV10.rpart = train(price ~ bedrooms + bathrooms
                   + sqftlv + sqftlot + floors 
                   + waterfr + view + cond + grade 
                   + sqfta + sqftb + yrb + yrr + lat + long
                   + sqftlv15 + sqftlot15,
                   data = kingCounty.df,
                   method = "rpart", 
                   tuneLength = 200,
                   trControl = trainControl(method = "cv", 
                                            number = 5, 
                                            repeats = 100))
CV10.rpart

boot.rpart = train(price~ bedrooms + bathrooms
                   + sqftlv + sqftlot + floors 
                   + waterfr + view + cond + grade 
                   + sqfta + sqftb + yrb + yrr + lat + long
                   + sqftlv15 + sqftlot15,
                   data = kingCounty.df,
                   method = "rpart", 
                   tuneLength = 200,
                   trControl = trainControl(method = "boot", 
                                            repeats = 200))
boot.rpart

tree.fit = rpart(price ~ bedrooms + bathrooms
                 + sqftlv + sqftlot + floors 
                 + waterfr + view + cond + grade 
                 + sqfta + sqftb + yrb + yrr + lat + long
                 + sqftlv15 + sqftlot15, 
                 data = kingCounty.df,
                 cp = 0.00019, 
                 minsplit = 7)

plotcp(tree.fit)
abline(v = 7, lty = 2, col = "blue", lwd = 2)
abline(v = 40, lty = 2, col = "blue", lwd = 2)

printcp(tree.fit)

RSS.tree = sum(residuals(tree.fit)^2)
RSS.tree

mean(residuals(tree.fit)^2)

newKingCounty.df = input.df[-used,]
rownames(newKingCounty.df) = 1:11613
predictions = predict(tree.fit, newdata = newKingCounty.df)

actuals = newKingCounty.df$price
mean((predictions - actuals)^2)

