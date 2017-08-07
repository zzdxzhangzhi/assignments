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

kingCounty = lm(log(price)~ bedrooms + bathrooms
			+ sqftlv + sqftlot
			+ floors + waterfr + view + cond + grade 
			+ sqfta + sqftb + yrb + yrr + lat + long
			+ sqftlv15 + sqftlot15, 
			data = kingCounty.df)
#kingCounty = lm(log(price)~ . - id - date - zip,
			data = kingCounty.df)

#kingCounty = lm(log(transPrice[1:10000])~ bedrooms + bathrooms
#			+ sqftlv + sqftlot + floors 
#			+ waterfr + view + cond + grade 
#			+ sqfta + yrb + lat + long
#			+ sqftlv15 + sqftlot15,
#			data = kingCounty.df)

mean(residuals(kingCounty)^2)

newKingCounty.df = input.df[-used,]
rownames(newKingCounty.df) = 1:11613
predictions = predict(kingCounty, newdata = newKingCounty.df)
#actuals = log(transPrice[10001:21613])
actuals = log(newKingCounty.df$price)
mean((predictions - actuals)^2)

library(R330)
cross.val(kingCounty, nfold = 10, nrep = 20)

####################################################################
cross.val.mod <- function (f, nfold = 10, nrep = 20, ...){
    X <- model.matrix(f$terms, model.frame(f))
    y = fitted.values(f) + residuals(f)
    n <- dim(X)[1]
    CV <- numeric(nrep)
    pred.error <- numeric(nfold)
    m <- n%/%nfold
    for (k in 1:nrep) {
        rand.order <- order(runif(n))
        yr <- y[rand.order]
        Xr <- X[rand.order, ]
        sample <- 1:m
        for (i in 1:nfold) {
              use.mat <- as.matrix(Xr[-sample,])
              test.mat <- as.matrix(Xr[sample,])
              y.use = yr[-sample]
              new.data <- data.frame(test.mat)
              fit <- lm(y.use ~ -1+use.mat)
              my.predict = test.mat%*%coefficients(fit) 
              pred.error[i] <- sum((yr[sample] - my.predict)^2)/m
              sample <- if(i==nfold) (max(sample)+1):n else sample + m

            }
            CV[k] <- mean(pred.error)
        }
mean(CV)
}
############################################# 

cvvec = 1:20
for (i in 1:20) {
	cvvec[i] = cross.val.mod(kingCounty, nfold = 10, nrep = 1)
}
mean(cvvec)
sd(cvvec)

err.boot(kingCounty, B = 50)

library(bootstrap)
theta.fit = function(x, y){lsfit(x, y)}
theta.predict = function(fit, x){cbind(1, x) %*% fit$coef}
sq.err = function(y, yhat) {(y - yhat)^2}

#y = log(transPrice[1:10000])
y = log(kingCounty.df[,3])
x = data.matrix(kingCounty.df[,-c(1:3, 17)])
cv10err = crossval(x, y, theta.fit, theta.predict, ngroup = 10)
cv10 = mean((y - cv10err$cv.fit)^2)
cv10

boot = bootpred(x, y, nboot = 200, 
		theta.fit, theta.predict, 
		err.meas = sq.err)
bootopt = boot[[1]] + boot[[2]]
bootopt
boot632 = boot[[3]]
boot632

library(caret)

#kingCounty.df$price = transPrice[1:10000]

CV10 = train(log(price)~ bedrooms + bathrooms
			+ sqftlv + sqftlot + floors 
			+ waterfr + view + cond + grade 
			+ sqfta + sqftb + yrb + yrr + lat + long
			+ sqftlv15 + sqftlot15,
		data = kingCounty.df,
		method = "lm",
		trControl = trainControl(method = "cv", number = 10, 
						repeats = 20))
CV10

boot = train(log(price)~ bedrooms + bathrooms
			+ sqftlv + sqftlot + floors 
			+ waterfr + view + cond + grade 
			+ sqfta + sqftb + yrb + yrr + lat + long
			+ sqftlv15 + sqftlot15,
		data = kingCounty.df,
		method = "lm",
		trControl = trainControl(method = "boot", 
						repeats = 200))
boot

null.model = lm(log(price)~1, data = kingCounty.df)
selected = step(null.model, scope = formula(kingCounty),
		direction = "forward", trace = 0)
selected

selected = step(kingCounty, scope = formula(kingCounty),
		direction = "backward", trace = 0)
selected

selected = step(kingCounty, scope = formula(kingCounty),
		direction = "both", trace = 0)
selected

allpossregs(kingCounty)

kingCounty.df = origInput.df[used,]
rownames(kingCounty.df) = 1:10000

null.model = lm(log(price)~1, data = kingCounty.df)
selected = step(null.model, scope = formula(kingCounty),
		direction = "forward", trace = 0)
selected

selected = step(kingCounty, scope = formula(kingCounty),
		direction = "backward", trace = 0)
selected

selected = step(kingCounty, scope = formula(kingCounty),
		direction = "both", trace = 0)
selected

allpossregs(kingCounty)
