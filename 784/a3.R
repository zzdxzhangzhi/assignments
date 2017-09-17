library(ISLR)
data(Default)
default.logistic = glm(default ~ student + balance + income,
                       data = Default, 
                       family = binomial)
coef(default.logistic)

probs = predict(default.logistic, type = "response")
my.table = table(Default$default, probs > 0.5)

class.sensi = function(table) {
  table[2,2] / sum(table[2,])
}

class.speci = function(table) {
  table[1,1] / sum(table[1,])
}

total.error = function(table) {
  1 - sum(diag(table)) / sum(table)
}

total.error(my.table)
class.sensi(my.table)
class.speci(my.table)

my.table = table(Default$default, probs > 0.1)
total.error(my.table)
class.sensi(my.table)
class.speci(my.table)

library(pROC)
rocCurve = roc(response = Default$default,
               predictor = probs,
               levels = rev(levels(Default$default)))

auc(rocCurve)
ci.auc(rocCurve)

par(mfrow = c(1,2))
plot.roc(rocCurve)
plot(rocCurve, legacy.axes = TRUE)
