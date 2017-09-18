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

voiceURL = "https://www.stat.auckland.ac.nz/~lee/784/Assignments/voice.csv"
input.df = read.csv(voiceURL, header = TRUE)
head(input.df)

apply(input.df, 2, function(x) sum(is.na(x)))

library(caret)
input.df$label = factor(input.df$label)
numericCols = unlist(lapply(input.df, is.numeric))
dataCont = input.df[, numericCols]
dataFactor = as.data.frame(input.df[, !numericCols])
names(dataFactor) = names(input.df)[!numericCols]
trans = preProcess(dataCont, 
                   method = c("BoxCox", "center", "scale"))
dataTransScaleCont = predict(trans, dataCont)
dataTransScaled = data.frame(
  as.data.frame(scale(dataTransScaleCont)), 
  dataFactor)
str(dataTransScaled)

# feature.trans = function(feature) {
#   trans = predict(BoxCoxTrans(feature), feature)
#   par(mfrow = c(1,2))
#   hist(feature, main = "Untransformed", nclass = 50)
#   hist(trans, main = "Transformed", nclass = 50)
#   if (any(abs(feature - trans) > 1e-07)) 
#     TRUE
#   else 
#     FALSE
# }

# apply(dataScaled[, numericCols], 2, feature.trans)

colnum = ncol(dataTransScaled)
cors = cor(dataTransScaled[, 1:(colnum - 1)])[1:(colnum - 2), colnum - 1]
par(mfrow = c(1, 1))
barplot(abs(cors), col = "red")

library(corrplot)
correlations = cor(dataTransScaled[, 1:(colnum - 1)])
corrplot(correlations)
round(correlations, 2)
removed.cols = findCorrelation(correlations)
removed.cols
nearZeroVar(dataTransScaled)

dataProcessed.df = dataTransScaled[, -removed.cols]
str(dataProcessed.df)

# pick 70% data as training set
use = sample(dim(dataProcessed.df)[1], 0.7 * dim(dataProcessed.df)[1])
dataProcessed.training = dataProcessed.df[use,]
dataProcessed.test = dataProcessed.df[-use,]

gender.logistic = glm(label ~ ., data = dataProcessed.training, family = binomial)
coef(gender.logistic)
probs = predict(gender.logistic, type = "response")
logistic.table = table(dataProcessed.training$label, probs > 0.5)
logistic.table

class.sensi(logistic.table)
class.speci(logistic.table)
total.error(logistic.table)

library(R330)
logistic.cv10 = cross.val(gender.logistic, nfold = 10)
logistic.cv10.error = 1 - mean(logistic.cv10$Correct)
logistic.cv10.error

err.boot(gender.logistic, B = 200)

logistic.CV5 = train(
  label ~ .,
  data = dataProcessed.training,
  method = "glm",
  trControl = trainControl(
    method = "cv",
    number = 5,
    repeats = 20
  )
)

logistic.CV5
1 - logistic.CV5$results$Accuracy

logistic.boot = train(
  label ~ .,
  data = dataProcessed.training,
  method = "glm",
  trControl = trainControl(method = "boot",
                           repeats = 200)
)

logistic.boot
1 - logistic.boot$results$Accuracy

probs.test = predict(gender.logistic, 
                     type = "response", 
                     newdata = dataProcessed.test)
logistic.table.test = table(dataProcessed.test$label,
                            probs.test > 0.5)
logistic.table.test

class.sensi(logistic.table.test)
class.speci(logistic.table.test)
total.error(logistic.table.test)

library(MASS)
gender.lda = lda(label ~ ., data = dataProcessed.training)
lda.prediction = predict(gender.lda)
predClasses = lda.prediction$class
head(predClasses)
lda.probs = lda.prediction$posterior
head(lda.probs)

ldaTable = table(predClasses, dataProcessed.training$label)
ldaTable

class.sensi(ldaTable)
class.speci(ldaTable)
total.error(ldaTable)

lda.CV10 = train(
  label ~ .,
  data = dataProcessed.training,
  method = "lda",
  trControl = trainControl(
    method = "cv",
    number = 10,
    repeats = 20
  )
)

lda.CV10
1 - lda.CV10$results$Accuracy

lda.boot = train(
  label ~ .,
  data = dataProcessed.training,
  method = "lda",
  trControl = trainControl(method = "boot",
                           repeats = 200)
)

lda.boot
1 - lda.boot$results$Accuracy

prediction.test = predict(gender.lda,
                          newdata = dataProcessed.test)
predClasses.test = prediction.test$class
lda.probs.test = prediction.test$posterior
lda.table.test = table(predClasses.test,
                       dataProcessed.test$label)
lda.table.test

class.sensi(lda.table.test)
class.speci(lda.table.test)
total.error(lda.table.test)

gender.qda = qda(label ~ ., data = dataProcessed.training)
qda.prediction = predict(gender.qda)
predClasses = qda.prediction$class
head(predClasses)
qda.probs = qda.prediction$posterior
head(qda.probs)

qdaTable = table(predClasses, dataProcessed.training$label)
qdaTable

class.sensi(qdaTable)
class.speci(qdaTable)
total.error(qdaTable)

qda.CV10 = train(
  label ~ .,
  data = dataProcessed.training,
  method = "qda",
  trControl = trainControl(
    method = "cv",
    number = 10,
    repeats = 20
  )
)

qda.CV10
1 - qda.CV10$results$Accuracy

qda.boot = train(
  label ~ .,
  data = dataProcessed.training,
  method = "qda",
  trControl = trainControl(method = "boot",
                           repeats = 200)
)

qda.boot
1 - qda.boot$results$Accuracy

prediction.test = predict(gender.qda,
                          newdata = dataProcessed.test)
predClasses.test = prediction.test$class
qda.probs.test = prediction.test$posterior
qda.table.test = table(predClasses.test,
                       dataProcessed.test$label)
qda.table.test

class.sensi(qda.table.test)
class.speci(qda.table.test)
total.error(qda.table.test)

library(class)
colnum = ncol(dataProcessed.training)
dataProcessed.knn = data.frame(dataProcessed.training[,1:(colnum - 1)], 
                               label = as.numeric(dataProcessed.training[, colnum]))
knn.prediction = knn(dataProcessed.knn, 
                     dataProcessed.knn,
                     dataProcessed.knn$label,
                     k = 5)

knnTable = table(dataProcessed.training$label, knn.prediction)
knnTable

class.sensi(knnTable)
class.speci(knnTable)
total.error(knnTable)

knn.CV10 = train(
  label ~ .,
  data = dataProcessed.training,
  method = "knn",
  trControl = trainControl(
    method = "cv",
    number = 10,
    repeats = 20
  )
)

knn.CV10
1 - knn.CV10$results$Accuracy

knn.boot = train(
  label ~ .,
  data = dataProcessed.training,
  method = "knn",
  trControl = trainControl(method = "boot",
                           repeats = 200)
)

knn.boot
1 - knn.boot$results$Accuracy

dataProcessed.knn.test = data.frame(dataProcessed.test[, 1:(colnum - 1)],
                                    label = as.numeric(dataProcessed.test[, colnum]))
knn.prediction.test = knn(dataProcessed.knn,
                          dataProcessed.knn.test,
                          dataProcessed.knn$label,
                          k = 5)

knnTable.test = table(dataProcessed.test$label, knn.prediction.test)
knnTable.test

class.sensi(knnTable.test)
class.speci(knnTable.test)
total.error(knnTable.test)

library(mgcv)
gender.gam = gam(label ~ s(sd) + s(median) + s(Q25)
                 + s(Q75) + s(IQR) + s(kurt) + s(sp.ent)
                 + s(sfm) + s(mode) + s(meanfun) + s(minfun)
                 + s(maxfun) + s(meandom) + s(mindom)
                 + s(dfrange) + s(modindx), 
                 data = dataProcessed.training, 
                 family = binomial)
par(mfrow = c(2, 2))
plot(gender.gam)
gam.probs = predict(gender.gam, type = "response")
gam.table = table(dataProcessed.training$label, gam.probs > 0.5)
gam.table

class.sensi(gam.table)
class.speci(gam.table)
total.error(gam.table)

gam.CV10 = train(
  label ~ .,
  data = dataProcessed.training,
  method = "gam",
  trControl = trainControl(
    method = "cv",
    number = 10,
    repeats = 20
  )
)

gam.CV10
1 - gam.CV10$results$Accuracy

gam.boot = train(
  label ~ .,
  data = dataProcessed.training,
  method = "gam",
  trControl = trainControl(method = "boot",
                           repeats = 200)
)

gam.boot
1 - knn.boot$results$Accuracy

gam.probs.test = predict(gender.gam,
                         type = "response",
                         newdata = dataProcessed.test)
gam.table.test = table(dataProcessed.test$label,
                       gam.probs.test > 0.5)
gam.table.test

class.sensi(gam.table.test)
class.speci(gam.table.test)
total.error(gam.table.test)

my.grid = expand.grid(.decay = c(0.01, 0.001), .size = c(4, 6, 8))
nn.CV = train(label ~ ., 
              data = dataProcessed.training, 
              method = "nnet", 
              maxit = 1000,
              tuneGrid = my.grid,
              trace = FALSE,
              trControl = trainControl(method = "cv",
                                       number = 10,
                                       repeats = 100))

nn.CV
1 - nn.CV$results$Accuracy[nn.CV$results$size == 8 & nn.CV$results$decay == 0.01]

library(nnet)
nnet.fit = nnet(label ~ ., 
                data = dataProcessed.training, 
                size = 8,
                decay = 0.01, 
                maxit = 1000)

summary(nnet.fit)

nnet.probs = predict(nnet.fit)
nnet.table.training = table(dataProcessed.training$label, 
                            nnet.probs > 0.5)
nnet.table.training

class.sensi(nnet.table.training)
class.speci(nnet.table.training)
total.error(nnet.table.training)

nnet.probs.test = predict(nnet.fit, newdata = dataProcessed.test)
nnet.table.test = table(dataProcessed.test$label,
                        nnet.probs.test > 0.5)
nnet.table.test

class.sensi(nnet.table.test)
class.speci(nnet.table.test)
total.error(nnet.table.test)

library(rpart)
CV10.rpart = train(label ~ .,
                   data = dataProcessed.training,
                   method = "rpart", 
                   tuneLength = 200,
                   trControl = trainControl(method = "cv", 
                                            number = 10, 
                                            repeats = 100))
CV10.rpart
1 - CV10.rpart$results$Accuracy[abs(CV10.rpart$results$cp - 0.004579055) < 1e-09]

boot.rpart = train(label ~ .,
                   data = dataProcessed.training,
                   method = "rpart", 
                   tuneLength = 200,
                   trControl = trainControl(method = "boot", 
                                            repeats = 200))
boot.rpart
1 - boot.rpart$results$Accuracy[abs(boot.rpart$results$cp - 0.004579055) < 1e-09]

tree.fit.training = rpart(
  label ~ .,
  data = dataProcessed.training,
  method = "class",
  parms = list(split = "gini"),
  cp = 0.0046
)

par(mfrow = c(1, 1))
plotcp(tree.fit.training)
plot(tree.fit.training)
text(tree.fit.training, cex = 0.7)
printcp(tree.fit.training)

tree.probs = predict(tree.fit.training)
tree.table = table(dataProcessed.training$label, tree.probs[,2] > 0.5)
tree.table

class.sensi(tree.table)
class.speci(tree.table)
total.error(tree.table)

tree.probs.test = predict(tree.fit.training, newdata = dataProcessed.test)
tree.table.test = table(dataProcessed.test$label, tree.probs.test[,2] > 0.5)
tree.table.test

class.sensi(tree.table.test)
class.speci(tree.table.test)
total.error(tree.table.test)

library(kernlab)
svm.stuff1.training = ksvm(
  label ~ ., 
  data = dataProcessed.training,
  C = 1,
  cross = 5
)
svm.stuff2.training = ksvm(
  label ~ ., 
  data = dataProcessed.training,
  C = 2,
  cross = 10
)
svm.stuff5.training = ksvm(
  label ~ ., 
  data = dataProcessed.training,
  C = 5,
  cross = 10
)

svm.stuff1.training
svm.stuff2.training
svm.stuff5.training

svm.probs.test = predict(svm.stuff5.training, newdata = dataProcessed.test)
svm.table.test = table(dataProcessed.test$label, svm.probs.test == "male")
svm.table.test

class.sensi(svm.table.test)
class.speci(svm.table.test)
total.error(svm.table.test)

library(mboost)
colnum = ncol(dataProcessed.training)
boost.tree = label ~ btree(dataProcessed.training[, -colnum],
                     tree_controls = party::ctree_control(maxdepth = 4))
gender.boost = mboost(
  boost.tree, 
  data = dataProcessed.training,
  family = Binomial(),
  control = boost_control(mstop = 250, nu = 0.1)
)

myrisk = cvrisk(gender.boost)
myrisk

boost.probs = predict(gender.boost)
boost.table = table(dataProcessed.training$label,
                    boost.probs > 0.5)
boost.table

class.sensi(boost.table)
class.speci(boost.table)
total.error(boost.table)

boost.probs.test = predict(gender.boost, newdata = dataProcessed.test)
boost.table.test = table(dataProcessed.test$label,
                    boost.probs.test > 0.5)
boost.table.test

class.sensi(boost.table.test)
class.speci(boost.table.test)
total.error(boost.table.test)

library(randomForest)
gender.rf = randomForest(
  label ~ ., 
  data = dataProcessed.training,
  mtry = 10,
  ntree = 1000
)

plot(gender.rf)
tail(gender.rf$err.rate, n = 2)

rf.probs = predict(gender.rf)
rf.table = table(dataProcessed.training$label,
                 rf.probs)
rf.table

class.sensi(boost.table)
class.speci(boost.table)
total.error(boost.table)

rf.probs.test = predict(gender.rf, newdata = dataProcessed.test)
rf.table.test = table(dataProcessed.test$label,
                      rf.probs.test)
rf.table.test

class.sensi(rf.table.test)
class.speci(rf.table.test)
total.error(rf.table.test)

# ridge fit
library(glmnet)
colnum = ncol(dataProcessed.training)
X = as.matrix(dataProcessed.training[, -colnum])
y = dataProcessed.training$label
lambda.seq = seq(0.1, 0.001, length = 100)

gender.ridge = glmnet(X, y, alpha = 0, lambda = lambda.seq, family = "binomial")
ridgecv = cv.glmnet(X, y, lambda = lambda.seq, family = "binomial")
plot(ridgecv)
Pred.err.cv = ridgecv$cvm[ridgecv$lambda == ridgecv$lambda.min]
Pred.err.cv

ridge.probs = predict(gender.ridge, X, type = "response")
ridge.table = table(dataProcessed.training$label,
                    ridge.probs[, length(ridgecv$cvm)] > 0.5)
ridge.table

class.sensi(ridge.table)
class.speci(ridge.table)
total.error(ridge.table)

newX = as.matrix(dataProcessed.test[, -colnum])
ridge.probs.test = predict(gender.ridge, newX, type = "response")
ridge.table.test = table(dataProcessed.test$label,
                    ridge.probs.test[, length(ridgecv$cvm)] > 0.5)
ridge.table.test

class.sensi(ridge.table.test)
class.speci(ridge.table.test)
total.error(ridge.table.test)

# lasso fit
gender.lasso = glmnet(X, y, alpha = 1, family = "binomial")
lassocv = cv.glmnet(
  X,
  y,
  lambda = gender.lasso$lambda,
  family = "binomial",
  type.measure = "class"
)

plot(lassocv)
lasso.err = lassocv$cvm[lassocv$lambda ==
                          lassocv$lambda.min]
lasso.err

lasso.probs = predict(gender.lasso, X)
lasso.table = table(dataProcessed.training$label,
                    lasso.probs[, length(lassocv$cvm)] > 0.5)
lasso.table

class.sensi(lasso.table)
class.speci(lasso.table)
total.error(lasso.table)

newX = as.matrix(dataProcessed.test[, -colnum])
lasso.probs.test = predict(gender.lasso, newX)
lasso.table.test = table(dataProcessed.test$label,
                         lasso.probs.test[, length(lassocv$cvm)] > 0.5)
lasso.table.test

class.sensi(lasso.table.test)
class.speci(lasso.table.test)
total.error(lasso.table.test)
