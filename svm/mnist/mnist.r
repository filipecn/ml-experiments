library("e1071")
source('load.r')
source('../utils.r')
load_mnist()
sink(file = 'results.txt')
samples = resampleBase(train$x, train$y, 0.2, c(5, 8))
reduction = reducePCA(samples$X)
samples$X = reduction$x
# linear
logMessage('Linear')
r = run.experiment(samples, c(1), c(0), c(10,100), 1, "linear")
a = gb.analyze(r$n, r$nu, r$radius, r$rho)
print(a$gb)
print(a$n)
# polynomial
logMessage('Polynomial')
r = run.experiment(samples, c(1,2,3), c(0,1), c(10,100), 1, "polynomial")
a = gb.analyze(r$n, r$nu, r$radius, r$rho)
print(a$gb)
print(a$n)
# gaussian
logMessage('Gaussian')
r = run.experiment(samples, c(1), c(0), c(10,100), 1, "radial")
a = gb.analyze(r$n, r$nu, r$radius, r$rho)
print(a$gb)
print(a$n)
sink()
asdasd
tests = resampleBase(test$x, test$y, 1.0, c(5, 8))
logMessage("Prepare test data")
tests$X = scale(tests$X, reduction$pca$center, reduction$pca$scale) %*% reduction$pca$rotation
tests$X = tests$X[,1:reduction$i]
logMessage("Predicting tests")
prediction = predict(svm.model, tests$X)
logMessage("finished!")
