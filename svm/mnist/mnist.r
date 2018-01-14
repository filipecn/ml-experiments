library("e1071")
source('load.r')
source('../utils.r')
load_mnist()
samples = resampleBase(train$x, train$y, 0.1, c(5, 8))
reduction = reducePCA(samples$X)
samples$X = reduction$x
logMessage("Tunning svm")
set.seed(1)
tuned.svm = tune.svm(x = samples$X, y = as.factor(samples$Y),
	                          degree = c(1,2,3), coef0 = c(0, 1),
														cost = c(10, 50, 100),
														gamma = 1,
														kernel = "polynomial")
logMessage("Trainning svm")
svm.model = svm(x = samples$X, y = as.factor(samples$Y),
									scale = FALSE, kernel = "polynomial",
                  degree = tuned.svm$best.parameters["degree"],
                  cost = tuned.svm$best.parameters["cost"],
                  gamma = tuned.svm$best.parameters["gamma"],
                  coef0 = tuned.svm$best.parameters["coef0"])
logMessage("Computing rho")
w = t(svm.model$coefs) %*% svm.model$SV
w.norm = sqrt(sum(w^2))
rho = 1 / w.norm
print(rho)
logMessage("Computing r")
degree = tuned.svm$best.parameters["degree"]
gamma = tuned.svm$best.parameters["gamma"]
coef0 = tuned.svm$best.parameters["coef0"]
points = gamma * (samples$X %*% t(samples$X) + coef0)^degree
radius = computeRadius(points) / 2
print(degree)
print(coef0)
print(radius)
logMessage("Computing nu")
prediction = predict(svm.model, samples$X, decision.value=T)
xis = attr(prediction, "decision.values")
n = length(xis)
nu = length(which(abs(xis) / w.norm < 1)) / n
print(nu)
print(n)
logMessage("Generalization Bound")
print(nu + sqrt((4 / n) * ((radius / (rho^2)) * log(n*n) + log(1 / 0.01))))
tests = resampleBase(test$x, test$y, 1.0, c(5, 8))
logMessage("Prepare test data")
tests$X = scale(tests$X, reduction$pca$center, reduction$pca$scale) %*% reduction$pca$rotation
tests$X = tests$X[,1:reduction$i]
logMessage("Predicting tests")
prediction = predict(svm.model, tests$X)
logMessage("finished!")
