logMessage <- function(m) {
	cat (as.character.Date(Sys.time()), m, '\n')
}

resampleBase <- function(X, Y, size = 0.1, cl = c()) {
	logMessage("Sampling data")
	ret = list()
	ret$X = c()
	ret$Y = c()
	classes = unique(Y)
	if(length(cl) > 0)
		classes = cl
	for(c in classes) {
		indices = which(Y == c)
		cat(length(indices), " instances of class ", c, "\n")
		samples = sample(indices, min(length(indices), round(length(indices) * size)))
		ret$X = matrix(c(ret$X, X[samples, ]), ncol = ncol(X))
    ret$Y = c(ret$Y, Y[samples])
	}
	cat("From ", nrow(X), " samples to ")
	cat(nrow(ret$X), " samples.\n")
	ret
}

reducePCA <- function(X, info = 0.85) {
	logMessage("Reducting via PCA")
	print(ncol(X))
	pca = prcomp(X)
	cuminfo = cumsum(pca$sdev/sum(pca$sdev))
	i = max(length(which(cuminfo <= info)), 2)
	print(i)
	print(cuminfo)
	ret = list()
	ret$i = i
  ret$x = pca$x[,1:i]
	ret$pca = pca
	print(ncol(ret$x))
	ret
}

computeRadius <- function(points) {
	radius = 0.0
	for (i in 1:nrow(points))
    for (j in 1:nrow (points))
			radius = max(radius, sqrt(t(points[i, ]) %*% points[j, ]))
	radius
}

run.experiment <- function(samples, tune.degree = c(1,2,3), tune.coef0 = c(0,1),
													 tune.cost = c(10, 100), tune.gamma = 1,
													 tune.kernel = "polynomial") {
	logMessage("Tunning svm")
	set.seed(1)
	tuned.svm = tune.svm(x = samples$X, y = as.factor(samples$Y),
		                          degree = tune.degree,
															coef0 = tune.coef0,
															cost = tune.cost,
															gamma = tune.gamma,
															kernel = tune.kernel)
	logMessage("Trainning svm")
	svm.model = svm(x = samples$X, y = as.factor(samples$Y),
										scale = FALSE, kernel = tune.kernel,
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
	logMessage("finished!")
	ret = list()
	ret$radius = radius
	ret$n = n
	ret$rho = rho
	ret$nu = nu
	ret
}

gb.analyze <- function(n, nu, radius, rho) {
	ret = list()
	ret$gb = nu + sqrt((4 / n) * ((radius / (rho^2)) * log(n*n) + log(1 / 0.01)))
	ret$n = 0
	new.n = n
	gb = ret$gb
	while(gb > 0.01) {
		new.n = new.n * 10
		gb = nu + sqrt((4 / new.n) * ((radius / (rho^2)) * log(new.n*new.n) + log(1 / 0.01)))
	}
	ret$n = new.n
	ret
}
