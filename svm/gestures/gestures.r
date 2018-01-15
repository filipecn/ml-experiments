library("e1071")
source("../utils.r")
data = read.table('data.txt', header=FALSE)
# sink(file = 'results.txt')
samples = list()
samples$X = data[,1:ncol(data)-1]
samples$Y = data[,ncol(data)]
reduction = reducePCA(samples$X)
samples$X = reduction$x
classes = unique(samples$Y)
gbs <- c()
# one-against-one
for(i in 2:5)
	for(j in i+1:5) {
		if(i == j)
			next
		indI = which(samples$Y == classes[i])
		indJ = which(samples$Y == classes[j])
		ind = c(indI, indJ)
		pair = list()
		pair$X = samples$X[ind,]
		pair$Y = samples$Y[ind]
		print(nrow(pair$X))
		print(length(pair$Y))
		# polynomial
		logMessage('Polynomial')
		r = run.experiment(pair, c(1,2,3), c(0,1), c(10,100), 1, "polynomial")
		a = gb.analyze(r$n, r$nu, r$radius, r$rho)
		print(i)
		print(j)
		print(a$gb)
		gbs = rbind(gbs, c(i, j, a$gb, a$n, r$performance))
	}
# sink()
