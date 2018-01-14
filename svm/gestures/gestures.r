library("e1071")
source("../utils.r")
data = read.table('data.txt', header=FALSE)
samples = list()
samples$X = data[,1:ncol(data)-1]
samples$Y = data[,ncol(data)]
reduction = reducePCA(samples$X)
samples$X = reduction$x
r = run.experiment(samples, c(1), c(0), c(10), 1, "polynomial")
a = gb.analyze(r$n, r$nu, r$radius, r$rho)
print(a$gb)
print(a$n)
