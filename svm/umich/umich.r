library("e1071")
source('../utils.r')
data = read.table('data.txt', header=FALSE)
sink(file = 'results.txt')
samples = list()
samples$X = data[,1:ncol(data)-1]
samples$Y = data[,ncol(data)]
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

