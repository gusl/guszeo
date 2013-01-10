momentk <- function(k) function(n){
  f <- function(x) x^k * dnorm(x, sd=1/sqrt(n))
  integrate(f, lower=-10/sqrt(n), upper=10/sqrt(n))
}

fourthMoment <- momentk(4)
sixthMoment <- momentk(6)

fourthMoment(1)
fourthMoment(2)




n <- 500

fourthMoments <- sapply(1:n, fourthMoment)
plot(1:n, fourthMoments, type="l")
pplot(1:n, sapply(1:n, function (x) 2/x^2), col="red", type="l")
pplot(1:n, sapply(1:n, function (x) 2/x), col="blue", type="l")


sixthMoments <- sapply(1:n, sixthMoment)
plot(1:n, sixthMoments, type="l")


###############

n <- 100000
X <- runif(n)
Y <- n*sort(X)
Z <- Y[2:n]-Y[1:(n-1)]
plot(density(Z))

