# # #
#cholesky decomposition - parametrs

#n          is the number of random numbers you want to generate
#n.asset    is the number of assets in portfolio
#rho        is the sensitivity of an option
#sigma.gbm  is volatility under GBM

n <- 1e6
n.asset <- 4
rho <- 0.5
sigma.gbm <- 0.5 

#cholesky decomposition
#cov.mat    is a covariance matrix 
cov.mat <- sigma.gbm*(diag(n.asset)+(1-rho))

#cov.chol   is a lower triangular matrix used for Cholesky decomposition
cov.chol <- t(chol(cov.mat)) 
#sanity check
#all(round(cov.chol %*% t(cov.chol), 10) == round(cov.mat, 10))

#generation of the (n)x(n.asset) correlated normal RNs using a normal distibution N(0,1)
#rn         is a (n)x(n.asset) matrix containing random numbers; rn~N(0,1)
rn <- matrix(rnorm(n*n.asset), nrow=n.asset)

#rn.corr    is a (n)x(n.asset) matrix containing correlated random numbers
rn.corr <- cov.chol %*% rn

# # #
#path generation - parameters

#K          is spot price
#sigma.n    is volatility under NM
#t.exp      is time to expiry
#weights    is a vector containing assets' weights
#r          is the risk-free rate

t.exp <- 1
sigma.gbm <- 0.5
K <- 100
sigma.n <- sigma.gbm * K   #put in function inputs as equation (default)
weights <- rep(1/n.asset, n.asset)
r <- 0

#path generation

#path.gbm   is the simulated path of the GBM
path.gbm <- K * exp(sigma.gbm*sqrt(t.exp)*rn.corr - 0.5*(sigma.gbm^2)*t.exp)

#some plots
# cpath.gbm <- cbind(rep(100,4),path.gbm)
# plot(cpath.gbm[1,], type ="l", col=1, lty=1, main="GBM Paths", ylab="Price")
# lines(cpath.gbm[2,], type ="l", col=2)
# lines(cpath.gbm[3,], type ="l", col=3)
# lines(cpath.gbm[4,], type ="l", col=4)

#path.nm    is the simulated path of the NM
path.nm <- K + sigma.n*sqrt(t.exp)*rn.corr

#some plots
# cpath.nm <- cbind(rep(100,4),path.nm)
# plot(cpath.nm[1,], type ="l", col=1, lty=1, main="NM Paths", ylab="Price")
# lines(cpath.nm[2,], type ="l", col=2)
# lines(cpath.nm[3,], type ="l", col=3)
# lines(cpath.nm[4,], type ="l", col=4)

# cor(path.nm[1,], path.nm[2,])
# cor(path.nm[1,], path.gbm[1,])

#option pricing - price baskets
price.basket.gbm <- t(weights %*% path.gbm)
price.basket.nm <- t(weights %*% path.nm)

#price of the option following the GBM
price.gbm <- sum(exp(-r*t.exp)*pmax(price.basket.gbm-K,0))/n

#price of the option following the NM
price.nm <- sum(exp(-r*t.exp)*pmax(price.basket.nm-K,0))/n


## Analytic Pricing
var.basket <- as.vector( t(weights) %*% cov.mat %*% weights )
price.bs <- sqrt(var.basket) * sqrt(1/2/pi)

