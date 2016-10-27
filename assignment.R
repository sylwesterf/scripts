spot <- 100
strike <- seq(80,125,5)
t.exp <- 1.2
sigma <- 0.2
r <- 0.05
L <- 1

#library(devtools)
#source_url('https://github.com/jaehyukchoi/phbs.asp.2016/blob/master/BlackScholes/bsm_price.R')
#source_url('https://github.com/jaehyukchoi/phbs.asp.2016/blob/master/BlackScholes/normal_impvol.R')

#source('bsm_price.R')
#source('normal_impvol.R')

#Assignment 1

price <- CalcBsmPrice(spot=spot, t.exp = t.exp, sigma=sigma, strike=strike, r=r)
impvol <- CalcNormalImpvol(price=price, spot=spot, strike=strike, t.exp=t.exp, r=r)

plot( price, impvol,
      type="b", xlab = "price", ylab = "implied volatility",
      col="blue", lwd = 3,
      main = paste("normal implied volatility of BS prices"))
lm(impvol ~ price)$coefficients[2]


#Assignement 2

#library(devtools)
#source_url('https://github.com/jaehyukchoi/phbs.asp.2016/blob/master/BlackScholes/bsm_impvol.R')

#source('bsm_impvol.R')

CalcDBsmPrice <- function(
  type = 'call', spot, forward = spot*exp((r-div)*t.exp),
  strike = forward, t.exp = 1, r = 0, div = 0, sigma,L=0
){
  stdev <- sigma*sqrt(t.exp)
  d1 <- log(forward/strike)/stdev +0.5*stdev
  d2 <- d1 - stdev
  disc.factor <- exp(-r*t.exp)
  
  pnorm.d1 <- pnorm(d1)
  pnorm.d2 <- pnorm(d2)
  
  if (type == "call" ){
    price <- forward*pnorm.d1 - strike*pnorm.d2
    delta <- pnorm.d1
  }else if (type == "put"){
    price <- strike*(1-pnorm.d2) - forward*(1-pnorm.d1)
    delta <- pnorm.d1 - 1
  }else if (type == "straddle"){
    price <- forward*(2*pnorm.d1 - 1) - strike*(2*pnorm.d2 - 1)
    delta <- 2*pnorm.d1 - 1
  }else if (type == "digit"){
    price <- pnorm.d2
    delta <- 1/sqrt(2*pi)*exp(-d2^2/2)/(s*stdev)
  }
  displaced.price <- price + L
  return( disc.factor * displaced.price )
}

priceD <- CalcDBsmPrice(spot=spot, t.exp = t.exp, sigma=sigma, strike=strike, r=r, L=0)
impvolD <- CalcBsmImpvol(price=priceD, spot=spot, strike=strike, t.exp=t.exp, r=r) 
