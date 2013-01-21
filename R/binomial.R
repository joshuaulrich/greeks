#### TODO
### helpers to move:



fischer.skewness <- function(r) {
  # r: lognormal asset returns
  
}  

# add in test results from Haug worksheet output as 'correct', use to verify this code
CRRtree_discrete_dividends <- function() {}
JRtree_discrete_dividends <- function() {}
LRtree_discrete_dividends <- function() {}

JarrowRuddSkewKurtosis <- function(type=c("call","put"),stock=42,strike=40,r=0.1,b=0.1,sigma=0.2,T=0.5,skew,kurt) {
  d1 <- ( log(stock/strike) + (r + sigma^2/2)*T )/ (sigma*sqrt(T))
  d2 <- d1 - sigma*sqrt(T)
  aX <- (strike * sigma * sqrt(T*2*pi))^-1 * exp(-d2^2 / 2)
  daX <- aX * (d2 - sigma * sqrt(T)) / (strike* sigma * sqrt(T))
  daXX <- aX / (strike^2 * sigma * sqrt(T)) * ((d2 - sigma * sqrt(T))^2 - sigma * sqrt(T) * (d2 - sigma * sqrt(T)) - 1)
  q <- sqrt(exp(sigma^2 * T) - 1)
  GA <- 3*q*q^3
  gAA <- 16 * q^2 + 15 * q^4 + 6 * q^6 + q^8 + 3
  Lambda1 <- skew - GA
  Lambda2 <- kurt - gAA

  Q3 <- -(stock * exp(r * T))^3 * (exp(sigma^2 * T) - 1)^(3/2) * exp(-r*T) / 6 * daX
  Q4 <- (stock * exp(r*T))^4 * (exp(sigma^2*T) - 1) ^ 2 * exp(-r*T) / 24 * daXX

  cp <- call.value(stock,strike,b,r,T,sigma)$value + Lambda1*Q3 + Lambda2*Q4
  if(match.arg(type) == "call")
    cp
  else
    cp - stock*exp((b-r)*T) + strike*exp(-r*T)
}
CorradoSuSkewKurtosis <- function(type=c("call","put"),stock=42,strike=40,r=0.1,b=0.1,sigma=0.2,T=0.5,skew,kurt) {
  d1 <- ( log(stock/strike) + (b + sigma^2/2)*T )/ (sigma*sqrt(T))
  d2 <- d1 - sigma*sqrt(T)
  Q4 <- 1/24 * stock * sigma * sqrt(T) * ((d1^2-1-3*sigma*sqrt(T)*d2) * dnorm(d1) + sigma^3 * T^1.5 * pnorm(d1))
  Q3 <- 1/6 * stock * sigma * sqrt(T) * ((2*sigma*sqrt(T)-d1) * dnorm(d1) + sigma^2 * T * pnorm(d1))
  
  cp <- call.value(stock,strike,b,r,T,sigma)$value + skew*Q3 + (kurt-3)*Q4
  if(match.arg(type) == "call")
    cp
  else
    cp - stock*exp((b-r)*T) + strike*exp(-r*T)  # put via put-call parity
}
ModifiedCorradoSuSkewKurtosis <- function(type=c("call","put"),stock=42,strike=40,r=0.1,b=0.1,sigma=0.2,T=0.5,skew,kurt) {
  w <- skew/6 * sigma^3 * T^1.5 + kurt/24 * sigma^4 * T^2
  d <- (log(stock/strike) + (b+sigma^2/2)*T - log(1+w)) / (sigma * sqrt(T))
  Q3 <- 1/(6*(1+w))*stock*sigma*sqrt(T)*(2*sigma*sqrt(T)-d)*dnorm(d)
  Q4 <- 1/(24*1+w)*stock*sigma*sqrt(T)*(d^2-3*d*sigma*sqrt(T)+3*sigma^2*T-1)*dnorm(d)

  cp <- call.value(stock,strike,b,r,T,sigma)$value + skew*Q3 + (kurt-3)*Q4
  if(match.arg(type) == "call")
    cp
  else
    cp - stock*exp((b-r)*T) + strike*exp(-r*T)  # put via put-call parity
}


CRRtree <- function(type=c('ac','ap','ec','ep'),stock=42,strike=40,r=0.1,b=0.1,sigma=0.2,T=0.5,N=52) {
  type <- match.arg(type)
  z <- ifelse(type %in% c('ac','ec'), 1, -1)
  american <- ifelse(type %in% c('ac','ap'), TRUE, FALSE)

  # Example from Haug PlainTrees.xls
  dt <- T/N
  u <- exp( sigma * sqrt(dt))
  d <- 1/u
  p = (exp(b * dt) - d) / (u - d)
  dis <- exp(-r*dt)

  # stock price
  S <- matrix(0,nc=N+1,nrow=N+1) 
  S <- lapply(0:N, function(N.) stock * u^(0:N.) * d^(N.:0))
 
  # option price
  P <- vector("list", length(S))
  P[[length(P)]] <- ifelse(z* (S[[length(S)]]-strike) > 0, z * (S[[length(S)]]-strike), 0)
  if(american) {
    for(i in N:1)
      P[[i]] <- mapply(max, dis*(p*P[[i+1]][1+(1:i)] + (1-p)*P[[i+1]][1:i]),z*(S[[i]]-strike))
  } else {
    for(i in N:1)
      P[[i]] <- dis*(p*P[[i+1]][1+(1:i)] + (1-p)*P[[i+1]][1:i])
  }
  g <- list()
  g$delta <- diff(P[[2]]) / (S[[1]]*u - S[[1]]*d)
  g$gamma <- (diff(P[[3]][2:3]) / (stock*u^2-stock) - diff(P[[3]][1:2]) / (stock-stock*d^2)) / (0.5*(stock*u^2-stock*d^2))
  g$theta <- (P[[3]][2] - P[[1]]) / (2*dt) / 365
  structure(list(Stock=S,Price=P,Greeks=g), class=c("binomialtree", "CRRtree"))
}

#JRtree <- function(stock=100,strike=100,r=0.06,sigma=0.165,T=1,N=3) {
JRtree <- function(type=c('ac','ap','ec','ep'),stock=42,strike=40,r=0.1,b=0.1,sigma=0.2,T=0.5,N=52) {
  type <- match.arg(type)
  z <- ifelse(type %in% c('ac','ec'), 1, -1)
  american <- ifelse(type %in% c('ac','ap'), TRUE, FALSE)
  dt <- T/N
  #u <- exp(sigma * sqrt(dt))
  #d <- exp(-sigma * sqrt(dt))
  u <- exp( (b-0.5*sigma^2)*dt + sigma*sqrt(dt))
  d <- exp( (b-0.5*sigma^2)*dt - sigma*sqrt(dt))

  dis <- exp(-r*dt)
  p <- 0.5 #*(1.0 + (r - 0.5*sigma^2)*sqrt(dt))

  # stock price
  S <- matrix(0,nc=N+1,nrow=N+1) 
  S <- lapply(0:N, function(N.) stock * u^(0:N.) * d^(N.:0))

  # option price
  P <- vector("list", length(S))
  P[[length(P)]] <- ifelse(z*(S[[length(S)]]-strike) > 0, z*(S[[length(S)]]-strike), 0)

  if(american) {
    for(i in N:1)
      P[[i]] <- mapply(max, dis*(p*P[[i+1]][1+(1:i)] + (1-p)*P[[i+1]][1:i]), z*(S[[i]]-strike))
  } else {
    for(i in N:1)
      P[[i]] <- dis*(p*P[[i+1]][1+(1:i)] + (1-p)*P[[i+1]][1:i])
  }
  g <- list()
  g$delta <- diff(P[[2]]) / (S[[1]]*u - S[[1]]*d)
  g$gamma <- (diff(P[[3]][2:3]) / (stock * u^2 - stock * u * d) - diff(P[[3]][1:2]) / (stock*u*d-stock*d^2)) /(0.5*(stock*u^2-stock*d^2))
  g$theta <- (P[[3]][2] - P[[1]]) / (2*dt) / 365
  structure(list(Stock=S,Price=P,Greeks=g), class=c("binomialtree", "JRtree"))
}

LRtree <- function(type=c('ac','ap','ec','ep'),stock=42,strike=40,r=0.1,b=0.1,sigma=0.2,T=0.5,N=53, method=2, force.odd=TRUE) {
  type <- match.arg(type)
  z <- ifelse(type %in% c('ac','ec'), 1, -1)
  american <- ifelse(type %in% c('ac','ap'), TRUE, FALSE)

  if(force.odd)
    N <- N + (N+1) %% 2  # N should be odd, so we round up if even

  d1 <- ( log(stock / strike) + (b + sigma^2 / 2)*T) / (sigma * sqrt(T))
  d2 <- d1 - sigma * sqrt(T)

  if(method==1) {
    # Preizer-Pratt inversion method 1
    hd1 <- 0.5 + sign(d1) * sqrt(0.25-0.25*exp(-(d1/ (N+1/3))^2 * (N+1/6)))
    hd2 <- 0.5 + sign(d2) * sqrt(0.25-0.25*exp(-(d2/ (N+1/3))^2 * (N+1/6)))
  } else {
    # Preizer-Pratt inversion method 2
    hd1 <- 0.5 + sign(d1) * sqrt(0.25-0.25*exp(-(d1/ (N+1/3+0.1/(N+1)))^2 * (N+1/6)))
    hd2 <- 0.5 + sign(d2) * sqrt(0.25-0.25*exp(-(d2/ (N+1/3+0.1/(N+1)))^2 * (N+1/6)))
  }
  dt <- T/N
  p <- hd2
  u <- exp(b*dt) * hd1/hd2
  d <- (exp(b*dt) - p*u) / (1-p)
  dis <- exp(-r*dt)

  # stock price
  S <- matrix(0,nc=N+1,nrow=N+1) 
  S <- lapply(0:N, function(N.) stock * u^(0:N.) * d^(N.:0))

  # option price
  P <- vector("list", length(S))
  P[[length(P)]] <- ifelse(z*(S[[length(S)]]-strike) > 0, z*(S[[length(S)]]-strike), 0)

  if(american) {
    for(i in N:1)
      P[[i]] <- mapply(max, dis*(p*P[[i+1]][1+(1:i)] + (1-p)*P[[i+1]][1:i]), z*(S[[i]]-strike))
  } else {  # european
    for(i in N:1)
      P[[i]] <- dis*(p*P[[i+1]][1+(1:i)] + (1-p)*P[[i+1]][1:i])
  }
  g <- list()
  g$delta <- diff(P[[2]]) / (stock*u - stock*d)
  g$gamma <- (diff(P[[3]][2:3]) / (stock * u^2 - stock * u * d) - diff(P[[3]][1:2]) / (stock*u*d-stock*d^2)) /(0.5*(stock*u^2-stock*d^2))
  g$theta <- (P[[3]][2] - P[[1]]) / (2*dt) / 365
  structure(list(Stock=S,Price=P,Greeks=g), class=c("binomialtree", "LRtree"))
}
BinomialTree <- function(stock=100,strike=100,r=0.06,sigma=0.165,N=3) {
  # I think this one is wrong
  dt <- T/N
  u <- 1.1
  d <- 1/u
  dis <- exp(-r*dt)
  p <- ( exp(r*dt) - d )/(u-d)

  S <- matrix(0,nc=N+1,nrow=N+1) 
  # stock price
  S <- lapply(0:N, function(N.) stock * u^(0:N.) * d^(N.:0))
  S
  P <- vector("list", length(S))
  P[[length(P)]] <- ifelse(strike-S[[length(S)]] > 0, strike-S[[length(S)]], 0)
  for(i in N:1)
    P[[i]] <- mapply(max, dis*(p*P[[i+1]][1+(1:i)] + (1-p)*P[[i+1]][1:i]), 100-S[[i]])
  list(Stock=S,Put=P)
}
