CRRtree <- function(stock=100,strike=100,r=0.06,sigma=0.165,T=1,N=3) {
  dt <- T/N
  u <- exp( (r-0.5*sigma^2)*dt + sigma*sqrt(dt))
  d <- exp( (r-0.5*sigma^2)*dt - sigma*sqrt(dt))

  dis <- exp(-r*dt)
  p <- 0.5

  S <- matrix(0,nc=N+1,nrow=N+1) 
  # stock price
  S <- lapply(0:N, function(N.) stock * u^(0:N.) * d^(N.:0))
  S
  P <- vector("list", length(S))
  P[[length(P)]] <- ifelse(strike-S[[length(S)]] > 0, strike-S[[length(S)]], 0)
  for(i in N:1)
    P[[i]] <- mapply(max, dis*(p*P[[i+1]][1+(1:i)] + (1-p)*P[[i+1]][1:i]), 100-S[[i]])
  structure(list(Stock=S,Put=P), class=c("binomialtree", "CRRtree"))
}

JRtree <- function(stock=100,strike=100,r=0.06,sigma=0.165,T=1,N=3) {
  dt <- T/N
  u <- exp(sigma * sqrt(dt))
  d <- exp(-sigma * sqrt(dt))

  dis <- exp(-r*dt)
  p <- 0.5*(1.0 + (r - 0.5*sigma^2)*sqrt(dt))

  S <- matrix(0,nc=N+1,nrow=N+1) 
  # stock price
  S <- lapply(0:N, function(N.) stock * u^(0:N.) * d^(N.:0))
  S
  P <- vector("list", length(S))
  P[[length(P)]] <- ifelse(strike-S[[length(S)]] > 0, strike-S[[length(S)]], 0)
  for(i in N:1)
    P[[i]] <- mapply(max, dis*(p*P[[i+1]][1+(1:i)] + (1-p)*P[[i+1]][1:i]), 100-S[[i]])
  structure(list(Stock=S,Put=P), class=c("binomialtree", "JRtree"))
}
BinomialTree <- function(stock=100,strike=100,r=0.06,sigma=0.165,N=3) {
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
