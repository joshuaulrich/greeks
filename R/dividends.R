# Code adapted from The Complete Guide To Option Pricing Formulas 2nd Edition
# Espen Haug.  Chapter 9. Options On Stocks
#
# Additional code used is for validation/inspiration:
#  http://www.mathfinance.cn/valuation-of-stock-option-with-discrete-dividend/

EscrowedDividend <- function(type=c('call','put'), S, X, b, r, Time, v, d, dT) {
  # Merton 73
  adj.s <- S-exp(-r*dT)*d
  if(missing(type))
  BlackScholesMerton(,S=adj.s, X=X, r=r, b=b, Time=Time, v=v)
  else
  BlackScholesMerton(type,S=adj.s, X=X, r=r, b=b, Time=Time, v=v)
}
HaugHaugDividend <- function(type=c('call','put'), S, X, b, r, Time, v, d, dT) {
  warning('BROKEN: do not use!!!')
  # Haug, Haug 1998
  adj.s <- S-exp(-r*dT)*d
  adj.vol <- v*S/adj.s
  adj.vol <- sqrt(((v^2)*dT + (adj.vol^2)*(Time-dT)) / Time)
  if(missing(type))
  BlackScholesMerton(,S=adj.s, X=X, r=r, b=b, Time=Time, v=adj.vol)
  else
  BlackScholesMerton(type,S=adj.s, X=X, r=r, b=b, Time=Time, v=adj.vol)
}
ChrissDividend <- function(type=c('call','put'), S, X, b, r, Time, v, d, dT) {
  # Chriss 1997
  adj.s <- S-exp(-r*dT)*d
  adj.vol <- v*S/adj.s
  if(missing(type))
  BlackScholesMerton(,S=adj.s, X=X, r=r, b=b, Time=Time, v=adj.vol)
  else
  BlackScholesMerton(type=type,S=adj.s, X=X, r=r, b=b, Time=Time, v=adj.vol)
}
BosGairatShepelevaDividend <- function(type=c('call','put'), S, X, b, r, Time, v, d, dT) {
  lns <- log(S)
  lnx <- log( (X+d*exp(-r*dT)) * exp(-r*Time))
  z1 <- (lns-lnx) / (v * sqrt(Time))+v*sqrt(Time)/2
  z2 <- z1 + v * sqrt(Time)/2
  adj.s <- S - exp(-r*dT)*d
  adj.vol <- sqrt(v^2 + v*sqrt(pi/(2*Time))*
             (4*exp(z1^2/2-lns)*d*exp(-r*dT)*
             (pnorm(z1) - pnorm(z1-v*dT/sqrt(Time))) +
             exp(z2^2/2-2*lns)*d^2*
             exp(-r*2*dT)*(pnorm(z2)-pnorm(z2-2*v*dT/sqrt(Time)))))
  BlackScholesMerton(type,adj.s, X, r, b=b, Time=Time, v=adj.vol)
}
HaugHaugLewisDividend <- function(type=c('ac','ap','ec','ep'),S,X,b,r,Time,v,d,dT) {
  lns <- log(S)
  cp <- ifelse(grepl('c',type), 'c','p')
  american <- ifelse(grepl('a',type), TRUE, FALSE)
  if( !american) {
    exp(-r*dT) * (integrate(function(x) BlackScholesMerton('c',x-d, X,b=b, r, Time-dT,v)$call$value * dlnorm(x,lns+(r-0.5*v^2)*dT,v*sqrt(dT)),d,X+d)$value +
    integrate(function(x) BlackScholesMerton(cp,x-d, X,b=b, r, Time-dT,sigma)$call$value * dlnorm(x,lns+(r-0.5*v^2)*dT,sigma*sqrt(dT)),X+d,20*S)$value)
  } else {
    i1 <- function(x) {
            max(x-X, BlackScholesMerton(cp,x-d, X,b=b, r, Time-dT,v)$call$value) * 
                     dlnorm(x,lns+(r-0.5*v^2)*dT,v*sqrt(dT))
          }
    i2 <- function(x) {
            max(x-X, BlackScholesMerton(cp,x-d, X,b=b, r, Time-dT,v)$call$value) * 
                     dlnorm(x,lns+(r-0.5*v^2)*dT,v*sqrt(dT))
          }
    exp(-r*dT) * (integrate(Vectorize(i1), d, X+d)$value+integrate(Vectorize(i2), X+d, 20*S)$value)
  }
}

DiscreteDividend <- function(type=c('ec','ep','ac','ap'), S, X, b, r, Time, v, d, dT,
                             method=c('BosVandermark','HaugHaugLewis','HaugHaug','Chriss','BosGairatShepeleva','Escrowed'))
{
  method <- match.arg(method)
  do.call(method, list(type=type,S=S,X=X,b=b,r=r,Time=Time,v=v,d=d,dT=dT))
}

BosVandermarkCashDividend <- function(type=c("call","put"),S,X,T,r,b,v,d,dT) {
  # only for european options!
  n <- length(d)
  if(n != length(dT))
    stop(paste(sQuote('d'),"dividends and",sQuote('dT'),"dividend times must",
               "be of equal length")) 

  Xn <- sum( (T-dT) / T * d * exp(-r*dT) )
  Xf <- sum( dT / T * d * exp(-r*dT) )

  if(match.arg(type)=="call")
    call.value(S-Xn, X+Xf * exp(r*T), r, r, T, v)$value
  else
    put.value(S-Xn, X+Xf * exp(r*T), r, r, T, v)$value
}


BinomialDiscreteDiv <- function(type=c("ac","ap","ec","ep"),S,X,T,r,v,n,d,dT) {
  z <- ifelse(grepl('c',match.arg(type)), 1, -1)
  dt <- T/n
  Df <- exp(-r*dt)
  u <- exp(v*sqrt(dt))
  d <- 1/u
  uu <- u^2
  p <- (exp(r*dt)-d) / (u-d)
  div.amt <- d[1]
  

}

DiscreteDivYield <- function(type=c("ac","ap","ec","ep"),S,X,T,r,v,n,d,dT) {
#  > div  <- c(8, 5, 5, 5, 5) / 100
#  > divT <- c(1, 2, 3, 4, 6) / 10
#  > ss <- DiscreteDivYield("ec",100,101,0.5,.1,.3,100,div,divT)
#  > ss$OV[[1]]
#  [1] 1.051132
#  > ss$St[[1]]
#  [1] 74.93458


  if(length(d) < 1 || is.null(d)) 
    return(CRRtree(type,S,X,T,r,r,v,n))


  dt <- T/n
  Df <- exp(-r*dt)
  U <- exp(v * sqrt(dt))
  D <- 1/U
  UU <- U^2
  p <- (exp(r*dt) - D) / (U-D)

  type <- match.arg(type)
  z <- ifelse(grepl("c",type), 1, -1)
  american <- ifelse(grepl("a",type), TRUE, FALSE)

  i <- 0:(length(d)-1)

  # calculate discrete steps at which dividends must be accounted for
  steps <- as.integer(dT[i+1]/T*n)

  # cumulative dividend adjustments
  sumdiv <- prod(c(1, (1-d[i+1])) )

  St <- vector("list", n+1)
  Ov <- vector("list", n+1)

  # nodes at expiration
  i <- 0:n
  St[[n+1]] <- S*U^i*D^(n-i)*sumdiv
#print(St[[n+1]])
#  St[[n+1]] <- (S*U^i*D^(n-i))-5
#print(St[[n+1]])
  Ov[[n+1]] <- mapply(max, z*(St[[n+1]]-X),0)

  #for(m in length(steps):1) {    ### WORKING on n=6...
  for(m in (n-1):1) {
    if(m %in% steps) {
#      cat('div:',m,'\n')
      div <- d[match(m, steps)]
      St[[m+1]] <- (St[[m+2]]/(1-div)*D)[-1]
#      St[[m+1]] <- ((St[[m+2]]-5)*D)[-1]
      Ov[[m+1]] <- ( p*Ov[[m+2]][-1] + (1-p)*Ov[[m+2]][-length(Ov[[m+2]])]) * Df
    } else {
#      cat('no div:',m,'\n')
      St[[m+1]] <- St[[m+2]][-1]*D
      Ov[[m+1]] <- ( p*Ov[[m+2]][-1] + (1-p)*Ov[[m+2]][-length(Ov[[m+2]])]) * Df
    }
    if(american) {
#print(Ov[[m+1]])
#print(z*(St[[m+1]]-X))
      Ov[[m+1]] <- mapply(max, Ov[[m+1]], z*(St[[m+1]] - X))
    }
  }
  St[[1]] <- St[[2]][-1]*D
  Ov[[1]] <- ( p*Ov[[2]][-1] + (1-p)*Ov[[2]][-length(Ov[[2]])]) * Df
    if(american) {
      Ov[[1]] <- mapply(max, Ov[[1]], z*(St[[1]] - X))
    }
  # TODO greeks
  list(p=p,Df=Df,D=D,steps=steps,St=St,OV=Ov)
}
