# dividend payouts 

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
  z <- ifelse(grep('c',match.arg(type)), 1, -1)
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

  ##### TODO add in american style options 

  if(length(d) < 1) 
    return(CRRtree(type,S,X,T,r,r,v,n))


  dt <- T/n
  Df <- exp(-r*dt)
  U <- exp(v * sqrt(dt))
  D <- 1/U
  UU <- U^2
  p <- (exp(r*dt) - D) / (U-D)

  z <- ifelse(grep('c',match.arg(type)), 1, -1)

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
  Ov[[n+1]] <- mapply(max, z*(St[[n+1]]-X),0)

  #for(m in length(steps):1) {    ### WORKING on n=6...
  for(m in (n-1):1) {
    if(m %in% steps) {
#      cat('div:',m,'\n')
      div <- d[match(m, steps)]
      St[[m+1]] <- (St[[m+2]]/(1-div)*D)[-1]
      Ov[[m+1]] <- z*( p*Ov[[m+2]][-1] + (1-p)*Ov[[m+2]][-length(Ov[[m+2]])]) * Df
    } else {
#      cat('no div:',m,'\n')
      St[[m+1]] <- St[[m+2]][-1]*D
      Ov[[m+1]] <- z*( p*Ov[[m+2]][-1] + (1-p)*Ov[[m+2]][-length(Ov[[m+2]])]) * Df
    }
  }
  St[[1]] <- St[[2]][-1]*D
  Ov[[1]] <- z*( p*Ov[[2]][-1] + (1-p)*Ov[[2]][-length(Ov[[2]])]) * Df
  list(p=p,Df=Df,D=D,steps=steps,St=St,OV=Ov)

}
