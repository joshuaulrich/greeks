call.value <-
function(S, X, b, r, Time, v) {
  S <- as.numeric(S)
  X <- as.numeric(X)
  b <- as.numeric(b)
  r <- as.numeric(r)
  Time <- as.numeric(Time)
  v <- as.numeric(v)

  d1 <- (log(S/X) + (b + (v^2)/2) * Time)/(v * sqrt(Time))
  d2 <- d1 - v * (sqrt(Time))
  cp <- (S * exp( (b-r) * Time) * pnorm(d1)) - (X * exp(-r * Time) *
        pnorm(d2))
  delta.c <- exp(-b * Time) * pnorm(d1) ###### FIXME/CHECKME
  if(identical(cp, NaN)) {
    cp <- delta.c <- 0  
  }
  list(value=cp, delta=delta.c)
}

