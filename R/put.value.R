put.value <-
function(S, X, b, r, Time, v) {
  S <- as.numeric(S)
  X <- as.numeric(X)
  b <- as.numeric(b)
  r <- as.numeric(r)
  Time <- as.numeric(Time)
  v <- as.numeric(v)

  d1 <- (log(S/X) + (r - b + (v^2)/2) * Time)/(v * sqrt(Time))
  d2 <- d1 - v * (sqrt(Time))
  pp <- exp(-r * Time) * X * pnorm(-d2) - exp(-b * Time) * 
        S * pnorm(-d1)
  delta.p <- -exp(-b * Time) * pnorm(-d1)
  if(identical(pp, NaN)) {
    pp <- delta.p <- 0  
  }
  list(value=pp, delta=delta.p)
}

