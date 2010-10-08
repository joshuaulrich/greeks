contract <- function(underlying., expiry., strike., right.) {
  # contract("AAPL", "201010", 250, "C")
  structure(list(underlying=underlying., expiry=expiry., strike=strike., right=right.),
            class="contract")
}

as.contract <- function(x, ...) {
  UseMethod("as.contract")
}

as.contract.contract <- function(x, ...) {
  x
}

as.contract.osi <- function(x, ...) {
  contract(underlying(x), expiry(x), strike(x), right(x))
}

as.contract.character <- function(x, ...) {
  x <- try.osi(x)
  if(is.osi(x))
    as.contract(x)
  else stop("improperly formatted 'osi' string")
}

as.combo <- function(...) {
  contracts <- lapply(list(...), as.contract)
  structure(contracts, class="combo")
}

is.combo <- function(x) {
  inherits(x, "combo")
}
