#'Running Cumulative Returns
#'@description Computes the running cumulative return of a time series
#'@param R a set of returns
#'@param n a lookback period
#'@return the cumulative return over an n-period lookback, with a base of zero
#'@export
"runCumRets" <- function(R, n = 252) {
  cumRets <- cumprod(1+R)
  rollingCumRets <- cumRets/lag(cumRets, k = n) - 1
  return(rollingCumRets)
}

