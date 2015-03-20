#'Running Cumulative Returns
#'@description Computes the running cumulative return of a time series
#'@param R a set of returns
#'@param n a lookback period
#'@param scale number of periods in a year 
#'(daily scale = 252, monthly scale = 12, quarterly scale = 4)
#'@param annualized whether or not to take the cumulative or the annualized returns
#'@return the cumulative return over an n-period lookback, with a base of zero
#'@export
"runCumRets" <- function(R, n = 252, annualized = FALSE, scale = NA) {
  R <- na.omit(R)
  if (is.na(scale)) {
    freq = periodicity(R)
    switch(freq$scale, minute = {
      stop("Data periodicity too high")
    }, hourly = {
      stop("Data periodicity too high")
    }, daily = {
      scale = 252
    }, weekly = {
      scale = 52
    }, monthly = {
      scale = 12
    }, quarterly = {
      scale = 4
    }, yearly = {
      scale = 1
    })
  }
  cumRets <- cumprod(1+R)
  if(annualized) {
    rollingCumRets <- (cumRets/lag(cumRets, k = n))^(scale/n) - 1 
  } else {
    rollingCumRets <- cumRets/lag(cumRets, k = n) - 1
  }
  return(rollingCumRets)
}

