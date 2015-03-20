#'Running Sharpe Ratio
#'@param R a return series
#'@param n a lookback period
#'@param scale number of periods in a year 
#'(daily scale = 252, monthly scale = 12, quarterly scale = 4)
#'@param volFactor a volatility factor -- can be raised to compute a value
#'biased further to volatility (volFactor > 1)
#'or away from volatility (volFactor < 1)
#'(default 1)
#'@return an n-day rolling Sharpe ratio
#'@export
"runSharpe" <- function(R, n = 252, scale = NA, volFactor = 1) {
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
  rollingAnnRets <- runCumRets(R, n = n, annualized = TRUE)
  rollingAnnSD <- sapply(R, runSD, n = n)*sqrt(scale)
  rollingSharpe <- rollingAnnRets/rollingAnnSD ^ volFactor
  return(rollingSharpe)
}