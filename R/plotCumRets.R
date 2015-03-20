#'Plot N-day Cumulative Returns
#'@description plots a running n-day cumulative return
#'@param R a return stream
#'@param n a lookback amount in days (default 252)
#'@param annualized whether or not to annualize returns
#'@param ... additional arguments
#'@return a rolling n-day cumulative return plot in percent
#'@export
"plotCumRets" <- function(R, n = 252, annualized = FALSE, ...) {
  cumRets <- runCumRets(R = R, n = n, annualized = annualized)
  cumRets <- cumRets[!is.na(cumRets[,1]),]
  if(annualized) {
    title <- "period rolling annualized return"
  } else {
    title <- "period rolling cumulative return"
  }
  chart.TimeSeries(cumRets, legend.loc="topleft", main=paste(n, title),
                   date.format="%Y", yaxis=FALSE, ylab="Return", auto.grid=FALSE, ...)
  
  meltedCumRets <- do.call(c, data.frame(cumRets))
  axisLabels <- pretty(meltedCumRets, n = 10)
  axisLabels <- unique(round(axisLabels, 1))
  axisLabels <- axisLabels[axisLabels > min(axisLabels) & axisLabels < max(axisLabels)]
  axis(side=2, at=axisLabels, label=paste(axisLabels*100, "%"), las=1)
}
