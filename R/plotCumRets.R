#'Plot N-day Cumulative Returns
#'@description plots a running n-day cumulative return
#'@param R a return stream
#'@param n a lookback amount in days (default 252)
#'@param ... additional plotting arguments
#'@return a rolling n-day cumulative return plot in percent
#'@export
"plotCumRets" <- function(R, n = 252, ...) {
  cumRets <- runCumRets(R = R, n = n)
  cumRets <- cumRets[!is.na(cumRets[,1]),]
  chart.TimeSeries(cumRets, legend.loc="topleft", main=paste(n, "day rolling cumulative return"),
                   date.format="%Y", yaxis=FALSE, ylab="Return", auto.grid=FALSE)
  
  meltedCumRets <- do.call(c, data.frame(cumRets))
  axisLabels <- pretty(meltedCumRets, n = 10)
  axisLabels <- round(axisLabels, 1)
  axisLabels <- axisLabels[axisLabels > min(axisLabels) & axisLabels < max(axisLabels)]
  axis(side=2, at=axisLabels, label=paste(axisLabels*100, "%"), las=1)
}
