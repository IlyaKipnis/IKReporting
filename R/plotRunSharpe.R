#'Plot N-day Rolling Sharpe Ratio
#'@description plots an n-day running sharpe ratio
#'@param R return stream
#'@param n a lookback period
#'@param ... additional plotting arguments
#'@return a plot of the Sharpe ratio over time
#'@export
"plotRunSharpe" <- function(R, n = 252, ...) {
  sharpes <- runSharpe(R = R, n = n)
  sharpes <- sharpes[!is.na(sharpes[,1]),]
  chart.TimeSeries(sharpes, legend.loc="topleft", main=paste("Rolling", n, "period Sharpe Ratio"),
                   date.format="%Y", yaxis=FALSE, ylab="Sharpe Ratio", auto.grid=FALSE, ...)
  meltedSharpes <- do.call(c, data.frame(sharpes))
  axisLabels <- pretty(meltedSharpes, n = 10)
  axisLabels <- unique(round(axisLabels, 1))
  axisLabels <- axisLabels[axisLabels > min(axisLabels) & axisLabels < max(axisLabels)]
  axis(side=2, at=axisLabels, label=axisLabels, las=1)
}
