#' @export
#' @importFrom quantmod getSymbols
#' @importFrom PerformanceAnalytics CalculateReturns
#' @importFrom zoo index
#' @import xts
get_return_data_from_tickers <- function(tickers, starting_date = "2007-01-01"){
  getSymbols(tickers, from = starting_date, source = 'yahoo')

  # align all prices into one xts object
  prices <- xts()
  for (i in 1:length(tickers)){
    prices  <- merge.xts(prices, get(tickers[i])[,6])
  }
  colnames(prices) <- tickers

  #calculate returns
  returns <- CalculateReturns(prices)

  # format as date
  index(returns) <- as.Date(index(returns))

  return(returns)
}
