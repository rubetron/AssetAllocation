#' @export
#' @importFrom quantmod getSymbols
#' @importFrom PerformanceAnalytics CalculateReturns
#' @importFrom zoo index na.locf
#' @import xts
get_return_data_from_tickers <- function(tickers, starting_date = "2007-01-01"){
  getSymbols(tickers, from = starting_date, source = 'yahoo')

  # align all prices into one xts object
  prices <- xts()
  for (i in seq(from = 1, to = length(tickers))){
    prices  <- merge.xts(prices, get(tickers[i])[,6])
  }
  colnames(prices) <- tickers

  # consider only weekdays: makes it possible
  # to mix exchange-traded and crypto
  w_days <- weekdays(index(prices))
  w_ends <- which(w_days == "Saturday" | (w_days == "Sunday"))
  if (length(w_ends) > 0){
    prices <- prices[-w_ends, ]
  }

  # take care of missing prices to avoid NA returns
  prices <- na.locf(prices)

  #calculate returns
  returns <- CalculateReturns(prices)

  # format as date
  zoo::index(returns) <- as.Date(zoo::index(returns))

  return(returns)
}
