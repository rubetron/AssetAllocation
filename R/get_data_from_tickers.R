#' Downloads prices in xts format from a list of tickers from Yahoo Finance (<https://finance.yahoo.com/>).
#'
#' \code{get_data_from_tickers} retrieves adjusted closing prices from Yahoo Finance
#' for a set of tickers and returns the prices and returns.
#'
#' The function retrieves data from Yahoo Finance (<https://finance.yahoo.com/>)
#' using the getSymbols function from the \code{quantmod} package. It calculates
#' returns from adjusted prices. The ticker names must correspond to those found
#' in Yahoo Finance.
#'
#' @param tickers A vector containing a tickers.
#' @param starting_date A date on which the allocation rule is applied.
#' @examples
#' ## download data for the following exchange-traded-funds: MTUM, VLUE, USMV, and QUAL.
#' factor_ETFs <- get_data_from_tickers(c("MTUM", "VLUE", "USMV", "QUAL"),
#'                                      starting_date = "2020-01-01")
#' @return An object of class \code{"List"} containing two objects of class
#' \code{"xts"} with respectively the prices and returns of the assets,
#' with column names corresponding to the tickers.
#' @export
#' @importFrom quantmod getSymbols
#' @importFrom PerformanceAnalytics CalculateReturns
#' @importFrom zoo index na.locf
#' @importFrom curl has_internet
#' @import xts
get_data_from_tickers <- function(tickers, starting_date = "2007-01-01"){

  # attempt to retrieve data from Yahoo Finance. Fail "gracefully"
  # in case there's an issue

  if (!has_internet()) {
    message("Problem connecting to Yahoo Finance. Check internet connection or try again later.")
    return(invisible(NULL))
  } else{
    suppressWarnings({
      msg <- "Problem retrieving data from Yahoo. Check server status or that tickers are valid."
      x <- try(silent = TRUE, getSymbols(tickers, from = starting_date, source = 'yahoo'))
      if (inherits(x, "try-error")) {
        message( msg )
        return(FALSE)
      }
    })
  }

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
  zoo::index(prices) <- as.Date(zoo::index(prices))

  return(list(P = prices, R = returns))
}
