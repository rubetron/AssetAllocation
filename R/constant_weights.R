#' Returns constant weights for static asset allocations
#'
#' \code{constant_weights} applies the identity function to the default weights
#' in a strategy.
#'
#'
#' @param strat A list representing an asset allocation strategy.
#' @param reb_date A date on which the allocation rule is applied.
#' @param P An xts object with daily prices of the tickers in strat.
#' @param R An xts object with daily returns of the tickers in strat.
#' @param risk_free Either an xts object with daily returns of the risk-free
#' asset, or a scalar numeric with the annual risk-free rate in decimals.
#'
#' @examples
#' us_60_40  <- asset_allocations$static$us_60_40
#' reb_date <- as.Date("2022-03-31")
#' constant_weights(us_60_40,
#'                  reb_date,
#'                  ETFs$Prices[, us_60_40$tickers],
#'                  ETFs$Returns[, us_60_40$tickers],
#'                  ETFs$risk_free)
#' @return A numeric vector of weights after applying the rule.
#' @export
# function for constant weights
constant_weights <- function(strat, reb_date = NULL, P, R, risk_free){
  identity(strat$default_weights)}
