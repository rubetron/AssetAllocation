#' Portfolio rebalancing dates
#'
#' \code{get_rebalance_dates} determines rebalancing dates based on rebalancing
#' frequency chosen by the user. This is a helper function used by
#' \code{backtest_allocation} and  is not intended to be called directly by the user.
#'
#' @param dates A vector of dates
#' @param reb_freq Character with rebalancing frequency. Options are
#' \code{"days"}, \code{"weeks"}, \code{"months"}, \code{"quarters"},
#' and \code{"years"}
#' @param k An integer with number of periods to skip.
#' @return A vector of dates.
#' @export
#' @importFrom xts endpoints
get_rebalance_dates <- function(dates, reb_freq, k = 1){
  # for now, we rebalance at the end of period in reb_freq
  # reb_freq must be one of the following:
  # "days", "weeks", "months", "quarters", and "years"
  reb_dates <- dates[endpoints(dates, on = reb_freq, k)]
  return(reb_dates[seq(from = 1, to = length(reb_dates)-1)])
}
