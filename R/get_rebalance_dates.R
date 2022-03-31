# helper function to determine rebalancing dates
#' @export
#' @import xts
get_rebalance_dates <- function(dates, reb_freq, k = 1){
  # for now, we rebalance at the end of period in reb_freq
  # reb_freq must be one of the following:
  # "days", "weeks", "months", "quarters", and "years"
  return(dates[endpoints(dates, on = reb_freq, k)])
}
