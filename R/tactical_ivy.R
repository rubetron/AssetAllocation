#' Returns allocations for the Ivy Portfolio on a given date
#'
#' \code{tactical_ivy} determines asset allocations for a strategy according to
#' the Ivy Portfolio rule of Faber (2013,  ISBN:978-1118008850).
#'
#' The function compares prices at the end of a month to their moving averages.
#' If the price of an asset is below its moving average, the corresponding
#' allocation in \code{strat$default_weights} is set to zero.
#' @param strat A list representing an asset allocation strategy.
#' @param reb_date A date on which the allocation rule is applied.
#' @param P An xts object with daily prices of the tickers in strat.
#' @param R An xts object with daily returns of the tickers in strat.
#' @examples
#' ivy  <- list(name = "Ivy",
#'              tickers = c("VTI", "VEU", "VNQ", "AGG", "DBC"),
#'              default_weights = c(0.20, 0.20, 0.20, 0.20, 0.20),
#'              rebalance_frequency = "month",
#'              portfolio_rule_fn = tactical_ivy)
#' tactical_ivy()
#' @return A numeric vector of weights after applying the rule.
#' @export
#' @import xts
#' @importFrom xts endpoints
#' @importFrom zoo rollmean
# Ivy portfolio allocation
tactical_ivy <- function(strat, reb_date, P, R){

  # comparison will be made using dates until the reb_date
  # Ivy is supposed to be rebalanced monthly
  # If user supplies different rebalancing frequency,
  # will use last month end.
  ava_dates <- paste0("/", reb_date)

  P <- P[ava_dates]
  R <- R[ava_dates]

  # first need to calculate moving average of prices at the end of each month
  P_month_ends <- P[endpoints(P, on = "months"), ]

  # check that user supplied a specific window.
  # if not, use the default 10 months
  if (length(strat$params) > 0){
    # check that there is element n_months in params
    if ("n_months" %in% names(strat$params)){
      n_months <- strat$params$n_months
    } else{
      warning("n_months not found in strat$params. Defaulting to 10")
      n_months <- 10 # default look-back for Ivy
    }
  } else {
    n_months <- 10 # default look-back for Ivy
  }

  w <- strat$default_weights
  if (nrow(P_month_ends) >= n_months){
    P_moving <- rollmean(P_month_ends,
                         k = n_months,
                         fill = NA,
                         align= "right")
    if (!any(is.na(P_moving[nrow(P_moving), ]))){
      w[which(P_month_ends[nrow(P_month_ends), ] < P_moving[nrow(P_moving), ])] <- 0
    }
  }

  return(w)
}
