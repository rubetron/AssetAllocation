#' Returns allocations for the Ivy Portfolio on a given date
#'
#' \code{tactical_TrendFriend} determines asset allocations for a strategy
#' according to the strategy in Clare et al (2016,
#' <https://doi.org/10.1016/j.jbef.2016.01.002)>.
#'
#' The allocation strategy proposed in the paper is based on using a a time
#' series momentum rule to select assets from a universe, and an allocation
#' rule which gives weights proportional to the inverse volatility of the assets.
#' The time-series (trend) momentum rule is based on whether the price of the
#' asset on the rebalancing date is above its 10-month moving average. If not,
#' the corresponding allocation in \code{strat$default_weights} is set to zero (
#' and is therefore allocated to the risk-free asset).
#'
#' @param strat A list representing an asset allocation strategy.
#' @param reb_date A date on which the allocation rule is applied.
#' @param P An xts object with daily prices of the tickers in strat.
#' @param R An xts object with daily returns of the tickers in strat.
#' @param risk_free Either an xts object with daily returns of the risk-free
#' @examples
#' trend_friend  <- asset_allocations$tactical$trend_friend
#' reb_date <- as.Date("2022-03-31")
#' tactical_TrendFriend(trend_friend,
#'                      reb_date,
#'                      ETFs$Prices[, trend_friend$tickers],
#'                      ETFs$Returns[, trend_friend$tickers]
#'                     )
#' @return A numeric vector of weights after applying the rule.
#' @export
#' @import xts
#' @importFrom xts endpoints
#' @importFrom zoo rollmean
# Ivy portfolio allocation
tactical_TrendFriend <- function(strat, reb_date, P, R, risk_free = NULL){

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
    # check that there is element n_months_trend in params
    if ("n_months_trend" %in% names(strat$params)){
      n_months_trend <- strat$params$n_months_trend
    } else{
      warning("n_months_trend not found in strat$params. Defaulting to 12")
      n_months_trend <- 12 # default look-back for Ivy
    }
  } else {
    n_months_trend <- 12 # default look-back for Ivy
  }

  # check that user supplied a specific window for vol calculation
  # if not, use the default 12 months of daily data
  if (length(strat$params) > 0){
    # check that there is element n_months_trend in params
    if ("n_months_vol" %in% names(strat$params)){
      n_months_vol <- strat$params$n_months_vol
    } else{
      warning("n_months_vol not found in strat$params. Defaulting to 12")
      n_months_vol <- 12 # default look-back for Ivy
    }
  } else {
    n_months_vol <- 12 # default look-back for Ivy
  }

  # step 1: calculation of weights using inverse-volatility weights
  if (nrow(P_month_ends) >= max(n_months_trend, n_months_vol)){
    w <- 1/apply(R[seq(from = nrow(R) - 21*n_months_vol +1,
                       to = nrow(R)), ], 2, stats::sd)
    w <- w/sum(w)

    # step 2: trend following rule
    P_moving <- rollmean(P_month_ends,
                         k = n_months_trend,
                         fill = NA,
                         align= "right")
    if (!any(is.na(P_moving[nrow(P_moving), ])) &
        !any(is.na(w))){
      assets_tf <- which(P_month_ends[nrow(P_month_ends), ]
                         > P_moving[nrow(P_moving), ])
      w[-assets_tf] <- 0
    }
  }
  return(w)
}
