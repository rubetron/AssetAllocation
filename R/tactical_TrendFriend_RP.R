#' Returns allocations for the Ivy Portfolio on a given date
#'
#' \code{tactical_TrendFriend_RP} determines asset allocations for a strategy
#' according to a modified version of the the strategy in Clare et al (2016,
#' <https://doi.org/10.1016/j.jbef.2016.01.002)>. The modified version uses full
#' risk parity instead of the inverse-volatility rule in the paper.
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
#' ivy  <- asset_allocations$tactical$ivy
#' reb_date <- as.Date("2022-03-31")
#' tactical_TrendFriend_RP(ivy, reb_date, ETFs$Prices[, ivy$tickers], ETFs$Returns[, ivy$tickers])
#' @return A numeric vector of weights after applying the rule.
#' @export
#' @import xts
#' @importFrom xts endpoints
#' @importFrom zoo rollmean
# Ivy portfolio allocation
tactical_TrendFriend_RP <- function(strat, reb_date, P, R, risk_free = NULL){

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
      warning("n_months_trend not found in strat$params. Defaulting to 10")
      n_months_trend <- 10 # default look-back for Ivy
    }
  } else {
    n_months_trend <- 10 # default look-back for Ivy
  }

  # check that user supplied a specific value for number of months to estimate
  # covariance matrix. if not, use the default 2 years
  if (length(strat$params) > 0){
    # check that there is element n_days_cov in params
    if ("n_days_cov" %in% names(strat$params)){
      n_days_cov <- strat$params$n_days_cov
    } else{
      warning("n_days_cov not found in strat$params. Defaulting to 252*2")
      n_days_cov <- 252*2 # default cov estimations window in days
    }
  } else {
    n_days_cov <- 252*2 # default cov estimations window in days
  }

  # step 1: calculation of weights using full risk parity
  if (nrow(R) >= n_days_cov){
    R <- R[seq(from = max(1, nrow(R) - n_days_cov + 1),
               to = nrow(R)), ]
    n_assets <- length(strat$tickers)

    # get valid data
    R <- R[seq(from = which.max(!is.na(rowSums(R))),
               to = nrow(R)), ]

    # check if there is at least one year of daily data
    if (nrow(R) > 252){
      # estimate covariance matrix with available data
      cov_mat <- covEstimation(R,
                               control = list(type = 'ewma',
                                              lambda = 0.98851))
      # risk parity optimization
      w <- riskParityPortfolio(cov_mat,
                               b = rep(1/n_assets, n_assets))$w
    } else {
      w <- rep(0, n_assets)
    }

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
