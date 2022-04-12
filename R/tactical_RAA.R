#' Returns allocations for the Robust Asset Allocation on a given date
#'
#' \code{tactical_RAA} determines asset allocations for a strategy according to
#' the Robust Asset Allocation (RAA) approach of Gray and Vogel (2015,
#' ISBN:978-1119071501).
#'
#' RAA uses two trend-following rules. The first one is based on comparing the
#' current price of assets with their 12-month moving average. The second one
#' compares returns with the returns of the risk-free asset. The allocation rule
#' keeps either 100%, 50%, or 0% of the default weight for each risky asset
#' if both rules provide a positive signal, only one rule provided a positive
#' signal, or both rules provide a negative signal, respectively. Any amounts
#' not allocated to risky assets are allocated to the risk-free asset as
#' implemented in the \code{backtest_allocation} function.
#'
#' @param strat A list representing an asset allocation strategy.
#' @param reb_date A date on which the allocation rule is applied.
#' @param P An xts object with daily prices of the tickers in strat.
#' @param R An xts object with daily returns of the tickers in strat.
#' @param risk_free Either an xts object with daily returns of the risk-free
#' asset, or a scalar numeric with the annual risk-free rate in decimals.
#'
#' @examples
#' raa  <- asset_allocations$tactical$raa
#' reb_date <- as.Date("2022-03-31")
#' tactical_RAA(raa,
#'              reb_date,
#'              ETFs$Prices[, raa$tickers],
#'              ETFs$Returns[, raa$tickers],
#'              ETFs$risk_free)
#' @return A numeric vector of weights after applying the rule.
#' @export
#' @import xts
#' @importFrom xts endpoints
#' @importFrom zoo rollmean
# raa portfolio allocation
tactical_RAA <- function(strat, reb_date, P, R, risk_free){

  # check that user supplied a specific window.
  # if not, use the default 12 months
  if (length(strat$params) > 0){
    # check that there is element n_months in params
    if ("n_months" %in% names(strat$params)){
      n_months <- strat$params$n_months
    } else{
      warning("n_months not found in strat$params. Defaulting to 12")
      n_months <- 12 # default look-back
    }
  } else {
    n_months <- 12 # default look-back
  }

  # comparison will be made using dates until the reb_date
  ava_dates <- paste0("/", reb_date)
  P <- P[ava_dates]
  R <- R[ava_dates]
  risk_free <- risk_free[ava_dates]
  R_e <- R - matrix(rep(risk_free, ncol(R)),
                         nrow = nrow(risk_free),
                         ncol = ncol(R))

  R_e <- apply.monthly(R_e, Return.cumulative)

  # need to calculate moving average of prices at the end of each month
  # as well as cumulative excess return
  P_month_ends <- P[endpoints(P, on = "months"), ]
  R_e <- R_e[seq(from = nrow(R_e) - n_months + 1,
                 to = nrow(R_e))]

  w <- strat$default_weights
  if (nrow(P_month_ends) >= n_months){
    P_moving <- rollmean(P_month_ends,
                         k = n_months,
                         fill = NA,
                         align= "right")
    tsmom <- cumprod(1 + R_e) - 1
    tsmom <- tsmom[nrow(tsmom)]

    # indicators to decide exposure
    ind_moving <- (P_month_ends[nrow(P_month_ends), ] > P_moving[nrow(P_moving), ])*1
    ind_tsmom <- (tsmom > 0)*1
    ind_tactical <- (ind_moving + ind_tsmom)/2

    # tactical RAA rule
    if (!any(is.na(P_moving[nrow(P_moving), ]))){
      w <- w * ind_tactical
    }
  }

  return(w)
}
