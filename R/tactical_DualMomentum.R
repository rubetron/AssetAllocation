#' Returns allocations for the dual momentum strategy on a given date
#'
#' \code{tactical_DualMomentum} determines asset allocations for a strategy
#' according to the dual momentum approach described in Antonacci (2016)
#' <https://dx.doi.org/10.2139/ssrn.2042750>.
#'
#' Dual momentum sorts assets within each asset class described in \code{strat}
#' on a relative basis (i.e. which asset outperforms others within the same
#' asset class) over the last 12 months, as well as whether an asset has
#' positive excess return over the last 12 months. Dual momentum invests in the
#' top performing asset within the asset class, as long as it also has positive
#' excess return over the risk-free rate. Otherwise, the allocation is shifted
#' to the risk-free asset.
#' Any amounts not allocated to risky assets are allocated to the risk-free
#' asset as implemented in the \code{backtest_allocation} function.
#'
#' @param strat A list representing an asset allocation strategy. For this
#' particular strategy, \code{strat$asset_class} must contain a character vector
#' containing the corresponding asset classes.
#' @param reb_date A date on which the allocation rule is applied.
#' @param P An xts object with daily prices of the tickers in strat.
#' @param R An xts object with daily returns of the tickers in strat.
#' @param risk_free Either an xts object with daily returns of the risk-free
#' asset, or a scalar numeric with the annual risk-free rate in decimals.
#'
#' @examples
#' dual_mom  <- asset_allocations$tactical$dual_mom
#' reb_date <- as.Date("2022-03-31")
#' tactical_DualMomentum(dual_mom,
#'              reb_date,
#'              ETFs$Prices[, dual_mom$tickers],
#'              ETFs$Returns[, dual_mom$tickers],
#'              ETFs$risk_free)
#' @return A numeric vector of weights after applying the rule.
#' @export
#' @import xts
#' @importFrom xts endpoints
#' @importFrom PerformanceAnalytics Return.cumulative
tactical_DualMomentum <- function(strat, reb_date, P, R, risk_free){

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

  # check that user has provided asset classes
  if (!"asset_class" %in% names(strat)){
    stop("strat$asset_class missing")
  }

  asset_classes <- unique(strat$asset_class)
  n_asset_classes <- length(asset_classes)

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
  w <- strat$default_weights
  if (nrow(R_e) >= n_months){
    R_e <- R_e[seq(from = nrow(R_e) - n_months + 1,
                   to = nrow(R_e))]
    tsmom <- cumprod(1 + R_e) - 1
    tsmom <- as.numeric(tsmom[nrow(tsmom)])

    for (ac in asset_classes){
      inds <- which(strat$asset_class == ac)
      if (length(inds) ==1){
        # only one asset in asset class, just check excess ret > 0
        if (tsmom[inds] < 0){
          w[inds] <- 0
        }
      } else {
        # best asset
        best_ind <- which.max(tsmom[inds])
        # zero allocation to worst performers
        w[inds[-best_ind]] <- 0
        # zero allocation if best asset has negative excess return
        w[inds[best_ind]] <- ifelse(tsmom[best_ind] < 0, 0, w[inds[best_ind]])
      }
    }
  }
  return(w)
}
