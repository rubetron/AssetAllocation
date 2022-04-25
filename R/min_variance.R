#' Returns minimum variance portfolio weights on a given date
#'
#' \code{min_variance} determines asset allocations that minimize the variance
#' of aportfolio.
#'
#' The function calculates the covariance matrix of returns using the last two
#' years (or minimum of one year) of daily returns. It relies on the
#' \code{minvar} function from the \code{NMOF} package.
#' @param strat A list representing an asset allocation strategy.
#' @param reb_date A date on which the allocation rule is applied.
#' @param P An xts object with daily prices of the tickers in strat.
#' @param R An xts object with daily returns of the tickers in strat.
#' @param risk_free Either an xts object with daily returns of the risk-free
#' asset, or a scalar numeric with the annual risk-free rate in decimals.
#'
#' @examples
#' ivy  <- asset_allocations$tactical$ivy
#' reb_date <- as.Date("2022-03-31")
#' risk_parity(ivy, reb_date, ETFs$Prices[, ivy$tickers], ETFs$Returns[, ivy$tickers])
#' @return A numeric vector of weights after applying the rule.
#' @export
#' @importFrom xts endpoints
#' @importFrom zoo rollmean
#' @importFrom RiskPortfolios covEstimation
#' @importFrom NMOF minvar
# Ivy portfolio allocation
min_variance <- function(strat, reb_date, P, R, risk_free = NULL){

  # check that user supplied a specific window for cov estimation
  # if not, use the default 2 years (2*252 days)
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

  # check that user supplied a specific lambda for EMWA
  # if not, use the default
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

  # calculations use data until the reb_date
  ava_dates <- paste0("/", reb_date)
  R <- R[ava_dates]
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
    mvp_weights <- minvar(cov_mat)

  } else {
    mvp_weights <- rep(0, n_assets)
  }
  return(mvp_weights)
}
