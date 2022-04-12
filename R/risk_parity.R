#' Returns risk parity weights on a given date
#'
#' \code{risk_parity} determines asset allocations using a risk parity rule.
#' It obtains the weights such that all assets provide the same risk
#' contribution to the risk of the portfolio.
#'
#' The function calculates the covariance matrix of returns using the last two
#' years (or minimum of one year) of daily returns.
#' @param strat A list representing an asset allocation strategy.
#' @param reb_date A date on which the allocation rule is applied.
#' @param P An xts object with daily prices of the tickers in strat.
#' @param R An xts object with daily returns of the tickers in strat.
#' @examples
#' ivy  <- asset_allocations$tactical$ivy
#' reb_date <- as.Date("2022-03-31")
#' risk_parity(ivy, reb_date, ETFs$Prices[, ivy$tickers], ETFs$Returns[, ivy$tickers])
#' @return A numeric vector of weights after applying the rule.
#' @export
#' @importFrom xts endpoints
#' @importFrom zoo rollmean
#' @importFrom RiskPortfolios covEstimation
#' @importFrom nloptr nloptr
# Ivy portfolio allocation
risk_parity <- function(strat, reb_date, P, R){

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

    # risk parity optimization
    opts <- list("algorithm" = "NLOPT_LN_COBYLA",
                 "xtol_rel" = 1e-12, "print_level" = 0)

    lmr <- nloptr(x0 = rep(1/n_assets, n_assets),
                  eval_f = port_vol,
                  lb = rep(0, n_assets),
                  ub = rep(100, n_assets),
                  eval_g_ineq = rp_nonlinear_constraint,
                  opts = opts,
                  cov_mat = cov_mat,
                  b = strat$default_weights,
                  c = -0.25)
    rp_weight <- lmr$solution
    rp_weight <- rp_weight/sum(rp_weight)

    # check that risk contributions are equal
    rcs <- rp_weight * (cov_mat %*% rp_weight)
    rcs <- rcs/port_vol(rp_weight, cov_mat)
    print(rcs)
  } else {
    rp_weight <- rep(0, n_assets)
  }
  return(rp_weight)
}
