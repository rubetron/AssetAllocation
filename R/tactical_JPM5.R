#' Calculates asset allocations for the JPMorgan ETF Efficiente® 5 portfolio.
#'
#' \code{tactical_JPM5} determines asset allocations using a replication
#' of the JPMorgan ETF Efficiente® 5 index methodology described in publicly
#' available documentation (<https://sp.jpmorgan.com/spweb/content/307403.pdf>).
#'
#' The strategy uses a window of six months of daily data to compute inputs to
#' perform a constrained mean-variance optimization. It relies on the
#' \code{mvFrontier} function from the \code{NMOF} package.
#' @param strat A list representing an asset allocation strategy.
#' @param reb_date A date on which the allocation rule is applied.
#' @param P An xts object with daily prices of the tickers in strat.
#' @param R An xts object with daily returns of the tickers in strat.
#' @param risk_free Either an xts object with daily returns of the risk-free
#' asset, or a scalar numeric with the annual risk-free rate in decimals.
#'
#' @examples
#' JPM_Eff5  <- asset_allocations$tactical$JPM_Eff5
#' reb_date <- as.Date("2022-03-31")
#' tactical_JPM5(JPM_Eff5, reb_date, ETFs$Prices[, JPM_Eff5$tickers], ETFs$Returns[, JPM_Eff5$tickers])
#' @return A numeric vector of weights after applying the rule.
#' @export
#' @importFrom xts endpoints
#' @importFrom zoo rollmean
#' @importFrom RiskPortfolios covEstimation
#' @importFrom NMOF mvFrontier
# Ivy portfolio allocation
tactical_JPM5 <- function(strat, reb_date, P, R, risk_free = NULL){

  # check that user supplied a specific window for cov estimation
  # if not, use the default 2 years (2*252 days)
  if (length(strat$params) > 0){
    # check that there is element n_days_cov in params
    if ("n_days_cov" %in% names(strat$params)){
      n_days_cov <- strat$params$n_days_cov
    } else{
      warning("n_days_cov not found in strat$params. Defaulting to 126 (six months)")
      n_days_cov <- 126*1 # default cov estimations window in days
    }
  } else {
    n_days_cov <- 126*1 # default cov estimations window in days
  }


  # asset caps
  LB <- rep(0, 13)
  UB <- c(0.20, 0.10, 0.20 ,0.20, 0.20, 0.20, 0.20, 0.20, 0.20, 0.10, 0.10, 0.50, 0.50)

  # sectors and sector caps
  sectors <- list(  1:3,    # Group 1 = Developed Equity
                    4:6,    # Group 2 = Bonds
                    7:8,    # Group 3 = Emerging Markets
                    9:11,   # Group 4 = Alternative Investments
                    12:13   # Group 5 = Inflation Protected Bonds/Cash
                  )

  sectors_LB <- rep(0, 5)
  sectors_UB <- c(0.50, 0.50, 0.25, 0.25, 0.50)

  # calculations use data until the reb_date
  ava_dates <- paste0("/", reb_date)
  R <- R[ava_dates]
  R <- R[seq(from = max(1, nrow(R) - n_days_cov + 1),
             to = nrow(R)), ]

  n_assets <- length(strat$tickers)

  # get valid data
  R <- R[seq(from = which.max(!is.na(rowSums(R))),
             to = nrow(R)), ]

  # check if there is at least six months of daily data
  if (nrow(R) >= 120){
    # estimate inputs
    m <- colMeans(R)*252

    # estimate covariance matrix with available data
    cov_mat <- covEstimation(R,
                             control = list(type = 'naive'))*252

    p1 <- mvFrontier(m, cov_mat, wmin = LB, wmax = UB, groups = sectors,
                     groups.wmin = sectors_LB, groups.wmax = sectors_UB,n = 50)

    # select first portfolio on the efficient frontier with vol of 5%
    sel_port <- which(p1$volatility>= 0.05)

    # if no portfolio attained 5% vol, take the last portfolio in the frontier
    # otherwise, select the first portfolio with volatility >= 5%
    if (length(sel_port) == 0){
      port_weights <- p1$portfolios[, ncol(p1$portfolios)]
    } else {
      port_weights <- p1$portfolios[, min(sel_port)]
    }

  } else {
    port_weights <- rep(0, n_assets)
  }
  return(port_weights)
}
