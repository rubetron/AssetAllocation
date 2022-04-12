#' Calculation of account value for backtesting asset allocation strategies
#'
#' \code{daily_account_calc} is a helper function used by
#' \code{backtest_allocation} to calculate theoretical the theoretical account
#' value given an initial allocation to assets. It is not intended to be called
#' directly by the user.
#'
#' The function simulates the value of a theoretical account from the initial
#' weights and the daily returns of a set of assets.
#'
#' @param w A vector of weights
#' @param R An xts object with daily returns of the tickers in strat.
#' @return A numeric vector with the daily value of the account.
#'
#' @export
daily_account_calc <- function(w, R){

  # some dimension checks
  if (length(w) != ncol(R)){
    stop('Number of elements of w should match the number of columns in R')
  }

  if (sum(w==0) == length(w)){
    return(rep(0, nrow(R) + 1))
  } else{
    n_assets <- ncol(R)
    n_periods <- nrow(R)

    PortMatrix <- matrix(0, nrow = n_periods + 1, ncol = n_assets)

    # Assume initial position in each stock equals the initial weight
    PortMatrix[1, 1:n_assets] <- w
    PortMatrix[2:nrow(PortMatrix), 1:n_assets] <- 1 + R
    PortMatrix <- apply(PortMatrix, 2, cumprod)
    port_notional <- rowSums(PortMatrix[, 1:n_assets])
    return(port_notional)
  }
}
