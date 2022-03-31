# helper function to calculate compounded daily returns from initial allocation
#' @export
daily_ret_calc <- function(w, R){

  # some dimension checks
  if (length(w) != ncol(R)){
    stop('Number of elements of w should match the number of columns in R')
  }

  n_assets <- ncol(R)
  n_periods <- nrow(R)

  PortMatrix <- matrix(0, nrow = n_periods + 1, ncol = n_assets)

  # Assume initial position in each stock equals the initial weight
  PortMatrix[1, 1:n_assets] <- w
  PortMatrix[2:nrow(PortMatrix), 1:n_assets] <- 1 + R
  PortMatrix <- apply(PortMatrix, 2, cumprod)
  port_notional <- rowSums(PortMatrix[, 1:n_assets])
  return(port_notional[2:length(port_notional)]/port_notional[1:(length(port_notional) - 1)] - 1)
}
