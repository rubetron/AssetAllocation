#' Portfolio volatility
#'
#' \code{port_vol} is a helper function for the \code{risk_parity} function.
#'
#' The function calculates the volatility of a portfolio given weights and the
#' covariance matrix.
#' @param x A vector of weights.
#' @param cov_mat A covariance matrix.
#' @param b A scalar parameter, passed to optimizer.
#' @param c A scalar parameter, passed to optimizer.
#' @examples
#' x <- c(0.75, 0.25)
#' cov_mat <- matrix(c(0.25^2, 0.00125, 0.00125, 0.05^2),
#'                   nrow = 2, ncol = 2)
#' (port_vol(x, cov_mat))
#' @return A scalar with the portfolio volatility.
#' @export
port_vol <- function(x, cov_mat, b,c) {
  as.numeric(sqrt(t(x) %*% cov_mat %*% x))
}
