#' Nonlinear constraint for risk parity portfolio optimization
#'
#' \code{rp_nonlinear_constraint} is a helper function for the
#' \code{risk_parity} function.
#'
#' The function implements a nonlinear constraint necessary in the optimization.
#' @param x A vector of weights.
#' @param cov_mat A covariance matrix, passed to optimizer
#' @param b A vector of desired risk budgets
#' @param c A scalar.
#' @examples
#' x <- c(0.75, 0.25)
#' cov_mat <- matrix(c(0.25^2, 0.00125, 0.00125, 0.05^2),
#'                   nrow = 2, ncol = 2)
#' b <- c(0.5, 0.5)
#' c <- -10
#' (rp_nonlinear_constraint(x, cov_mat, b, c))
#' @return A scalar with the portfolio volatility.
#' @export
rp_nonlinear_constraint <- function(x, cov_mat, b, c) {
  c -  sum(b * log(x))
}
