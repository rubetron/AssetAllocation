#' Full investment constraint for minimum variance portfolio optimization
#'
#' \code{min_variance_constraint} is a helper function for the
#' \code{min_variance} function.
#'
#' The function implements a full investment constraint necessary in the
#' optimization.
#' @param x A vector of weights.
#' @param cov_mat A covariance matrix, passed to optimizer
#' @param b A vector of desired risk budgets
#' @param c A scalar.
#' @return A scalar with the sum of the portfolio weights.
#' @export
min_variance_constraint <- function(x, cov_mat, b, c) {
  sum(x)-1
}
