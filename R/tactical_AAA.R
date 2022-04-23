#' Returns allocations for the Adaptive Asset Allocation strategy on a given
#' date
#'
#' \code{tactical_AAA} determines asset allocations according to the Adaptive
#' Asset Allocation approach described in Butler, Philbrick, Gordillo, and
#' Varadi (2012) <doi:https://dx.doi.org/10.2139/ssrn.2328254>.
#'
#' The Adaptive Asset Allocation strategy sorts a specific list of assets
#' based on 6-month momentum, selects the top 5 assets, and then calculates
#' weights that yield the minimum portfolio variance. The parameters controlling
#' the number of months for the momentum calculation (\code{n_months_mom},
#' default = 6), number of months of daily data used to estimate the covariance
#' matrix (\code{n_months_mom}, default value = 1), and the number of assets to
#' select using the momentum rule (\code{n_assets}, default = 5) can be changed
#' by adding them to a list called \code{param} in the \code{strat} object. This
#' allows the user to apply the simple principle of the strategy (momentum and
#' minimum variance) to any set of assets.
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
#' @return A numeric vector of weights after applying the rule.
#' @export
#' @import xts
#' @importFrom xts endpoints
#' @importFrom PerformanceAnalytics Return.cumulative
# raa portfolio allocation
tactical_AAA <- function(strat, reb_date, P, R, risk_free){

  # check that user supplied a specific window.
  # if not, use the default 6 months
  if (length(strat$params) > 0){
    # check that there is element n_months_mom in params
    if ("n_months_mom" %in% names(strat$params)){
      n_months_mom <- strat$params$n_months_mom
    } else{
      print("n_months_mom not found in strat$params. Defaulting to 6")
      n_months_mom <- 6 # default look-back
    }

    # check if user provided a number of months for cov matrix calculation
    if ("n_months_cov" %in% names(strat$params)){
      n_months_cov <- strat$params$n_months_cov
    } else{
      print("n_months_cov not found in strat$params. Defaulting to 1")
      n_months_cov <- 1 # default vol look-back
    }

    # check if user provided a number of assets to be selected
    if ("n_assets" %in% names(strat$params)){
      n_assets <- strat$params$n_assets
    } else{
      if (length(strat$tickers) > 5){
        print("n_assets not found in strat$params. Defaulting to 5")
        n_assets <- 5
      }
      else{
        stop("Number of assets less than 5 with no default for n_assets")
      }
    }
  } else {
    n_months_mom <- 6 # default momentum look-back
    n_months_cov <- 1 # default vol look-back
    if (length(strat$tickers) > 5){
      n_assets <- 5
    } else {
      print("Number < 5 with no default for n_assets. Momentum will not be applied")
      n_assets <- length(strat$tickers)
      }
    }


  # comparison will be made using dates until the reb_date
  ava_dates <- paste0("/", reb_date)
  P <- P[ava_dates]
  R <- R[ava_dates]
  R_m <- apply.monthly(R, Return.cumulative)
  R_cum <- R_m[seq(from = nrow(R_m) - n_months_mom + 1,
                 to = nrow(R_m))]

  R_cum <- cumprod(1+R_cum[seq(from = nrow(R_cum) - n_months_mom + 1,
                         to = nrow(R_cum)), ])
  R_cum <- R_cum[nrow(R_cum), ]

  # select top n_assets based on R_cum
  sort_mom <- rank(as.numeric(R_cum))
  sel_assets <- which(sort_mom > length(strat$tickers) - n_assets)

  # check if there is at least one year of daily data
  if (nrow(R) > n_months_cov*21){

    # it's not clear what window is used to estimate vol. Will use default 1 month
    #D <- diag(apply(R[seq(from = nrow(R) - n_months_cov*21 + 1,
    #                 to = nrow(R)), sel_assets], 2, sd))

    #C <- cor(R[seq(from = nrow(R) - 6*21 + 1,
    #               to = nrow(R)), sel_assets])


    #cov_mat <- D %*% C %*% D
    cov_mat <- cov(R[seq(from = nrow(R) - n_months_cov*21 + 1,
                         to = nrow(R)), sel_assets])

    # apply minimum variance optimization
    mvp_weights <- minvar(cov_mat)

  } else {
    mvp_weights <- rep(0, n_assets)
  }

  weights <- rep(0, length(strat$tickers))
  weights[sel_assets] <- mvp_weights
  return(weights)
}
