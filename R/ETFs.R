#' Daily prices and total returns for 24 ETFs.
#'
#' Data set containing daily prices and total returns for 37 exchange-traded
#' funds (ETFs) as well as daily returns for U.S. Treasury bills
#' (risk-free asset).
#'
#' @docType data
#'
#' @usage data(ETFs)
#'
#' @format An object of class \code{"list"}
#' \describe{
#'  \item{Prices}{\code{xts} object with daily prices}
#'  \item{Returns}{\code{xts} object with daily total returns}
#'  \item{Description}{\code{data.frame} with information about the ETFs}
#'  \item{risk_free}{\code{xts} object with daily returns of U.S. Treasury bills}
#' }
#'
#' @keywords datasets
#' @examples
#'
#' data(ETFs)
#' head(ETFs$Prices)
#' ETFs$Description
#'
"ETFs"
