# helper function to determine ivy portfolio weights on one date
#' @export
ivy_port_weights <- function(w_0, prices, prices_MA){

  # initial allocation is w_0 (equally weighted)
  # if current price of an asset < moving average, shift that allocation to cash
  w_0[which(prices < prices_MA)] <- 0
  return(w_0)
}
