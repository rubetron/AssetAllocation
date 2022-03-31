\name{ivy_port_weights}
\alias{ivy_port_weights}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ivy_port_weights
%%  ~~function to do ... ~~
}
\description{
Calculates weights for Ivy portfolio allocation. This function is not intended to be directly executed by users of the package.
}
\usage{
ivy_port_weights(w_0, prices, prices_MA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{w_0}{Default weights for assets}
  \item{prices}{Prices of assets on rebalancing date}
  \item{prices_MA}{Moving averages of prices of assets on rebalancing date}
}
\details{
The function compares the prices on the rebalancing date with their moving averages. If the price of an asset is currently below its moving average, the allocation to the asset is set to zero.
}
\value{
A vector of weights after applying the Ivy portfolio rule.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Alexandre Rubesam
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

# weights for 5 assets
(w_0 <- rep(0.20, 5))

# for reproducibility
set.seed(12345)

# some random prices
(prices <- rnorm(5, 100, 15))

# some random prices for the moving averages
(prices_MA <- rnorm(5, 100, 15))

# apply the Ivy portfolio rule
(ivy_port_weights(w_0, prices, prices_MA))

# Ivy set the weight of asset 4 to zero since the price is below the moving average
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
