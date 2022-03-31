\name{daily_ret_calc}
\alias{daily_ret_calc}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{daily_ret_calc
%%  ~~function to do ... ~~
}
\description{
This is a helper function used by backtest_allocation to calculate daily portfolio returns based on begining of period weights. This function is not intended to be directly executed by users of the package.
}
\usage{
daily_ret_calc(w, R)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{w}{vector of weights}
  \item{R}{matrix of daily returns. Number of columns should match length of w}
}
\details{
The function compounds each position using the daily returns to calculate a portfolio value. The return is calculated as the simple return on the portfolio value.
}
\value{Returns a vector of numeric type whose number of elements equals the number of rows in R.}
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
# tickers for the United States 60/40 allocation:
(strat <- basic_asset_alloc$us_60_40)
(tickers <- strat$tickers)

# get returns for one month from the data set ETF_daily
(ret_month <- ETFs_daily["2020-01", tickers])

# calculate daily returns
(daily_ret_calc(strat$default_weights, ret_month))
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
