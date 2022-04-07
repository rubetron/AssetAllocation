# create some lists representing asset allocation strategies

# constant allocation rules
us_60_40 <- list(name = "United States 60/40",
                 tickers = c("SPY", "IEF"),
                 default_weights = c(0.60, 0.40),
                 rebalance_frequency = "month",
                 portfolio_rule_fn = "identity")

golden_butterfly <- list(name = "Golden Butterfly",
                         tickers = c("SHY", "TLT", "VTI", "IWN", "GLD"),
                         default_weights = c(0.20, 0.20, 0.20, 0.20, 0.20),
                         rebalance_frequency = "month",
                         portfolio_rule_fn = "identity")

rob_arnott <- list(name = "Rob Arnott Portfolio",
                   tickers = c("BNDX", "LQD", "VEU", "VNQ", "VNQ", "SPY", "TLT", "TIP", "DBC"),
                   default_weights = c(0.20, 0.20, 0.10, 0.10, 0.10, 0.10, 0.10, 0.10, 0.10),
                   rebalance_frequency = "month",
                   portfolio_rule_fn = "identity")

globalAA <- list(name = "Global Asset Allocation",
                 tickers = c("SPY", "EFA", "EEM", "LQD", "BNDX", "TLT", "TIP", "DBC", "GLD", "VNQ"),
                 default_weights = c(0.18, 0.135, 0.045, 0.18, 0.144, 0.135, 0.018, 0.05, 0.05, 0.045),
                 rebalance_frequency = "month",
                 portfolio_rule_fn = "identity")

permanent <- list(name = "Permanent Portfolio",
                  tickers = c("BIL", "GLD", "TLT", "SPY"),
                  default_weights = c(0.25, 0.25, 0.25, 0.25),
                  rebalance_frequency = "month",
                  portfolio_rule_fn = "identity")

desert <- list(name = "Desert Portfolio",
               tickers = c("IEF", "VTI", "GLD"),
               default_weights = c(0.60, 0.30, 0.10),
               rebalance_frequency = "month",
               portfolio_rule_fn = "identity")

larry <- list(name = "Larry Portfolio",
              tickers = c("IWN", "DLS", "EEM", "IEF"),
              default_weights = c(0.15, 0.075, 0.075, 0.70),
              rebalance_frequency = "month",
              portfolio_rule_fn = "identity")

big_rocks <- list(name = "Big Rocks Portfolio",
                  tickers = c("AGG", "SPY", "IWD", "IWM", "IWN", "EFV", "VNQ", "EFA", "SCZ", "DLS", "EEM"),
                  default_weights = c(0.60, 0.06, 0.06, 0.06, 0.06, 0.04,0.04, 0.02, 0.02, 0.02, 0.02),
                  rebalance_frequency = "month",
                  portfolio_rule_fn = "identity")

sandwich <- list(name = "Sandwich Portfolio",
                 tickers = c("IEF", "SPY", "SCZ", "IWM", "EEM", "EFA", "VNQ", "BIL"),
                 default_weights = c(0.41, 0.20, 0.10, 0.08, 0.06, 0.06, 0.05, 0.04),
                 rebalance_frequency = "month",
                 portfolio_rule_fn = "identity")


balanced_tax <- list(name = "Balanced Tax Aware Portfolio",
                     tickers = c("AGG", "SPY", "BIL", "EFA", "IWM", "VNQ", "DBC", "EEM" ),
                     default_weights = c(0.38, 0.15, 0.15, 0.13, 0.05, 0.05, 0.05, 0.04),
                     rebalance_frequency = "month",
                     portfolio_rule_fn = "identity")

balanced <- list(name = "Balanced Portfolio",
                 tickers = c("AGG", "SPY", "BIL", "EFA", "IWM", "VNQ", "DBC", "EEM", "TIP", "BNDX", "HYG"),
                 default_weights = c(0.33, 0.15, 0.15, 0.13, 0.05, 0.05, 0.05, 0.04, 0.02, 0.02, 0.01),
                 rebalance_frequency = "month",
                 portfolio_rule_fn = "identity")

income_gr <- list(name = "Income with Growth Portfolio",
                  tickers = c("AGG", "BIL", "TIP", "SPY", "EFA", "VNQ", "HYG", "BNDX", "IWM", "DBC"),
                  default_weights = c(0.37, 0.20, 0.10, 0.09, 0.08, 0.05, 0.04, 0.04, 0.02, 0.01),
                  rebalance_frequency = "month",
                  portfolio_rule_fn = "identity")

income_gr_tax <- list(name = "Income with Growth Tax Aware Portfolio",
                      tickers = c("AGG", "BIL", "SPY", "EFA", "VNQ", "IWM", "DBC"),
                      default_weights = c(0.55, 0.20, 0.09, 0.08, 0.05, 0.02, 0.01),
                      rebalance_frequency = "month",
                      portfolio_rule_fn = "identity")

con_income  <- list(name = "Conservative Income",
                    tickers = c("AGG", "BIL", "TIP", "HYG", "VNQ", "BNDX"),
                    default_weights = c(0.40, 0.25, 0.18, 0.07, 0.05, 0.05),
                    rebalance_frequency = "month",
                    portfolio_rule_fn = "identity")

con_income_tax  <- list(name = "Conservative Income Tax Aware",
                        tickers = c("AGG", "BIL", "VNQ"),
                        default_weights = c(0.70, 0.25, 0.05),
                        rebalance_frequency = "month",
                        portfolio_rule_fn = "identity")

all_weather  <- list(name = "All Weather Portfolio",
                     tickers = c("SPY", "TLT", "IEF", "GLD", "DBC"),
                     default_weights = c(0.30, 0.40, 0.15, 0.075, 0.075),
                     rebalance_frequency = "month",
                     portfolio_rule_fn = "identity")

# tactical allocation rules
ivy  <- list(name = "Ivy",
             tickers = c("VTI", "VEU", "VNQ", "AGG", "DBC"),
             default_weights = c(0.20, 0.20, 0.20, 0.20, 0.20),
             rebalance_frequency = "month",
             portfolio_rule_fn = tactical_ivy)


static <- list(us_60_40 = us_60_40,
               golden_butterfly = golden_butterfly,
               rob_arnott= rob_arnott,
               globalAA = globalAA,
               permanent = permanent,
               desert = desert,
               larry = larry,
               big_rocks = big_rocks,
               sandwich = sandwich,
               balanced_tax = balanced_tax,
               balanced = balanced,
               income_gr = income_gr,
               income_gr_tax = income_gr_tax,
               con_income = con_income,
               con_income_tax = con_income_tax,
               all_weather = all_weather)
tactical <- list(ivy)

basic_asset_alloc <- list(static, tactical)

usethis::use_data(basic_asset_alloc, overwrite = TRUE)
