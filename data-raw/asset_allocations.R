# create some lists representing asset allocation strategies

# constant allocation rules
us_60_40 <- list(name = "United States 60/40",
                 tickers = c("SPY", "IEF"),
                 default_weights = c(0.60, 0.40),
                 rebalance_frequency = "month",
                 portfolio_rule_fn = "constant_weights")

golden_butterfly <- list(name = "Golden Butterfly",
                         tickers = c("SHY", "TLT", "VTI", "IWN", "GLD"),
                         default_weights = c(0.20, 0.20, 0.20, 0.20, 0.20),
                         rebalance_frequency = "month",
                         portfolio_rule_fn = "constant_weights")

rob_arnott <- list(name = "Rob Arnott Portfolio",
                   tickers = c("BNDX", "LQD", "VEU", "VNQ", "VNQ", "SPY", "TLT", "TIP", "DBC"),
                   default_weights = c(0.20, 0.20, 0.10, 0.10, 0.10, 0.10, 0.10, 0.10, 0.10),
                   rebalance_frequency = "month",
                   portfolio_rule_fn = "constant_weights")

globalAA <- list(name = "Global Asset Allocation",
                 tickers = c("SPY", "EFA", "EEM", "LQD", "BNDX", "TLT", "TIP", "DBC", "GLD", "VNQ"),
                 default_weights = c(0.18, 0.135, 0.045, 0.198, 0.144, 0.135, 0.018, 0.05, 0.05, 0.045),
                 rebalance_frequency = "month",
                 portfolio_rule_fn = "constant_weights")

permanent <- list(name = "Permanent Portfolio",
                  tickers = c("BIL", "GLD", "TLT", "SPY"),
                  default_weights = c(0.25, 0.25, 0.25, 0.25),
                  rebalance_frequency = "month",
                  portfolio_rule_fn = "constant_weights")

desert <- list(name = "Desert Portfolio",
               tickers = c("IEF", "VTI", "GLD"),
               default_weights = c(0.60, 0.30, 0.10),
               rebalance_frequency = "month",
               portfolio_rule_fn = "constant_weights")

larry <- list(name = "Larry Portfolio",
              tickers = c("IWN", "DLS", "EEM", "IEF"),
              default_weights = c(0.15, 0.075, 0.075, 0.70),
              rebalance_frequency = "month",
              portfolio_rule_fn = "constant_weights")

big_rocks <- list(name = "Big Rocks Portfolio",
                  tickers = c("AGG", "SPY", "IWD", "IWM", "IWN", "EFV", "VNQ", "EFA", "SCZ", "DLS", "EEM"),
                  default_weights = c(0.60, 0.06, 0.06, 0.06, 0.06, 0.04,0.04, 0.02, 0.02, 0.02, 0.02),
                  rebalance_frequency = "month",
                  portfolio_rule_fn = "constant_weights")

sandwich <- list(name = "Sandwich Portfolio",
                 tickers = c("IEF", "SPY", "SCZ", "IWM", "EEM", "EFA", "VNQ", "BIL"),
                 default_weights = c(0.41, 0.20, 0.10, 0.08, 0.06, 0.06, 0.05, 0.04),
                 rebalance_frequency = "month",
                 portfolio_rule_fn = "constant_weights")


balanced_tax <- list(name = "Balanced Tax Aware Portfolio",
                     tickers = c("AGG", "SPY", "BIL", "EFA", "IWM", "VNQ", "DBC", "EEM" ),
                     default_weights = c(0.38, 0.15, 0.15, 0.13, 0.05, 0.05, 0.05, 0.04),
                     rebalance_frequency = "month",
                     portfolio_rule_fn = "constant_weights")

balanced <- list(name = "Balanced Portfolio",
                 tickers = c("AGG", "SPY", "BIL", "EFA", "IWM", "VNQ", "DBC", "EEM", "TIP", "BNDX", "HYG"),
                 default_weights = c(0.33, 0.15, 0.15, 0.13, 0.05, 0.05, 0.05, 0.04, 0.02, 0.02, 0.01),
                 rebalance_frequency = "month",
                 portfolio_rule_fn = "constant_weights")

income_gr <- list(name = "Income with Growth Portfolio",
                  tickers = c("AGG", "BIL", "TIP", "SPY", "EFA", "VNQ", "HYG", "BNDX", "IWM", "DBC"),
                  default_weights = c(0.37, 0.20, 0.10, 0.09, 0.08, 0.05, 0.04, 0.04, 0.02, 0.01),
                  rebalance_frequency = "month",
                  portfolio_rule_fn = "constant_weights")

income_gr_tax <- list(name = "Income with Growth Tax Aware Portfolio",
                      tickers = c("AGG", "BIL", "SPY", "EFA", "VNQ", "IWM", "DBC"),
                      default_weights = c(0.55, 0.20, 0.09, 0.08, 0.05, 0.02, 0.01),
                      rebalance_frequency = "month",
                      portfolio_rule_fn = "constant_weights")

con_income  <- list(name = "Conservative Income",
                    tickers = c("AGG", "BIL", "TIP", "HYG", "VNQ", "BNDX"),
                    default_weights = c(0.40, 0.25, 0.18, 0.07, 0.05, 0.05),
                    rebalance_frequency = "month",
                    portfolio_rule_fn = "constant_weights")

con_income_tax  <- list(name = "Conservative Income Tax Aware",
                        tickers = c("AGG", "BIL", "VNQ"),
                        default_weights = c(0.70, 0.25, 0.05),
                        rebalance_frequency = "month",
                        portfolio_rule_fn = "constant_weights")

all_weather  <- list(name = "All Weather Portfolio",
                     tickers = c("SPY", "TLT", "IEF", "GLD", "DBC"),
                     default_weights = c(0.30, 0.40, 0.15, 0.075, 0.075),
                     rebalance_frequency = "month",
                     portfolio_rule_fn = "constant_weights")

# tactical allocations
ivy  <- list(name = "Ivy",
             tickers = c("VTI", "VEU", "VNQ", "AGG", "DBC"),
             default_weights = c(0.20, 0.20, 0.20, 0.20, 0.20),
             rebalance_frequency = "month",
             portfolio_rule_fn = "tactical_ivy")

# RAA
raa     <- list(name = "RAA Balanced",
                tickers = c("MTUM", "IWD", "EFA", "EFV", "VNQ", "DBC", "IEF"),
                default_weights = c(0.10, 0.10, 0.10, 0.10, 0.20, 0.20, 0.20),
                rebalance_frequency = "month",
                portfolio_rule_fn = "tactical_RAA")

# Dual momentum
dual_mom     <- list(name = "Antonacci's Dual Momentum",
                     tickers = c("SPY", "EFA",    # equity
                                 "HYG", "LQD",    # bonds
                                 "VNQ", "REM",    # real estate
                                 "GLD", "TLT"),   # "stress"
                     asset_class = c("Equity", "Equity",
                                     "Bond", "Bond",
                                     "REIT", "REIT",
                                     "Stress", "Stress"),
                     default_weights = c(0.25, 0.25,
                                         0.25, 0.25,
                                         0.25, 0.25,
                                         0.25, 0.25),
                     rebalance_frequency = "month",
                     portfolio_rule_fn = "tactical_DualMomentum")

# Adaptive Asset Allocation (AKA Momentum + Min Vol)
aaa     <- list(name = "Adaptive Asset Allocation",
                tickers = c("SPY",   # U.S. stocks
                            "VGK",   # European stocks
                            "EWJ",   # Japanese stocks
                            "EEM",   # Emerging stocks
                            "VNQ",   # US REITs
                            "RWX",   # International REITs
                            "IEF",   # U.S. 7-10 year Treasuries
                            "TLT",   # U.S. 20+ year Treasuries
                            "DBC",   # Commodities
                            "GLD"),  # Gold
                default_weights = rep(0.1, 10),
                rebalance_frequency = "month",
                portfolio_rule_fn = "tactical_AAA")


trend_friend     <- list(name = "Trend Friend original",
                         tickers = c("IEF",   # U.S. Intermediate-Term Bonds
                                     "BWX",   # International bonds
                                     "DBC",   # Commodities
                                     "EEM",   # Emerging stocks
                                     "EWJ",   # Japanese stocks
                                     "SPY",   # U.S. stocks
                                     "RWO",   # Real Estate, Global
                                     "VNQ",   # US REITs
                                     "EFA"),  # International Dev. Equity
                         default_weights = rep(1/9, 9),
                         rebalance_frequency = "month",
                         portfolio_rule_fn = "tactical_TrendFriend")

trend_friend_RP     <- list(name = "Trend Friend Risk Parity",
                            tickers = c("IEF",   # U.S. Intermediate-Term Bonds
                                        "BWX",   # International bonds
                                        "DBC",   # Commodities
                                        "EEM",   # Emerging stocks
                                        "EWJ",   # Japanese stocks
                                        "SPY",   # U.S. stocks
                                        "RWO",   # Real Estate, Global
                                        "VNQ",   # US REITs
                                        "EFA"),  # International Dev. Equity
                            default_weights = rep(1/9, 9),
                            rebalance_frequency = "month",
                            portfolio_rule_fn = "tactical_TrendFriend_RP")

JPM_Eff5      <- list(name = "JPM Efficiente 5",
                      tickers = c("SPY", # U.S. Equities / SPDR S&P 500® ETF Trust
                                  "IWM", # U.S. Small Cap Equities / iShares® Russell 2000 ETF
                                  "EFA", # Developed Market Equities (excluding U.S.) / iShares® MSCI EAFE ETF
                                  "TLT", # Treasuries / iShares® 20+ Year Treasury Bond ETF
                                  "LQD", # Investment Grade Bonds / iShares® iBoxx$ Investment Grade Corporate Bond ETF
                                  "HYG", # High Yield Bonds / iShares® iBoxx$ High Yield Corporate Bond ETF
                                  "EEM", # Emerging Market Equities / iShares® MSCI Emerging Markets ETF
                                  "EMB", # Emerging Market Bonds / iShares® JPMorgan USD Emerging Markets Bond ETF
                                  "IYR", # Real Estate / iShares® U.S. Real Estate ETF
                                  "GSG", # Broad Commodities / iShares® S&P GSCI™ Commodity-Indexed Trust
                                  "GLD", # Gold / SPDR® Gold Trust
                                  "TIP", # Inflation Protected Bonds iShares® TIPS Bond ETF
                                  "BIL"), # Cash / SPDR Bloomberg 1-3 Month T-Bill ETF
                      default_weights = rep(1/13, 13),
                      rebalance_frequency = "month",
                      portfolio_rule_fn = "tactical_JPM5")

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

tactical <- list(ivy = ivy,
                 raa = raa,
                 dual_mom = dual_mom,
                 aaa = aaa,
                 trend_friend = trend_friend,
                 trend_friend_RP = trend_friend_RP,
                 JPM_Eff5 = JPM_Eff5)

asset_allocations <- list(static = static,
                          tactical = tactical)

usethis::use_data(asset_allocations, overwrite = TRUE)
