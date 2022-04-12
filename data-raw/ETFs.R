library(xts)
library(PerformanceAnalytics)

# load prices of ETFs from csv
ETFs_P <- data.table::fread(system.file("extdata",
                        "ETFs_Prices.csv",
                        package = "AssetAllocation"))
ETFs_P$Date <- as.Date(ETFs_P$Date, format="%m/%d/%Y")

ETFs_P <- as.xts(ETFs_P)

# load total return indices of ETFs from csv
ETFs_R <- data.table::fread(system.file("extdata",
                               "ETFs_TotalReturnIndices.csv",
                               package = "AssetAllocation"))
ETFs_R$Date <- as.Date(ETFs_R$Date, format="%m/%d/%Y")
ETFs_R <- as.xts(ETFs_R)
ETFs_R <- PerformanceAnalytics::CalculateReturns(ETFs_R)

# load ETFs descriptions from csv
ETFs_desc <- read.csv(system.file("extdata",
                                  "ETFs_desc.csv",
                                  package = "AssetAllocation"))

# load Treasury bill from FRED and align
quantmod::getSymbols("DGS3MO", src = "FRED")
rf <- merge.xts(ETFs_P[, 1], DGS3MO, join = "left", fill = NA)$DGS3MO/100/252
rf <- zoo::na.locf(rf)

ETFs <- list(Prices = ETFs_P,
             Returns = ETFs_R,
             Description = ETFs_desc,
             risk_free = rf)

usethis::use_data(ETFs, overwrite = TRUE)


