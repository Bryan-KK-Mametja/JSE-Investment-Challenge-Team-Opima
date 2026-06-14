library(TTR)
library(yfR)
library(ggplot2)
library(quantmod)
library(tidyverse)
library(PerformanceAnalytics)

library(quantmod)

ticker <- "PPC.JO"

data <- getSymbols(ticker, src = "yahoo",
                   from = "2025-01-01",
                   auto.assign = FALSE)

#prices
price <- Cl(data)

#returns
returns <- dailyReturn(Cl(data))

# momentum
price_vec <- na.omit(as.numeric(price))

momentum_1m <- (last(price_vec) / first(tail(price_vec, 21)) - 1) * 100
momentum_3m <- (last(price_vec) / first(tail(price_vec, 63)) - 1) * 100
momentum_6m <- (last(price_vec) / first(tail(price_vec, 126)) - 1) * 100

# moving averages
ma20 <- SMA(price, 20)
ma50 <- SMA(price, 50)

ma20_clean <- na.omit(ma20)
ma50_clean <- na.omit(ma50)
price_clean <- na.omit(price)

signal <- ifelse(
  as.numeric(last(price_clean)) > as.numeric(last(ma20_clean)) &
    as.numeric(last(ma20_clean)) > as.numeric(last(ma50_clean)),
  "Bullish",
  "Bearish"
)

# relative Strength Index.
# overbought (> 70, strong), 50-70 (healthy), Oversold (< 30 unhealthy)
rsi <- RSI(price, n = 14)
tail(rsi,1)

# volatility
volatility <- sd(returns) * sqrt(252)

# sharp-ratio
sharpe <- SharpeRatio.annualized(returns)
sharpe_value <- as.numeric(sharpe)

# score model
momentum_norm <- as.numeric(momentum_3m / 100)
rsi_value <- as.numeric(last(rsi))
rsi_norm <- (rsi_value - 50) / 50
signal_value <- ifelse(signal == "Bullish", 1, 0)

score <- 0.4 * momentum_norm +
  0.3 * sharpe_value +
  0.2 * rsi_norm +
  0.1 * signal_value