

# ARMA Models to European Value Stock Options

## Overview

This package provides ways to estimate the value of European stock options given stock price data. It includes functions for calculating option values based on Auto-Regressive–Moving-Average (ARMA) models and also returns information about these models. This package is make to be easy to understand and for financial analysis capabilities. This package is dependent on the 'forecast' and 'stats' packages.

## Installation

The current version of the `armaOptions` package can be installed with:

```r
devtools::install_github("BrianMacCarvill/armaOptions")
```

## Usage
Calculating put option values for simulated data.

```r
library(armaOptions)
library(forecast)
library(stats)

# Create simulated data
n = 100
set.seed(42)
arma_values = arima.sim(n = n, model = list(ar = c(0.6), ma = c(0.5, -0.5)))
linear_model = 5 + 1:n
stock_data = arma_values + linear_model

# Define a sell value and future times
sell_value = 110
future_times = c(1, 3, 5)

# Calculate put option values over a list of future times
results = PutOptionsOverTime(stock_data = stock_data, future_times = future_times, sell_value = sell_value)

# Print results
print(results)
```


```r
# Define a list of sell values
sell_values = seq(90, 110, length.out = 5)
future_time = 2

# Calculate put option values over a list of sell values
results = PutOptionsOverStrikePrices(stock_data = stock_data, future_time = future_time, sell_values = sell_values)

# Print results
print(results)
```
