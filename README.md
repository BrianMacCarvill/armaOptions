# ARMA Models to European Value Stock Options

## Overview

This package provides ways to estimate the value of European stock options given stock price data. It includes functions for calculating option values based on Auto-Regressive–Moving-Average (ARMA) models and also returns information about these models. This package is make to be easy to understand and is built for financial analysis capabilities, however it can be used in many other situations. This package is dependent on the 'forecast' and 'stats' packages.

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


# armaOptions Package Theoretical Explanation

## Setup

These functions are based on the assumption that price data follow the following equation

$$
P_t = \beta_0 + \beta_1 \, t + X_t
$$

where \(t\) is time, \(\beta_0\) and \(\beta_1\) are linear regression parameters, and \(X_t\) are variables from an ARMA(p,q) model.

An ARMA(p,q) model is defined as

$$
X_t = \epsilon_t + \sum_{i=1}^{p}\phi_i X_{t-i} + \sum_{j=1}^{q}\theta_j \epsilon_{t-j}
$$

where \(\epsilon_t \sim \mathcal{N}(0,1)\) for all \(t \in \mathbb{N}\), and \(\phi\) and \(\theta\) are stationary time series parameters.

So let's say our friend Bob wants to sell us a European put option at a price \(S\), \(h\) days into the future. How do we find a fair price? This put option is only valuable if, in \(h\) days, \(P_{t+h} < S\) because then we can sell a security at a value greater than its price, and the option is worthless if \(P_{t+h} > S\).

## Method

The expected value can therefore be written as

$$
\begin{aligned}
\text{Expected Value} &= \int_{-\infty}^{\infty} \max(S - P_{t+h}, 0) \, f(P_{t+h}) \, dP_{t+h} \\
&= \int_{-\infty}^{S} (S - P_{t+h}) \, f(P_{t+h}) \, dP_{t+h} \\
&= S \int_{-\infty}^{S} f(P_{t+h}) \, dP_{t+h}
    - \int_{-\infty}^{S} P_{t+h} \, f(P_{t+h}) \, dP_{t+h} \\
&= S \, \mathbb{P}(P_{t+h} < S)
    - \mathbb{E}(P_{t+h} \mid P_{t+h} < S) \, \mathbb{P}(P_{t+h} < S) \\
&= \left( S - \mathbb{E}(P_{t+h} \mid P_{t+h} < S) \right)
    \, \mathbb{P}(P_{t+h} < S).
\end{aligned}
$$

where \(f(P_{t+h})\) is the distribution function of \(P_{t+h}\).

We have defined the stock price at point \(t+h\) as

$$
P_{t+h} = \beta_0 + \beta_1 \, (t+h) + X_{t+h}.
$$

Since \(P_{t+h} < S \implies X_{t+h} < S - \beta_0 - \beta_1 (t+h)\), if we assume knowledge of the regression parameters \(\beta_0\) and \(\beta_1\) then \(S - \beta_0 - \beta_1 (t+h)\) is a deterministic value and so

$$
\left( S - \mathbb{E}(P_{t+h} \mid P_{t+h} < S) \right)
\mathbb{P}(P_{t+h} < S)
=
\left( S_r - \mathbb{E}(X_{t+h} \mid X_{t+h} < S_r) \right)
\mathbb{P}(X_{t+h} < S_r)
$$

where

$$
S_r = S - \beta_0 - \beta_1 (t+h).
$$

For an ARMA(p,q) model, assuming knowledge of the model parameters, at a forecasted value \(X_{t+h}\) forecasted \(h\) days into the future this value follows a normal distribution with a distribution function

$$
\mathcal{N}\left( \hat{X}(t+h), \hat{\sigma}(t+h) \right).
$$

For more details about how the forecasting or time series parameter fitting is performed, I recommend chapters 3 and 4 of the book *Time Series Analysis* by Lewis Hamilton or the source code and documentation to the R package **forecast**.

The logic for call options is exactly the same, just in the opposite direction.

