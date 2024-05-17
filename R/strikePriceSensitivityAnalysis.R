#' Strike Price Sensitivity Analysis for European Call Option
#'
#' This function analyzes the sensitivity of the European call option value to changes in the strike price and future time periods.
#' It calculates the option value for a range of strike prices and future times using the `europeanCallOptionValue` function.
#'
#' @param stock_data Numeric vector representing the historical stock prices.
#' @param future_times Numeric vector representing the future time periods (in the same units as stock_data) for which the option value is calculated.
#' @param strike_prices Numeric vector representing the range of strike prices to analyze.
#' @param max.p The maximum order of the autoregressive part of the ARMA model (default is 5).
#' @param max.q The maximum order of the moving average part of the ARMA model (default is 5).
#'
#' @return A data frame containing the future times, strike prices, and their corresponding option values.
#'
#' @examples
#' \dontrun{
#' stock_data <- c(100, 102, 105, 107, 110, 108, 105, 103, 101, 99, 98, 97)
#' future_times <- c(1, 3, 5)
#' strike_prices <- seq(90, 130, by = 5)
#' result <- strikePriceSensitivityAnalysis(stock_data, future_times, strike_prices)
#' print(result)
#' }
#'
#' @importFrom forecast auto.arima forecast
#' @importFrom stats coef lm residuals
#' @export
strikePriceSensitivityAnalysis <- function(stock_data, future_times, strike_prices, max.p = 5, max.q = 5) {
  # Validate inputs
  if (!is.numeric(stock_data) || length(stock_data) < 2) {
    stop("stock_data must be a numeric vector with at least two elements.")
  }

  if (!is.numeric(future_times) || length(future_times) < 1) {
    stop("future_times must be a numeric vector with at least one element.")
  }

  if (!is.numeric(strike_prices) || length(strike_prices) < 1) {
    stop("strike_prices must be a numeric vector with at least one element.")
  }

  # Initialize a data frame to store results
  results <- expand.grid(future_time = future_times, strike_price = strike_prices)
  results$option_value <- NA

  # Loop over each combination of future time and strike price to calculate the option value
  for (i in seq_len(nrow(results))) {
    future_time <- results$future_time[i]
    sell_value <- results$strike_price[i]
    option_value_result <- europeanCallOptionValue(stock_data, future_time, sell_value, max.p, max.q)
    results$option_value[i] <- option_value_result$option_value
  }

  # Return the results data frame
  return(results)
}

# Example usage

# Load the necessary libraries
library(forecast)
library(stats)

# Example data
stock_data <- c(100, 102, 105, 107, 110, 108, 105, 103, 101, 99, 98, 97)
future_times <- c(1, 3, 5)
strike_prices <- seq(90, 130, by = 5)

# Perform the strike price sensitivity analysis
result <- strikePriceSensitivityAnalysis(stock_data, future_times, strike_prices)

# Print the results
print(result)
