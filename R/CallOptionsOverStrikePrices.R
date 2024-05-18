#' Strike Price Sensitivity Analysis for European Call Option
#'
#' This function analyzes the sensitivity of the European call option value to changes in the strike price for a single future time period.
#' It calculates the option value for a range of strike prices using the `europeanCallOptionValue` function.
#'
#' @param stock_data Numeric vector representing the historical stock prices.
#' @param future_time Numeric value representing the future time period (in the same units as stock_data) for which the option value is calculated.
#' @param strike_prices Numeric vector representing the range of strike prices to analyze.
#' @param max.p The maximum order of the autoregressive part of the ARMA model (default is 5).
#' @param max.q The maximum order of the moving average part of the ARMA model (default is 5).
#'
#' @return A data frame containing the strike prices and their corresponding option values for the given future time period.
#'
#' @examples
#' \dontrun{
#' stock_data <- c(100, 102, 105, 107, 110, 108, 105, 103, 101, 99, 98, 97)
#' future_time <- 3
#' strike_prices <- seq(90, 130, by = 5)
#' result <- CallOptionsOverStrikePrices(stock_data, future_time, strike_prices)
#' print(result)
#' }
#'
#' @import forecast auto.arima forecast
#' @import stats coef lm residuals
#' @export
CallOptionsOverStrikePrices <- function(stock_data, future_time, strike_prices, max.p = 5, max.q = 5) {
  # Validate inputs
  if (!is.numeric(stock_data) || length(stock_data) < 2) {
    stop("stock_data must be a numeric vector with at least two elements.")
  }

  if (!is.numeric(future_time) || length(future_time) != 1) {
    stop("future_time must be a single numeric value.")
  }

  if (!is.numeric(strike_prices) || length(strike_prices) < 1) {
    stop("strike_prices must be a numeric vector with at least one element.")
  }

  # Initialize a data frame to store results
  results <- data.frame(strike_price = strike_prices, option_value = NA)

  # Loop over each strike price to calculate the option value
  for (i in seq_along(strike_prices)) {
    sell_value <- strike_prices[i]
    option_value_result <- europeanCallOptionValue(stock_data, future_time, sell_value, max.p, max.q)
    results$option_value[i] <- option_value_result$stock_option_value
  }

  # Return the results data frame
  return(results)
}

