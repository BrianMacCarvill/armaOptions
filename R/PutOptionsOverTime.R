#' Time Sensitivity Analysis for European Call Option
#'
#' This function analyzes the sensitivity of the European call option value to changes in the time to maturity.
#' It calculates the option value for a fixed strike price over a range of future time periods using the `europeanCallOptionValue` function.
#'
#' @param stock_data Numeric vector representing the historical stock prices.
#' @param future_times Numeric vector representing the future time periods (in the same units as stock_data) for which the option value is calculated.
#' @param strike_price Numeric value representing the fixed strike price to analyze.
#' @param max.p The maximum order of the autoregressive part of the ARMA model (default is 5).
#' @param max.q The maximum order of the moving average part of the ARMA model (default is 5).
#'
#' @return A data frame containing the future times and their corresponding option values.
#'
#' @examples
#' \dontrun{
#' library(stats)
#'
#' # Set the model parameters
#' n <- 1000  # Number of observations
#' ar_params <- c(0.5, -0.25)  # AR coefficients
#' ma_params <- c(0.4, 0.3)    # MA coefficients
#' intercept <- 0.5            # Intercept term for the linear component
#'
#' # Generate an ARMA(2,2) series
#' set.seed(1)  # For reproducibility
#' arma_series <- arima.sim(n = n, model = list(ar = ar_params, ma = ma_params), sd = 1)
#'
#' # Add a linear trend
#' time_index <- 1:n
#' linear_component <- intercept + 0.05 * time_index  # Linear trend: intercept + slope * time
#' trended_series <- arma_series + linear_component
#'
#' # Perform the time sensitivity analysis
#' future_times <- c(1, 3, 5)  # Future time periods
#' strike_price <- 50          # Strike price
#'
#' PutOptionsOverTime(stock_data = trended_series, future_times = future_times, strike_price = strike_price)
#' }
#'
#' @importFrom forecast auto.arima forecast
#' @importFrom stats coef lm residuals
#' @export
PutOptionsOverTime <- function(stock_data, future_times, strike_price, max.p = 5, max.q = 5) {
  # Validate inputs
  if (!is.numeric(stock_data) || length(stock_data) < 2) {
    stop("stock_data must be a numeric vector with at least two elements.")
  }

  if (!is.numeric(future_times) || length(future_times) < 1) {
    stop("future_times must be a numeric vector with at least one element.")
  }

  if (!is.numeric(strike_price) || length(strike_price) != 1) {
    stop("strike_price must be a single numeric value.")
  }

  # Initialize a data frame to store results
  results <- data.frame(future_time = future_times, option_value = NA)

  # Loop over each future time period to calculate the option value
  for (i in seq_along(future_times)) {
    future_time <- future_times[i]
    option_value_result <- europeanPutOptionValue(stock_data, future_time, strike_price, max.p, max.q)
    results$option_value[i] <- option_value_result$option_value
  }

  # Return the results data frame
  return(results)
}

