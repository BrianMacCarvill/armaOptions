#' Time Sensitivity Analysis for European Call Option
#'
#' This function analyzes the sensitivity of the European call option value to changes in the time to maturity.
#' It calculates the option value for a fixed strike price over a range of future time periods using the `europeanCallOptionValue` function.
#'
#' @param stock_data Numeric vector of stock prices data.
#' @param future_times Numeric vector of the future times
#' @param strike_price Numeric value representing the fixed strike price to analyze.
#' @param max.p The maximum order of the autoregressive part of the ARMA model (default is 5).
#' @param max.q The maximum order of the moving average part of the ARMA model (default is 5).
#'
#' @return Estimated values of a European put option at different future_times
#'
#' @examples
#' \dontrun{
#' library(stats)
#'
#' # Create simulated data
#' n <- 100
#' set.seed(42)
#' arma_values <- arima.sim(n = n, model = list(ar = c(0.6), ma = c(0.5, -0.5)))
#' linear_model <- 5 + 1:n
#' stock_data <- arma_values + linear_model
#'
#' sell_value <- 110
#' future_times = c(1, 3, 5)
#'
#' PutOptionsOverTime(stock_data = stock_data, future_times = future_times, sell_value = sell_value)
#' }
#'
#' @importFrom forecast auto.arima forecast
#' @importFrom stats coef lm residuals
#' @export
PutOptionsOverTime = function(stock_data, future_times, sell_value, max.p = 5, max.q = 5) {
  # Validate inputs
  if (!is.numeric(stock_data)) {
    stop("stock_data must be a numeric vector with at least two elements.")
  }

  if (!is.numeric(future_times)) {
    stop("future_times must be a numeric vector with at least one element.")
  }

  if (!is.numeric(sell_value)) {
    stop("sell_value must be a single numeric value.")
  }

  n = length(future_times)

  results = data.frame(future_time = future_times, stock_option_value = rep(0,n))

  for (i in seq_along(future_times)) {
    results$stock_option_value[i] = europeanPutOptionValue(stock_data, future_time, sell_value, max.p, max.q)$stock_option_value
  }

  # Return the results
  return(results)
}
