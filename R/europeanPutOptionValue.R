#' Calculate European Put Option Value
#'
#' This function calculates the value of a European put option based on historical stock prices, a future time period, and a specified strike price.
#'
#' @param stock_data Numeric vector representing the historical stock prices.
#' @param future_time Future time period (in same units as stock_data) for which the put option value is calculated.
#' @param sell_value The strike price of the European put option.
#' @param p The maximum order of the autoregressive part of the ARMA model.
#' @param q The maximum order of the moving average part of the ARMA model.
#'
#' @return The expected payoff of the European put option.
#'
#' @examples
#' \dontrun{
#' stock_data <- c(100, 102, 105, 107, 110)
#' future_time <- 5
#' sell_value <- 115
#' europeanPutOptionValue(stock_data, future_time, sell_value)
#' }
#'
#' @export
europeanPutOptionValue <- function(stock_data, future_time, sell_value, p = 5, q = 5) {
  # Parameters:
  # stock_data: Numeric vector representing the historical stock prices.
  # future_time: Future time period (in same units as stock_data) for which the put option value is calculated.
  # sell_value: The strike price of the European put option.
  # p: The maximum order of the autoregressive part of the ARMA model.
  # q: The maximum order of the moving average part of the ARMA model.

  # Validate inputs
  if(!is.numeric(stock_data) || length(stock_data) < 2) {
    stop("stock_data must be a numeric vector with at least two elements.")
  }

  if(!is.numeric(future_time) || future_time <= 0) {
    stop("future_time must be a positive numeric value.")
  }

  if(!is.numeric(sell_value)) {
    stop("sell_value must be a numeric value.")
  }

  # Calculate the length of the stock data
  n <- length(stock_data)
  time_index <- 1:n

  # Linear regression to find trends in stock data
  model <- lm(stock_data ~ time_index)

  # Extract residuals for ARMA modeling
  residuals <- residuals(model)

  # Fit ARMA model to the residuals
  arma_model <- auto.arima(residuals, max.p = p, max.q = q, stationary = TRUE, allowmean = FALSE, ic = "aic", method = "ML")
  l_step_prediction = forecast(arma_model, h = future_time)
  # Extract AR and MA coefficients
  coefficients <- coef(arma_model)
  ar_coefficients <- coefficients[grep("^ar", names(coefficients))]
  ma_coefficients <- coefficients[grep("^ma", names(coefficients))]

  # Calculate expected future stock price
  expected_future_value <- as.numeric(coef(model)[1] + coef(model)[2] * (n + future_time))
  print(arma_model)
  # Calculate the target residual value for the specified sell_value
  residual_target <- sell_value - expected_future_value

  # Determine the variance of future residuals
  max_lag <- 1000  # Assuming convergence
  acf_values <- ARMAtoMA(ar = ar_coefficients, ma = ma_coefficients, lag.max = max_lag)
  sigma_future <- sqrt((l_step_prediction$upper[future_time] - l_step_prediction$lower[future_time]) / (2 * 1.96))

  # Estimate the future residual and its conditional expectation
  future_residual <- l_step_prediction$mean[future_time]
  conditional_expectation <- conditional_expectation_normal(future_residual, sigma_future, residual_target)
  print(sigma_future)
  print(residual_target)
  print(future_residual)
  # Probability that the future residual exceeds the target
  probability_exceed <- pnorm(residual_target, mean = future_residual, sd = sigma_future, lower.tail = FALSE)
  print(probability_exceed)
  # Return the expected payoff of the put option
  if (probability_exceed == 0 || conditional_expectation == Inf) {
    return(0)
  } else {
    return((conditional_expectation - residual_target) * probability_exceed)
  }
}
