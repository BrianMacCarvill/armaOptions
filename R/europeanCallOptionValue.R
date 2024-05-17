#' Calculate European Call Option Value
#'
#' This function calculates the value of a European call option based on historical stock prices, a future time period, and a specified strike price.
#'
#' @param stock_data Numeric vector representing the historical stock prices.
#' @param future_time Future time period (in same units as stock_data) for which the put option value is calculated.
#' @param sell_value The strike price of the European call option.
#' @param max.p The maximum order of the autoregressive part of the ARMA model.
#' @param max.q The maximum order of the moving average part of the ARMA model.
#'
#' @return An estimate of the value of a European call option.
#'
#' @examples
#' \dontrun{
#' library(forecast)
#' library(stats)
#' # Set the model parameters
#' n <- 1000  # Number of observations
#' ar_params <- c(0.5, -0.25)  # AR coefficients
#' ma_params <- c(0.4, 0.3)    # MA coefficients
#' intercept <- 0.5            # Intercept term for the linear component
#' # Generate an ARMA(2,2) series
#' set.seed(1)  # For reproducibility
#' arma_series <- arima.sim(n = n, model = list(ar = ar_params, ma = ma_params), sd = 1)
#'
#' # Add a linear trend
#' time_index <- 1:n
#' linear_component <- intercept + 0.05 * time_index  # Linear trend: intercept + slope * time
#' trended_series <- arma_series + linear_component
#' europeanCallOptionValue(stock_data = trended_series, future_time = 3, sell_value = 50, max.p = 5, max.q = 5)
#' }
#'
#' @importFrom forecast auto.arima
#' @importFrom stats coef lm residuals
#'
#' @export
europeanCallOptionValue <- function(stock_data, future_time, sell_value, max.p = 5, max.q = 5, method = 'CSS-ML') {
  # Parameters:
  # stock_data: Numeric vector representing the historical stock prices.
  # future_time: Future time period (in same units as stock_data) for which the put option value is calculated.
  # sell_value: The strike price of the European put option.
  # p: The maximum order of the auto regressive part of the ARMA model.
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

  if (!is.character(method) || !(method %in% c("ML", "CSS-ML", "CSS"))) {
    stop("method must be one of 'ML', 'CSS-ML', or 'CSS'.")
  }

  # Calculate the length of the stock data
  n <- length(stock_data)
  time_index <- 1:n

  # Linear regression to find trends in stock data
  model <- lm(stock_data ~ time_index)

  # Extract residuals for ARMA modeling
  residuals <- residuals(model)

  # Fit ARMA model to the residuals
  arma_model <- auto.arima(residuals, max.p = max.p, max.q = max.q, stationary = TRUE, allowmean = FALSE, ic = "aic", method = "ML")
  l_step_prediction = forecast(arma_model, h = future_time)
  # Extract AR and MA coefficients
  coefficients <- coef(arma_model)
  ar_coefficients <- coefficients[grep("^ar", names(coefficients))]
  ma_coefficients <- coefficients[grep("^ma", names(coefficients))]

  # Calculate expected future stock price
  expected_future_value <- as.numeric(coef(model)[1] + coef(model)[2] * (n + future_time))

  # Calculate the target residual value for the specified sell_value
  residual_target <- sell_value - expected_future_value

  # Determine the variance of future residuals
  max_lag <- 1000  # Assuming convergence
  acf_values <- ARMAtoMA(ar = ar_coefficients, ma = ma_coefficients, lag.max = max_lag)
  sigma_future <- sqrt((l_step_prediction$upper[future_time] - l_step_prediction$lower[future_time]) / (2 * 1.96))

  # Estimate the future residual and its conditional expectation
  future_residual <- l_step_prediction$mean[future_time]
  conditional_expectation <- conditional_expectation_normal_negation(future_residual, sigma_future, residual_target)

  # Probability that the future residual exceeds the target
  probability_exceed <- pnorm(residual_target, mean = future_residual, sd = sigma_future, lower.tail = TRUE)
  # Return the expected payoff of the put option
  if (probability_exceed == 0 || conditional_expectation == Inf) {
    option_value = 0
  } else {
    option_value = (residual_target - conditional_expectation) * probability_exceed
  }
  # Create a list of detailed results
  listing <- list(
    option_value = option_value,
    arma_model = arma_model,
    expected_future_value = expected_future_value,
    probability_exceed = probability_exceed,
    conditional_expectation = conditional_expectation
  )

  # Return the detailed results
  return(listing)
}


