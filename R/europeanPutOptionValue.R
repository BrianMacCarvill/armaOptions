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
#' @return An estimate of the value of a European put option.
#'
#' @examples
#' \dontrun{
#' library(stats)
#' library(forecast)
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
#' @importFrom function stats coef lm residuals auto.arima
#'
#' @export
europeanPutOptionValue <- function(stock_data, future_time, sell_value, max.p = 5, max.q = 5) {
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
  arma_model <- auto.arima(residuals, max.p = max.p, max.q = max.p, stationary = TRUE, allowmean = FALSE, ic = "aic", method = "ML")
  l_step_prediction = forecast(arma_model, h = future_time)
  # Extract AR and MA coefficients
  coefficients <- coef(arma_model)
  ar_consonants <- coefficients[grep("^ar", names(coefficients))]
  ma_consonants <- coefficients[grep("^ma", names(coefficients))]

  # Calculate expected future stock price
  expected_future_value <- as.numeric(coef(model)[1] + coef(model)[2] * (n + future_time))
  # Calculate the target residual value for the specified sell_value
  residual_target <- sell_value - expected_future_value

  # Determine the variance of future residuals
  sigma_future <- sqrt((l_step_prediction$upper[future_time] - l_step_prediction$lower[future_time]) / (2 * 1.96))

  # Estimate the future residual and its conditional expectation
  future_residual <- l_step_prediction$mean[future_time]

  conditional_expectation <- conditional_expectation_normal(future_residual, sigma_future, residual_target)

  # Probability that the future residual exceeds the target
  probability_to_exceed <- pnorm(residual_target, mean = future_residual, sd = sigma_future, lower.tail = FALSE)


  # Return the expected payoff of the put option
  if (probability_to_exceed == 0 || conditional_expectation == Inf) {
    stock_option_value = 0
  } else {
    stock_option_value = (conditional_expectation - residual_target) * probability_to_exceed
  }
  # Return the detailed results
  return(list(
    stock_option_value = stock_option_value,
    probability_of_profit = probability_to_exceed,
    ar_consonants = ar_consonants,
    ma_consonants = ma_consonants,
    Regression_model_coefficients = model$coefficients
  ))
}



