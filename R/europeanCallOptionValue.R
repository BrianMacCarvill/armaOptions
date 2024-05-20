#' Estimate European Call Option Value
#'
#' This function calculates the value of a European call option based on stock data, a future time value, and a buy value
#'
#' @param stock_data Numeric vector of stock prices data.
#' @param future_time Numeric constant of the future time
#' @param buy_value The numeric buy value of the European call option.
#' @param max.p The maximum order of the autoregressive part of the ARMA model.
#' @param max.q The maximum order of the moving average part of the ARMA model.
#' @param method The way that the ARMA model is calculated, accepted values are "ML", "CSS-ML" and "CSS"
#' @return An estimated value of a European call option.
#'
#' @examples
#' \dontrun{
#' # Import Packages
#' library(stats)
#' library(forecast)
#' # Create simulated data
#' n = 100
#' set.seed(42)
#' arma_values = arima.sim(n = n, model = list(ar = c(0.5), ma = c(0.5, -0.5)))
#' linear_model = 5 +  1:n
#' stock_data = arma_values + linear_model
#' europeanCallOptionValue(stock_data = stock_data, future_time = 5, buy_value = 110, max.p = 5, max.q = 5)
#' }
#'
#' @importFrom forecast auto.arima forecast
#' @importFrom stats coef lm residuals
#'
#' @export
europeanCallOptionValue = function(stock_data, future_time, buy_value, max.p = 5, max.q = 5, method = 'CSS-ML') {
  # Parameters:
  # stock_data: Numeric vector representing the historical stock prices.
  # future_time: Future time period (in same units as stock_data) for which the call option value is calculated.
  # buy_value: The strike price of the European call option.
  # max.p The maximum order of the autoregressive part of the ARMA model
  # max.q The maximum order of the moving average part of the ARMA model
  # method The way that the ARMA model is calculated

  # Check input
  if (!is.numeric(stock_data)) {
    stop("stock_data needs to be numeric")
  }

  if (!is.numeric(future_time)) {
    stop("future_time needs to be numeric")
  }

  if (!is.numeric(sell_value)) {
    stop("sell_value needs to be numeric")
  }

  if (!(method %in% c("ML", "CSS-ML", "CSS"))) {
    stop("method must be one of 'ML', 'CSS-ML', or 'CSS'.")
  }

  # Define positive_conditional_expectation function
  positive_conditional_expectation <- function(mu, sigma, S) {
    z <- (S - mu) / sigma

    pdf <- dnorm(z)
    cdf <- pnorm(z)

    # Compute E[X | X > S]
    expectation <- mu + sigma * (pdf / (1 - cdf))

    return(expectation)
  }

  # Get linear model
  n = length(stock_data)
  time_index = 1:n

  # Fit Linear regression
  lm_model = lm(stock_data ~ time_index)

  # Get residuals for ARMA modeling
  residuals = residuals(lm_model)

  # Fit ARMA model to the residuals
  arma_model = auto.arima(residuals, max.p = max.p, max.q = max.q, stationary = TRUE, allowmean = FALSE, ic = "aic", method = method)
  # forecast future ARMA predictions
  future_prediction = forecast(arma_model, h = future_time)

  # Calculate expected future stock price
  expected_future_value = as.numeric(coef(lm_model)[1] + coef(lm_model)[2] * (n + future_time))

  # Calculate the target residual value for the specified buy_value
  profit_threshold = expected_future_value - buy_value

  # Determine the standard deviation of the future residual at future_time
  forecast_sd = sqrt((future_prediction$upper[future_time] - future_prediction$lower[future_time]) / (2 * 1.96))

  # Estimate the future residual
  future_expected_residual = future_prediction$mean[future_time]

  # Calculate the expected value of profit
  conditional_value = positive_conditional_expectation(future_expected_residual, forecast_sd, profit_threshold)

    # Calculate the expected value give the option is profitable
  if (is.na(conditional_value) || conditional_value == Inf) {
    conditional_value = profit_threshold
  }

  # Probability that the future residual doesn't exceeds the target
  probability_not_exceed = pnorm(profit_threshold, mean = future_expected_residual, sd = forecast_sd, lower.tail = TRUE)

  # Return the expected payoff of the put option
  stock_option_value = (conditional_value) * probability_not_exceed

  # Return results
  return(list(
    stock_option_value = stock_option_value,
    probability_of_profit = probability_not_exceed,
    expected_profit_when_profitable = conditional_value,
    arma_coefficients = coef(arma_model),
    Regression_model_coefficients = lm_model$coefficients
  ))
}

