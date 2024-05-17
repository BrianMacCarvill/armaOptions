#' Track European Call Option Value Differences
#'
#' This function tracks the differences between the estimated European call option values and the actual values over a specified number of iterations.
#'
#' @param closing_prices Numeric vector representing the historical closing prices of the stock.
#' @param steps The number of steps (time periods) to look ahead for the option value calculation.
#' @param iter The total number of iterations to perform.
#' @param p The maximum order of the autoregressive part of the ARMA model (default is 5).
#' @param q The maximum order of the moving average part of the ARMA model (default is 5).
#'
#' @return A list containing:
#' \describe{
#'   \item{tracker}{Numeric vector of estimated European call option values.}
#'   \item{actual_values}{Numeric vector of actual European call option values.}
#'   \item{difference}{Numeric vector of differences between actual and estimated values.}
#' }
#'
#' @examples
#' \dontrun{
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
#' # Use the trackPutOptionValueDifferences function with the generated series
#' steps <- 3  # Number of steps into the future for option value estimation
#' iter <- 50  # Number of iterations to track
#'
#' # Call the function
#' result <- trackPutOptionValueDifferences(trended_series, steps, iter)
#'
#' # Plot the differences between actual and estimated values
#' plot(result$difference, type = "o", col = "blue", main = "Differences between Actual and Estimated Put Option Values",
#'      xlab = "Iteration", ylab = "Difference")
#'
#' }
#'
#' @export
trackCallOptionValueDifferences <- function(closing_prices, steps = 1, iter, p = 5, q = 5) {
  # Validate inputs
  if (length(closing_prices) <= (iter + steps)) {
    stop("`closing_prices` must be longer than `iter + steps`.")
  }
  if (!is.numeric(steps) || steps <= 0 || steps >= iter) {
    stop("`steps` must be a positive integer less than `iter`.")
  }
  if (!is.numeric(iter) || iter <= 0 || iter >= length(closing_prices)) {
    stop("`iter` must be a positive integer less than the length of `closing_prices`.")
  }
  if (!is.numeric(p) || p < 0) {
    stop("`p` must be a non-negative integer.")
  }
  if (!is.numeric(q) || q < 0) {
    stop("`q` must be a non-negative integer.")
  }
  # Initialize vectors to store results
  tracker <- numeric(iter - steps)
  actual_values <- numeric(iter - steps)
  difference <- numeric(iter - steps)

  # Loop over specified iterations
  for (i in steps:iter) {
    # Calculate estimated call option value
    values <- europeanCallOptionValue(
      stock_data = closing_prices[1:(length(closing_prices) - i)],
      future_time = steps,
      sell_value = closing_prices[(length(closing_prices) - i) + steps],
      max.p = p,
      max.q = q
    )$option_value

    # Calculate actual call option value
    actual_value <- max(closing_prices[(length(closing_prices) - i)] - closing_prices[length(closing_prices) - i + steps]
                        , 0)

    # Store results
    actual_values[i - steps] <- actual_value
    tracker[i - steps] <- values
    difference[i - steps] <- actual_value - values

    # Print iteration number
    print(i - steps)
  }

  # Return results as a list
  return(list(tracker = tracker, actual_values = actual_values, difference = difference))
}

