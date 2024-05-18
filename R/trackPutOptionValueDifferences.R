#' Track European Put Option Value Differences
#'
#' This function tracks the differences between the estimated European put option values and the actual values over a specified number of iterations.
#'
#' @param stock_data Numeric vector representing the historical closing prices of the stock.
#' @param steps Integer. The number of steps (time periods) to look ahead for the option value calculation.
#' @param iter Integer. The total number of iterations to perform.
#' @param p Integer. The maximum order of the autoregressive part of the ARMA model (default is 5).
#' @param q Integer. The maximum order of the moving average part of the ARMA model (default is 5).
#'
#' @return A list containing:
#' \describe{
#'   \item{tracker}{Numeric vector of estimated European put option values.}
#'   \item{actual_values}{Numeric vector of actual European put option values.}
#'   \item{difference}{Numeric vector of differences between actual and estimated values.}
#' }
#'
#' @examples
#' \dontrun{
#'
#' library(stats)
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
#' linear_component <- intercept + 0.05 * time_index  # Linear trend: intercept + slope with respect to time
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
#' }
#'
#' @export
trackPutOptionValueDifferences <- function(stock_data, steps = 1, iter = 10, p = 5, q = 5) {
  # Validate inputs
  if (length(stock_data) <= (iter + steps)) {
    stop("`stock_data` must be longer than `iter + steps`.")
  }
  if (!is.numeric(steps) || steps <= 0 || steps >= iter) {
    stop("`steps` must be a positive integer less than `iter`.")
  }
  if (!is.numeric(iter) || iter <= 0 || iter >= length(stock_data)) {
    stop("`iter` must be a positive integer less than the length of `stock_data`.")
  }
  if (!is.numeric(p) || p < 0) {
    stop("`p` must be a non-negative integer.")
  }
  if (!is.numeric(q) || q < 0) {
    stop("`q` must be a non-negative integer.")
  }

  # Initialize vectors to store results
  tracker <- numeric(iter - steps + 1)
  actual_values <- numeric(iter - steps + 1)
  difference <- numeric(iter - steps + 1)

  # Loop over specified iterations
  for (i in steps:iter) {
    # Calculate estimated put option value
    values <- europeanPutOptionValue(
      stock_data = stock_data[1:(length(stock_data) - i)],
      future_time = steps,
      sell_value = stock_data[(length(stock_data) - i) + steps],
      p = p,
      q = q
    )$option_value

    # Calculate actual put option value
    actual_value <- max(stock_data[length(stock_data) - i + steps] - stock_data[length(stock_data) - i], 0)

    # Store results
    actual_values[i - steps + 1] <- actual_value
    tracker[i - steps + 1] <- values
    difference[i - steps + 1] <- actual_value - values

    # Print iteration number
    print(values)
  }

  # Return results as a list
  return(list(tracker = tracker, actual_values = actual_values, difference = difference))
}
