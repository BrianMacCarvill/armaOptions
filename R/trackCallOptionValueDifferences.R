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
#' closing_prices <- c(100, 102, 105, 107, 110, 108, 105, 103, 101, 99, 98, 97)
#' steps <- 2
#' iter <- 10
#' result <- trackCallOptionValueDifferences(closing_prices, steps, iter)
#' }
#'
#' @export
trackCallOptionValueDifferences <- function(closing_prices, steps, iter, p = 5, q = 5) {
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
    )

    # Calculate actual call option value
    actual_value <- max(-closing_prices[length(closing_prices) - i + steps] + closing_prices[(length(closing_prices) - i)], 0)

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
