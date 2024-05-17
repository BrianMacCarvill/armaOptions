#' Calculate Conditional Expectation for Normal Distribution
#'
#' This function calculates the conditional expectation \eqn{E[X | X > S]} for a normal distribution
#' with mean \eqn{\mu} and standard deviation \eqn{\sigma}, given a threshold \eqn{S}.
#'
#' @param mu Mean of the normal distribution.
#' @param sigma Standard deviation of the normal distribution.
#' @param S Threshold value.
#'
#' @return The conditional expectation \eqn{E[X | X > S]}.
#'
#' @examples
#' conditional_expectation_normal(0, 1, 1.5)
#' conditional_expectation_normal(10, 2, 12)
#'
#' @export
conditional_expectation_normal <- function(mu, sigma, S) {
  # Calculate z-value
  z <- (S - mu) / sigma

  # Calculate the PDF and CDF values at z
  pdf_value <- dnorm(z)
  cdf_value <- pnorm(z)

  # Compute the conditional expectation E[X | X > S]
  expectation <- mu + sigma * (pdf_value / (1 - cdf_value))

  return(expectation)
}
