#' Calculate Conditional Expectation for Normal Distribution Below a Threshold
#'
#' This function calculates the conditional expectation \eqn{E[X | X < S]} for a normal distribution
#' with mean \eqn{\mu} and standard deviation \eqn{\sigma}, given a threshold \eqn{S}.
#'
#' @param mu Mean of the normal distribution.
#' @param sigma Standard deviation of the normal distribution.
#' @param S Threshold value.
#'
#' @return The conditional expectation \eqn{E[X | X < S]}.
#'
#' @examples
#' conditional_expectation_normal_negation(0, 1, 1.5)
#' conditional_expectation_normal_negation(10, 2, 12)
#'
#' @export
conditional_expectation_normal_negation <- function(mu, sigma, S) {
  # Standardize the threshold S
  z <- (S - mu) / sigma

  # Calculate the CDF (Phi) and PDF (phi) for the standardized value z
  Phi <- pnorm(z)
  phi <- dnorm(z)

  # Calculate E[X|X<S] using the formula for truncated normal expectation below a threshold
  E_X_given_X_less_S <- mu - sigma * (phi / Phi)

  return(E_X_given_X_less_S)
}
