# Helper function to calculate the conditional expectation
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
