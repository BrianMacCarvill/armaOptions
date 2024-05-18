
# Helper function to calculate the conditional expectation
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
