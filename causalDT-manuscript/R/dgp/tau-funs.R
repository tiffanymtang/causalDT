generate_tau_simple <- function(X, beta = 1) {
  result <- beta * (X[, 1] > 0)
  thresholds <- list(
    X1 = 0
  )
  return(list(result = result, thresholds = thresholds))
}

generate_tau_or <- function(X) {
  result <- 2 * (X[, 1] > 0) - (X[, 2] > 0.5 | X[, 2] < -0.5)
  thresholds <- list(
    X1 = 0,
    X2 = c(-0.5, 0.5)
  )
  return(list(result = result, thresholds = thresholds))
}

generate_tau_additive <- function(X) {
  result <- 2 * (X[, 1] > 0) - (X[, 2] < -0.5)
  thresholds <- list(
    X1 = 0,
    X2 = -0.5
  )
  return(list(result = result, thresholds = thresholds))
}

generate_tau_and <- function(X) {
  result <- 2 * (X[, 1] > 0) * (X[, 2] > 0.5)
  thresholds <- list(
    X1 = 0,
    X2 = 0.5
  )
  return(list(result = result, thresholds = thresholds))
}

generate_tau_random <- function(X) {
  result <- rbinom(nrow(X), 1, 0.5)
  thresholds <- list()
  return(list(result = result, thresholds = thresholds))
}

generate_tau_null <- function(X) {
  result <- rep(0, nrow(X))
  thresholds <- list()
  return(list(result = result, thresholds = thresholds))
}
