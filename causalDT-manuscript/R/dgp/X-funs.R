#' @title Generate a random sample from a multivariate Gaussian distribution
#' @description This function generates a random sample from a multivariate
#'  Gaussian distribution with mean \code{mean} and covariance matrix
#'  \code{Sigma}. Alternatively, can specify constant correlation \code{corr}
#'  and standard deviation \code{sd} for all variables or a subset thereof via
#'  \code{corr_idxs}.
#'
#' @param n The number of observations to generate.
#' @param p The number of variables to generate.
#' @param mean The mean vector of the Gaussian distribution. If a scalar, then
#'   the same mean is used for all variables. If a vector, then the length must
#'   be equal to \code{p}.
#' @param sd The standard deviation of the Gaussian distribution.
#' @param corr The constant correlation between all variables. Used if
#'   \code{Sigma} is not specified.
#' @param corr_idxs A callable or vector of indices of variables to have the
#'   same correlation. All other variables have no correlation. If \code{NULL},
#'   specified correlation applies to all variables. Only used if \code{Sigma}
#'   is not specified.
#' @param Sigma The covariance matrix of the Gaussian distribution. If
#'   specified, then \code{corr} is ignored.
generate_gaussian_X <- function(n, p, mean = 0, sd = 1, corr = 0,
                                corr_idxs = NULL, Sigma = NULL) {
  if (length(mean) == 1) {
    mean_vec <- rep(mean, p)
  } else if (length(mean) != p) {
    stop("The argument mean must be a scalar or a vector of length .p.",
         call. = FALSE)
  } else {
    mean_vec <- mean
  }

  if (is.null(Sigma)) {
    if ((corr == 0) && (length(mean) == 1)) {
      X <- matrix(stats::rnorm(n * p, mean = mean, sd = sd), nrow = n, ncol = p)
    } else {
      if (is.null(corr_idxs)) {
        corr_idxs <- 1:p
      } else if (is.function(corr_idxs)) {
        corr_idxs <- corr_idxs(p)
      }
      nocorr_idxs <- !(1:p %in% corr_idxs)
      Sigma <- matrix(corr, nrow = p, ncol = p)
      Sigma[nocorr_idxs, ] <- 0
      Sigma[, nocorr_idxs] <- 0
      diag(Sigma) <- 1
      D <- diag(sd, nrow = p, ncol = p)
      X <- MASS::mvrnorm(n = n, mu = mean_vec, Sigma = D %*% Sigma %*% D)
    }
  } else {
    X <- MASS::mvrnorm(n = n, mu = mean_vec, Sigma = Sigma)
  }
  colnames(X) <- paste0("X", 1:ncol(X))
  return(X)
}
