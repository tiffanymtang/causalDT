#' @export
causal_forest <- function(X, Y, Z, ...) {
  grf::causal_forest(
    X = X, Y = Y, W = Z, ...
  )
}

#' @export
predict_causal_forest <- function(...) {
  predict(...)$predictions
}

#' @export
rboost <- function(X, Y, Z, ...) {
  rlearner::rboost(
    x = X, y = Y, w = Z, ...
  )
}

#' @export
rlasso <- function(X, Y, Z, ...) {
  rlearner::rlasso(
    x = X, y = Y, w = Z, ...
  )
}

#' @export
rkern <- function(X, Y, Z, ...) {
  rlearner::rkern(
    x = X, y = Y, w = Z, ...
  )
}
