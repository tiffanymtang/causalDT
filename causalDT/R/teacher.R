#' Teacher models for causal distillation trees
#'
#' @name teacher_models
#'
#' @description
#' These functions are wrappers around various heterogeneous treatment effect
#' learners (and their associated `predict` methods) that can be easily used as
#' teacher models in the causal distillation tree framework.
#' - \code{causal_forest()}: wrapper around \code{grf::causal_forest()}
#' - \code{predict_causal_forest()}: wrapper around \code{predict()} for
#'     \code{causal_forest()} models.
#' - \code{rboost()}: wrapper around \code{rlearner::rboost()}
#' - \code{rlasso()}: wrapper around \code{rlearner::rlasso()}
#' - \code{rkern()}: wrapper around \code{rlearner::rkern()}
#'
#' @inheritParams shared_args
#' @param ... Additional arguments to pass to the base model functions.
#'
#' @keywords internal
NULL

#' @rdname teacher_models
#' @export
causal_forest <- function(X, Y, Z, ...) {
  grf::causal_forest(
    X = X, Y = Y, W = Z, ...
  )
}

#' @rdname teacher_models
#' @export
predict_causal_forest <- function(...) {
  predict(...)$predictions
}

#' @rdname teacher_models
#' @export
rboost <- function(X, Y, Z, ...) {
  rlearner::rboost(
    x = X, y = Y, w = Z, ...
  )
}

#' @rdname teacher_models
#' @export
rlasso <- function(X, Y, Z, ...) {
  rlearner::rlasso(
    x = X, y = Y, w = Z, ...
  )
}

#' @rdname teacher_models
#' @export
rkern <- function(X, Y, Z, ...) {
  rlearner::rkern(
    x = X, y = Y, w = Z, ...
  )
}
