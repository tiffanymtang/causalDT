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
#' @inheritParams bcf::bcf
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

#' @rdname teacher_models
#' @export
bcf <- function(X, Y, Z, pihat = "default", w = NULL,
                nburn = 2000, nsim = 1000, n_threads = 1, ...) {
  if (identical(pihat, "default")) {
    pihat_fit <- glm(Z ~ X, family = "binomial")
    pihat <- predict(pihat_fit, data.frame(X), type = "response")
  } else if (length(pihat) == 1) {
    pihat <- rep(pihat, nrow(X))
  }
  if (is.null(w)) {
    w <- rep(1, nrow(X))
  }
  dots_ls <- rlang::dots_list(...)
  if ("x_moderate" %in% names(dots_ls)) {
    X_moderate <- dots_ls$x_moderate
  } else {
    X_moderate <- X
  }
  bcf::bcf(
    y = Y, z = Z, x_control = X, x_moderate = X_moderate,
    pihat = pihat, w = w, nburn = nburn, nsim = nsim, n_threads = n_threads,
    ...
  )
}

#' @rdname teacher_models
#' @export
predict_bcf <- function(...) {
  # predict(...)
  object <- rlang::dots_list(...)[[1]]
  colMeans(object$tau)
}
