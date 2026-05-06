#' Teacher models for causal distillation trees
#'
#' @name teacher_models
#'
#' @description
#' These functions are wrappers around various heterogeneous treatment effect
#' learners that can be easily used as
#' teacher models in the causal distillation tree framework.
#' - \code{causal_forest()}: wrapper around \code{grf::causal_forest()}.
#' - \code{bcf()}: wrapper around \code{bcf::bcf()}.
#' - \code{rboost()}: (defunct) wrapper around \code{rlearner::rboost()}.
#' - \code{rlasso()}: (defunct) wrapper around \code{rlearner::rlasso()}.
#' - \code{rkern()}: (defunct) wrapper around \code{rlearner::rkern()}.
#'
#' Warning: The \code{rboost()}, \code{rlasso()}, and \code{rkern()} functions
#'   are defunct as of version 1.0.0. Use \code{rlearner_teacher()} (e.g.,
#'   \code{rlearner_teacher(rlearner::rboost)}) instead to convert
#'   \code{rlearner} functions into correct format for use as teacher model in
#'   CDT.
#'
#' @inheritParams bcf::bcf
#' @inheritParams shared_args
#' @param ... Additional arguments to pass to the base model functions.
#'
#' @returns Outputs of the respective base model functions:
#' - \code{causal_forest()}: see output of \code{grf::causal_forest()}.
#' - \code{rboost()} (defunct): see output of \code{rlearner::rboost()}.
#' - \code{rlasso()} (defunct): see output of \code{rlearner::rlasso()}.
#' - \code{rkern()} (defunct): see output of \code{rlearner::rkern()}.
#'
#' @keywords internal
NULL


#' Predict wrappers for teacher models for causal distillation trees
#'
#' @name predict_teacher_models
#'
#' @description
#' These functions are \code{predict()} method wrappers for various heterogeneous
#' treatment effect learners that can be easily used as
#' teacher models in the causal distillation tree framework.
#' - \code{predict_causal_forest()}: wrapper around \code{predict()} for
#'     \code{causal_forest()} models.
#' - \code{predict_bcf()}: wrapper around \code{predict()} for
#'     \code{bcf()} models.
#'
#' @param ... Additional arguments to pass to the base model \code{predict} functions.
#'
#' @returns Vector of predicted conditional average treatment effects (CATEs).
#'
#' @keywords internal
NULL


#' @rdname teacher_models
#' @export
causal_forest <- function(X, Y, Z, W = NULL, ...) {
  grf::causal_forest(
    X = X, Y = Y, W = Z, W.hat = W, ...
  )
}

#' @rdname predict_teacher_models
#' @export
predict_causal_forest <- function(...) {
  predict(...)$predictions
}

#' Rlearner teacher model wrapper for causal distillation trees
#'
#' @description This is a wrapper function to convert any of the \code{rlearner}
#'   model functions into a format that can be used as teacher model in the
#'   causal distillation tree framework.
#'
#' @param rlearner_fun One of \code{rlearner::rboost},
#'   \code{rlearner::rlasso}, or \code{rlearner::rkern} to be transformed to
#'   teacher model format for CDT.
#' @param ... Additional arguments to pass to the base model functions.
#'
#' @returns Outputs a function that can be used as teacher model in the
#'   causal distillation tree framework. The returned function has the
#'   signature \code{function(X, Y, Z, W = NULL, ...)}.
#'
#' @export
rlearner_teacher <- function(rlearner_fun, ...) {
  teacher_fun <- function(X, Y, Z, W = NULL, ...) {
    rlearner_fun(
      x = X, y = Y, w = Z, p_hat = W, ...
    )
  }
  return(teacher_fun)
}

#' @rdname teacher_models
#' @export
rboost <- function(X, Y, Z, W = NULL, ...) {
  lifecycle::deprecate_stop(
    when = "1.0.0",
    what = "rboost()",
    details = "Use rlearner_teacher(rlearner::rboost) instead."
  )
}

#' @rdname teacher_models
#' @export
rlasso <- function(X, Y, Z, W = NULL, ...) {
  lifecycle::deprecate_stop(
    when = "1.0.0",
    what = "rlasso()",
    details = "Use rlearner_teacher(rlearner::rlasso) instead."
  )
}

#' @rdname teacher_models
#' @export
rkern <- function(X, Y, Z, W = NULL, ...) {
  lifecycle::deprecate_stop(
    when = "1.0.0",
    what = "rkern()",
    details = "Use rlearner_teacher(rlearner::rkern) instead."
  )
}

#' @rdname teacher_models
#' @export
bcf <- function(X, Y, Z, W = NULL, pihat = "default", w = NULL,
                nburn = 2000, nsim = 1000, n_threads = 1, no_output = TRUE,
                ...) {
  if (is.null(W)) {
    if (identical(pihat, "default")) {
      pihat_fit <- glm(Z ~ X, family = "binomial")
      pihat <- predict(pihat_fit, data.frame(X), type = "response")
    } else if (length(pihat) == 1) {
      pihat <- rep(pihat, nrow(X))
    }
  } else {
    pihat <- W
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
  if (no_output) {
    logged_output <- capture.output(
      fit <- bcf::bcf(
        y = Y, z = Z, x_control = X, x_moderate = X_moderate,
        pihat = pihat, w = w, nburn = nburn, nsim = nsim, n_threads = n_threads,
        no_output = no_output, ...
      )
    )
  } else {
    fit <- bcf::bcf(
      y = Y, z = Z, x_control = X, x_moderate = X_moderate,
      pihat = pihat, w = w, nburn = nburn, nsim = nsim, n_threads = n_threads,
      no_output = no_output, ...
    )
  }
  return(fit)
}

#' @rdname predict_teacher_models
#' @export
predict_bcf <- function(...) {
  # predict(...)
  object <- rlang::dots_list(...)[[1]]
  colMeans(object$tau)
}
