#' Rpart wrapper for causal distillation trees.
#'
#' @description
#' This function is a wrapper around \code{rpart::rpart()} that can be easily
#' used as a student model in the causal distillation tree framework.
#'
#' @inheritParams shared_args
#' @param method Same as \code{method} argument in \code{rpart::rpart()}.
#'   Default is \code{"anova"}. See \code{rpart::rpart()} for more details.
#' @param fit_only Logical. If \code{TRUE}, only the fitted model is returned.
#'   Default is \code{FALSE}.
#'
#' @returns
#' If \code{fit_only = TRUE}, the fitted model is returned. Otherwise, a list
#' with the following components is returned:
#' \item{fit}{Fitted model. An \code{rpart} model object.}
#' \item{tree_info}{Data frame with tree structure/split information.}
#' \item{subgroups}{List of subgroups given by their string representation.}
#' \item{predictions}{Student model predictions for the given `X` data.}
#'
#' @export
student_rpart <- function(X, y, method = "anova", rpart_control = NULL,
                          prune = c("none", "min", "1se"), fit_only = FALSE) {
  prune <- match.arg(prune)
  df <- data.frame(X, y)

  # if tauhat is constant, return NULL model (no subgroups)
  if (length(unique(y)) == 1) {
    if (fit_only) {
      out <- NULL
    } else {
      out <- list(
        fit = NULL,
        tree_info = NULL,
        subgroups = list(),
        predictions = rep(unique(y), nrow(df))
      )
    }
  } else {
    if (is.null(rpart_control)) {
      fit <- rpart::rpart(
        y ~ ., data = df, method = method
      )
    } else {
      fit <- rpart::rpart(
        y ~ ., data = df, method = method, control = rpart_control
      )
    }

    # pruning
    if (prune != "none") {
      best_cp <- as.data.frame(fit$cptable) |>
        dplyr::filter(xerror == min(xerror, na.rm = TRUE)) |>
        dplyr::slice(1)
      if (prune == "min") {
        fit <- rpart::prune(fit, cp = best_cp$CP)
      } else if (prune == "1se") {
        best1se_cp <- as.data.frame(fit$cptable) |>
          dplyr::filter(xerror <= (best_cp$xerror + best_cp$xstd)) |>
          dplyr::filter(nsplit == min(nsplit, na.rm = TRUE)) |>
          dplyr::slice(1)
        fit <- rpart::prune(fit, cp = best1se_cp$CP)
      }
    }

    if (fit_only) {
      out <- fit
    } else {
      subgroups <- get_rpart_paths(fit)
      tree_info <- get_rpart_tree_info(fit)
      predictions <- predict(fit)
      out <- list(
        fit = fit,
        tree_info = tree_info,
        subgroups = subgroups,
        predictions = predictions
      )
    }
  }
  return(out)
}


#' Subgroup CATE estimation.
#'
#' @description
#' This function estimates the conditional average treatment effect for each
#' subgroup given by the fitted decision tree. The conditional average
#' treatment effect is estimated as the difference in the average outcome
#' between treated and control units that fall within each subgroup (i.e.,
#' each leaf node in the decision tree).
#'
#' @inheritParams shared_args
#' @param fit Fitted subgroup model used to determine subgroup membership of
#'   individuals. Typically, this is a `party` or `rpart` object, but any model
#'   object that can be used to determine subgroup membership via
#'   `predict(fit, x, type = 'node')` can be used.
#'
#' @returns
#' Estimated subgroup average treatment effects tibble with the following columns:
#' \item{leaf_id}{Leaf node identifier.}
#' \item{subgroup}{String representation of the subgroup.}
#' \item{estimate}{Estimated conditional average treatment effect for the subgroup.}
#' \item{variance}{Asymptotic variance of the estimated conditional average treatment effect.}
#' \item{.var1}{Sample variance for treated observations in the subgroup.}
#' \item{.var0}{Sample variance for control observations in the subgroup.}
#' \item{.n1}{Number of treated observations in the subgroup.}
#' \item{.n0}{Number of control observations in the subgroup.}
#' \item{.sample_idxs}{Indices of (holdout) observations in the subgroup.}
#'
#' @examples
#' n <- 100
#' p <- 5
#' X <- matrix(rnorm(n * p), nrow = n, ncol = p)
#' Z <- rbinom(n, 1, 0.5)
#' Y <- 2 * Z * (X[, 1] > 0) + X[, 2] + rnorm(n, 0.1)
#'
#' # causal distillation tree output
#' out <- causalDT(X, Y, Z)
#' # compute subgroup CATEs manually
#' group_cates <- estimate_group_cates(
#'   out$student_fit$fit,
#'   X = X[out$holdout_idxs, , drop = FALSE],
#'   Y = Y[out$holdout_idxs],
#'   Z = Z[out$holdout_idxs]
#' )
#' all.equal(out$estimate, group_cates)
#'
#' @export
estimate_group_cates <- function(fit, X, Y, Z) {
  if (!is.null(fit)) {
    if ("rpart" %in% class(fit)) {
      fit <- partykit::as.party(fit)
    }
    if ("H2ORegressionModel" %in% class(fit)) {
      leaf_ids <- predict(fit, h2o::as.h2o(data.frame(X)), type = 'node') |>
        as.vector()
    } else {
      leaf_ids <- predict(fit, data.frame(X), type = 'node')
    }
  } else {
    leaf_ids <- NULL
  }
  group_cates <- tibble::tibble(
    Z = Z,
    Y = Y,
    leaf_id = leaf_ids
  ) |>
    dplyr::group_by(dplyr::across(tidyselect::any_of("leaf_id"))) |>
    dplyr::summarise(
      estimate = mean(Y[Z == 1]) - mean(Y[Z == 0]),
      variance = 1 / sum(Z == 1) * var(Y[Z == 1]) +
        1 / sum(Z == 0) * var(Y[Z == 0]),
      .var1 = var(Y[Z == 1]),
      .var0 = var(Y[Z == 0]),
      .n1 = sum(Z == 1),
      .n0 = sum(Z == 0),
      .sample_idxs = list(dplyr::cur_group_rows()),
      .groups = "drop"
    )
  if ("party" %in% class(fit)) {
    group_cates <- dplyr::left_join(
      get_party_paths(fit),
      group_cates,
      by = "leaf_id"
    )
  }
  return(group_cates)
}
