#' Subgroup stability diagnostics
#'
#' @description TODO
#'
#' @inheritParams shared_args
#'
#' @param estimator Function used to estimate subgroups of individuals and their
#'   corresponding estimated treatment effects. The function should take in
#'   `X`, `y`, and optionally `Z` (if input is not \code{NULL}) and return a
#'   model fit (e.g,. output of `rpart`) that can be coerced into a `party`
#'   object via \code{partykit::as_party()}. Typically, \code{student_rpart}
#'   will be used as the `estimator`.
#' @param fit Fitted subgroup model (often, the output of `estimator()`). Mainly
#'   used to determine an appropriate `max_depth` for the stability diagnostics.
#'   If `fit` is not an `rpart` object, stability diagnostics will be skipped.
#' @param B Number of bootstrap samples to use in evaluating stability
#'   diagnostics. Default is 100.
#' @param max_depth Maximum depth of the tree to consider when evaluating
#'   stability diagnostics. If \code{NULL}, the default is
#'   max(4, max depth of `fit`).
#'
#' @returns A list with the following elements:
#' \item{jaccard_mean}{Vector of mean Jaccard similarity index for each tree depth. The tree depth is given by the vector index.}
#' \item{jaccard_distribution}{List of Jaccard similarity indices across all bootstraps for each tree depth.}
#' \item{bootstrap_predictions}{List of mean student model predictions (for training (non-holdout) data) across all bootstraps for each tree depth.}
#' \item{bootstrap_predictions_var}{List of variance of student model predictions (for training (non-holdout) data) across all bootstraps for each tree depth.}
#' \item{leaf_ids}{List of leaf node identifiers, indicating the leaf membership of each training sample in the (original) fitted student model.}
#'
#' @examples
#' n <- 200
#' p <- 10
#' X <- matrix(rnorm(n * p), nrow = n, ncol = p)
#' Z <- rbinom(n, 1, 0.5)
#' Y <- 2 * Z * (X[, 1] > 0) + X[, 2] + rnorm(n, 0.1)
#'
#' # run causal distillation trees without stability diagnostics
#' out <- causalDT(X, Y, Z, B_stability = 0)
#' # run stability diagnostics manually
#' stability_out <- evaluate_subgroup_stability(
#'   estimator = student_rpart,
#'   fit = out$student_fit$fit,
#'   X = X[-out$holdout_idxs, , drop = FALSE],
#'   y = out$student_fit$predictions
#' )
#'
#' @export
evaluate_subgroup_stability <- function(estimator, fit, X, y, Z = NULL,
                                        rpart_control = NULL,
                                        B = 100,
                                        max_depth = NULL) {

  if (!("rpart" %in% class(fit))) {
    warning(
      "fit is not an rpart object. ",
      "Stability diagnostics have only been implemented for the rpart student model. ",
      "Skipping stability diagnostics."
    )
    return(NULL)
  } else if ((B == 0) || is.null(fit)) {
    return(NULL)
  }

  fit_orig <- partykit::as.party(fit)
  node_depths_orig <- get_party_node_depths(fit_orig)
  leaf_ids_orig <- predict(fit_orig, data.frame(X), type = "node")
  if (is.null(max_depth)) {
    max_depth <- max(max(node_depths_orig), 4)
  }

  # modify rpart controls so that the tree is forced to make a split when possible
  rpart_control[["minsplit"]] <-  2
  rpart_control[["minbucket"]] <- 1
  rpart_control[["cp"]] <- 0
  rpart_control[["maxdepth"]] <- max_depth
  estimator <- purrr::partial(estimator, rpart_control = rpart_control)

  bootstrap_out <- purrr::map(
    1:(2 * B),
    function(b) {
      bootstrap_idx <- sample(1:nrow(X), size = nrow(X), replace = TRUE)
      X_b <- X[bootstrap_idx, , drop = FALSE]
      y_b <- y[bootstrap_idx]
      if (is.null(Z)) {
        fit_b <- estimator(X = X_b, y = y_b, fit_only = TRUE)
      } else {
        Z_b <- Z[bootstrap_idx]
        fit_b <- estimator(X = X_b, Y = y_b, Z = Z_b)
      }
      if (!is.null(fit_b)) {
        fit_b <- partykit::as.party(fit_b)
        node_depths_b <- get_party_node_depths(fit_b)
        return(
          list(
            "fit" = fit_b,
            "node_depths" = node_depths_b
          )
        )
      } else {
        return(NULL)
      }
    }
  ) |>
    purrr::compact()

  bootstrap_fits <- purrr::map(bootstrap_out, "fit")
  node_depths <- purrr::map(bootstrap_out, "node_depths")

  Js <- list()
  preds_mean <- list()
  preds_var <- list()
  for (n_depth in 1:max_depth) {
    # if (any(node_depths_orig > n_depth)) {
    #   fit_orig_pruned <- partykit::nodeprune(
    #     fit_orig, ids = names(node_depths_orig)[node_depths_orig == n_depth]
    #   )
    # } else {
    #   fit_orig_pruned <- fit_orig
    # }
    # node_depths_orig_pruned <- get_party_node_depths(fit_orig_pruned)
    # leaf_ids_orig <- predict(fit_orig_pruned, data.frame(X), type = "node")

    bootstrap_leaf_ids <- purrr::map2(
      bootstrap_fits, node_depths,
      function(fit_b, node_depths_b) {
        if (any(node_depths_b > n_depth)) {
          fit_b_pruned <- partykit::nodeprune(
            fit_b, ids = names(node_depths_b)[node_depths_b == n_depth]
          )
        } else {
          fit_b_pruned <- fit_b
        }
        leaf_ids_b <- predict(fit_b_pruned, data.frame(X), type = "node")
        return(leaf_ids_b)
      }
    )

    bootstrap_leaf_preds <- purrr::map2(
      bootstrap_fits, node_depths,
      function(fit_b, node_depths_b) {
        if (any(node_depths_b > n_depth)) {
          fit_b_pruned <- partykit::nodeprune(
            fit_b, ids = names(node_depths_b)[node_depths_b == n_depth]
          )
        } else {
          fit_b_pruned <- fit_b
        }
        leaf_preds <- predict(fit_b_pruned, data.frame(X))
        return(leaf_preds)
      }
    )

    J <- purrr::map_dbl(
      1:floor(length(bootstrap_fits) / 2),
      ~ jaccardSSI(
        as.numeric(as.factor(bootstrap_leaf_ids[[.x * 2 - 1]])) - 1,
        as.numeric(as.factor(bootstrap_leaf_ids[[.x * 2]])) - 1
      )
    )
    Js[[n_depth]] <- J

    preds_mean[[n_depth]] <- do.call(cbind, bootstrap_leaf_preds) |>
      rowMeans()
    preds_var[[n_depth]] <- do.call(cbind, bootstrap_leaf_preds) |>
      apply(1, var)
  }

  feature_dist <- purrr::map(
    bootstrap_fits, ~ get_party_node_depths(.x, return_features = TRUE)
  ) |>
    dplyr::bind_rows(.id = "bootstrap_idx") |>
    dplyr::filter(
      !is.na(feature)
    ) |>
    dplyr::group_by(depth, feature) |>
    dplyr::summarise(
      freq = dplyr::n()
    ) |>
    dplyr::ungroup()

  out <- list(
    "jaccard_mean" = sapply(Js, mean),
    "jaccard_distribution" = Js,
    "feature_distribution" = feature_dist,
    "bootstrap_predictions_mean" = preds_mean,
    "bootstrap_predictions_var" = preds_var,
    "leaf_ids" = leaf_ids_orig
  )
  return(out)
}



