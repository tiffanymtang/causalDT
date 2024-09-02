#' @export
evaluate_subgroup_stability <- function(estimator, fit, X, y, Z = NULL,
                                        rpart_control = NULL,
                                        B = 100,
                                        max_depth = NULL) {

  if (!("rpart" %in% class(fit))) {
    warning(
      "Estimator is not an rpart object. ",
      "Stability diagnostics have only been implemented for the rpart student model. ",
      "Skipping stability diagnostics."
    )
    return(NULL)
  } else if ((B == 0) || is.null(fit)) {
    return(NULL)
  }

  fit_orig <- partykit::as.party(fit)
  node_depths_orig <- get_party_node_depths(fit_orig)
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
        fit_b <- estimator(X = X_b, y = y_b, fit_only = TRUE) |>
          partykit::as.party()
      } else {
        Z_b <- Z[bootstrap_idx]
        fit_b <- estimator(X = X_b, Y = y_b, Z = Z_b) |>
          partykit::as.party()
      }
      node_depths_b <- get_party_node_depths(fit_b)
      return(
        list(
          "fit" = fit_b,
          "node_depths" = node_depths_b
        )
      )
    }
  )

  bootstrap_fits <- purrr::map(bootstrap_out, "fit")
  node_depths <- purrr::map(bootstrap_out, "node_depths")

  Js <- list()
  preds_mean <- list()
  preds_var <- list()
  for (n_depth in 1:max_depth) {
    if (any(node_depths_orig > n_depth)) {
      fit_orig_pruned <- partykit::nodeprune(
        fit_orig, ids = names(node_depths_orig)[node_depths_orig == n_depth]
      )
    } else {
      fit_orig_pruned <- fit_orig
    }
    node_depths_orig_pruned <- get_party_node_depths(fit_orig_pruned)
    leaf_ids_orig <- predict(fit_orig_pruned, data.frame(X), type = "node")

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
      1:B,
      ~ jaccardCPP(
        bootstrap_leaf_ids[[.x * 2 - 1]],
        bootstrap_leaf_ids[[.x * 2]]
      )
    )
    Js[[n_depth]] <- J

    preds_mean[[n_depth]] <- do.call(cbind, bootstrap_leaf_preds) |>
      rowMeans()
    preds_var[[n_depth]] <- do.call(cbind, bootstrap_leaf_preds) |>
      apply(1, var)
  }

  leaf_ids_orig <- predict(fit_orig, data.frame(X), type = "node")
  out <- list(
    "jaccard_mean" = sapply(Js, mean),
    "jaccard_distribution" = Js,
    "bootstrap_predictions_mean" = preds_mean,
    "bootstrap_predictions_var" = preds_var,
    "leaf_ids" = leaf_ids_orig
  )
  return(out)
}



