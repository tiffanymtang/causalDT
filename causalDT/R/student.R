#' @export
student_rpart <- function(X, y, method = "anova", rpart_control = NULL,
                          fit_only = FALSE) {
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


#' @export
estimate_group_cates <- function(fit, X, Y, Z) {
  if (!is.null(fit)) {
    if ("rpart" %in% class(fit)) {
      fit <- partykit::as.party(fit)
    }
    leaf_ids <- predict(fit, data.frame(X), type = 'node')
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
