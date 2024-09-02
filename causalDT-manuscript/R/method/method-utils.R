dummy_code <- function(data, fullRank = FALSE) {
  if (is.matrix(data)) {
    return(data)
  }
  data <- data |>
    droplevels() |>
    drop_constant_columns()
  nonnumeric_cols <- sapply(data, class) %in% c("factor", "character")
  if (any(nonnumeric_cols)) {
    nonnumeric_colnames <- colnames(data)[nonnumeric_cols]
    is_binary_col <- data |>
      dplyr::select(tidyselect::all_of(nonnumeric_colnames)) |>
      purrr::map_lgl(~ length(unique(.x)) == 2)
    binary_colnames <- names(is_binary_col)[is_binary_col]

    require(caret)
    dummy_fit <- caret::dummyVars(
      ~ ., data = data, sep = "..", fullRank = fullRank
    )
    data <- predict(dummy_fit, data)

    # remove columns so that binary factors only get one column instead of two
    if ((length(binary_colnames) > 0) && !fullRank) {
      rm_cols <- c()
      for (binary_colname in binary_colnames) {
        rm_cols[binary_colname] <- which(
          stringr::str_starts(colnames(data), paste0(binary_colname, ".."))
        )[1]
      }
      data <- data[, -rm_cols]
    }
  }
  return(data)
}


causaltree_wrapper <- function(X, Y, Z,
                               rpart_control = NULL,
                               causaltree_args = NULL) {
  df <- data.frame(X, Y)
  if (is.null(rpart_control)) {
    logged_output <- capture.output(
      fit <- do.call(
        causalTree::causalTree,
        args = c(
          list(formula = Y ~ ., data = df, treatment = Z),
          causaltree_args
        )
      )
    )
  } else {
    logged_output <- capture.output(
      fit <- do.call(
        causalTree::causalTree,
        args = c(
          list(formula = Y ~ ., data = df, treatment = Z),
          causaltree_args,
          list(control = rpart_control)
        )
      )
    )
  }
  return(fit)
}


get_interaction_formula <- function(data, max_int) {
  model_terms <- setdiff(colnames(data), "Y")
  for (int_order in 1:max_int) {
    model_terms <- c(
      model_terms,
      setdiff(colnames(data), c("Z", "Y")) |>
        combn(m = int_order, simplify = FALSE) |>
        purrr::map_chr(~ paste0("Z:", paste0(.x, collapse = ":")))
    )
  }
  formula <- sprintf("Y ~ %s", paste0(model_terms, collapse = " + "))  |>
    as.formula()
  return(formula)
}


tidy_lm <- function(fit) {
  tidy_df <- broom::tidy(fit) |>
    dplyr::filter(p.value < 0.05)
  return(tidy_df)
}


tidy_glmnet <- function(fit) {
  best_lambda_idx <- which(fit$lambda == fit$lambda.1se)
  tidy_df <- broom::tidy(fit$glmnet.fit) |>
    dplyr::filter(step == !!best_lambda_idx)
  return(tidy_df)
}


get_lm_info <- function(tidy_fit) {
  model_info <- tidy_fit |>
    dplyr::filter(
      stringr::str_detect(term, "Z:") | stringr::str_detect(term, ":Z")
    ) |>
    dplyr::mutate(var = stringr::str_split(term, ":")) |>
    tidyr::unnest_longer(var) |>
    dplyr::filter(var != "Z") |>
    dplyr::mutate(thr = NA_real_)
  return(model_info)
}


get_lm_subgroups <- function(tidy_fit) {
  subgroups <- tidy_fit |>
    dplyr::filter(
      stringr::str_detect(term, "Z:") | stringr::str_detect(term, ":Z")
    ) |>
    dplyr::mutate(var = stringr::str_split(term, ":")) |>
    dplyr::pull(var) |>
    purrr::map(~ setdiff(.x, "Z")) |>
    purrr::compact()
  return(subgroups)
}
