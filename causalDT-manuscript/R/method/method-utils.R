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


prune_tree <- function(fit, prune = c("none", "min", "1se")) {
  prune <- match.arg(prune)
  if (is.null(fit)) {
    return(NULL)
  } else if (prune != "none") {
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

  subgroups <- causalDT::get_rpart_paths(fit)
  tree_info <- causalDT::get_rpart_tree_info(fit)
  predictions <- predict(fit)
  out <- tibble::tibble(
    fit = list(fit),
    model_info = list(tree_info),
    subgroups = list(subgroups),
    student_predictions = list(predictions)
  )
  return(out)
}


unnest_fit_results <- function(fit_results) {
  is_nested <- sapply(
    fit_results$fit,
    function(x) any(c("none", "min", "1se") %in% names(x))
  )
  if (!any(is_nested)) {
    return(fit_results)
  }
  nontree_fit_results <- fit_results |>
    dplyr::filter(
      !stringr::str_detect(.method_name, "Distilled") &
        !stringr::str_detect(.method_name, "Causal Tree")
    )
  tree_fit_results <- fit_results |>
    dplyr::filter(
      stringr::str_detect(.method_name, "Distilled") |
        stringr::str_detect(.method_name, "Causal Tree")
    ) |>
    dplyr::mutate(
      fit = purrr::map(fit, ~ list(none = .x$none, min = .x$min)),
      model_info = purrr::map(model_info, ~ list(none = .x$none, min = .x$min)),
      subgroups = purrr::map(subgroups, ~ list(none = .x$none, min = .x$min)),
      group_cates = purrr::map(group_cates, ~ list(none = .x$none, min = .x$min)),
      teacher_predictions = purrr::map2(
        teacher_predictions, .method_name,
        ~ if (stringr::str_detect(.y, "Causal Tree")) {
          list("none" = .x$none, min = .x$min)
        } else {
          list("none" = .x, min = .x)
        }
      ),
      student_predictions = purrr::map2(
        student_predictions, .method_name,
        ~ if (stringr::str_detect(.y, "Causal Tree")) {
          list("none" = .x, min = .x)
        } else {
          list("none" = .x$none, min = .x$min)
        }
      )
    ) |>
    tidyr::unnest_longer(
      c(
        fit, model_info, subgroups, group_cates,
        teacher_predictions, student_predictions
      )
    ) |>
    dplyr::mutate(
      .method_name = dplyr::case_when(
        fit_id == "none" ~ sprintf("%s (unpruned)", .method_name),
        TRUE ~ .method_name
      )
    )
  if (!isTRUE(all.equal(tree_fit_results$fit_id, tree_fit_results$model_info_id)) ||
      !isTRUE(all.equal(tree_fit_results$fit_id, tree_fit_results$subgroups_id)) ||
      !isTRUE(all.equal(tree_fit_results$fit_id, tree_fit_results$group_cates_id)) ||
      !isTRUE(all.equal(tree_fit_results$fit_id, tree_fit_results$teacher_predictions_id)) ||
      !isTRUE(all.equal(tree_fit_results$fit_id, tree_fit_results$student_predictions_id))) {
    stop("Mismatch between fit_id, model_info_id, subgroups_id, group_cates_id, teacher_predictions_id, and student_predictions_id.")
  }

  fit_results <- dplyr::bind_rows(tree_fit_results, nontree_fit_results)
  return(fit_results)
}
