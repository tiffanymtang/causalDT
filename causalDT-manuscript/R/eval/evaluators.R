eval_subgroup_feature_selection_err <- function(fit_results,
                                                vary_params = NULL,
                                                nested_cols = NULL,
                                                group_cols = NULL,
                                                na_rm = FALSE,
                                                summary_funs = c("mean", "median", "min",
                                                                 "max", "sd", "raw"),
                                                custom_summary_funs = list(
                                                  "se_feature_selection_err" =
                                                    function(x) sd(x) / sqrt(length(x))
                                                ),
                                                eval_id = "feature_selection_err") {

  group_vars <- c(
    ".dgp_name", ".method_name", vary_params, group_cols, ".metric"
  )

  fit_results <- add_best_distilled_method(
    fit_results = fit_results,
    vary_params = vary_params
  )

  subgroup_feature_selection_err <- function(data) {
    true_vars <- names(data[["true_thresholds"]][[1]])
    model_info <- data[["model_info"]][[1]]

    if (is.null(model_info)) {
      tp <- 0
      fp <- 0
      fn <- length(true_vars)
      f1 <- 0
    } else {
      subgroup_vars <- unique(collapse_dummy_vars(model_info$var))
      tp <- sum(subgroup_vars %in% true_vars)
      fp <- sum(!(subgroup_vars %in% true_vars))
      fn <- sum(!(true_vars %in% subgroup_vars))
      f1 <-  (2 * tp) / (2 * tp + fp + fn)
    }
    return(tibble::tibble(F1 = f1, tp = tp, fp = fp, fn = fn))
  }

  eval_data <- simChef::eval_constructor(
    fit_results = fit_results, vary_params = vary_params,
    fun = subgroup_feature_selection_err,
    nested_cols = nested_cols, group_cols = group_cols, na_rm = na_rm
  ) |>
    tidyr::unnest(.eval_result) |>
    tidyr::pivot_longer(
      cols = c(F1, tp, fp, fn), names_to = ".metric", values_to = ".estimate"
    ) |>
    dplyr::group_by(dplyr::across(tidyselect::any_of(group_vars)))

  if (!is.null(summary_funs) || !is.null(custom_summary_funs)) {
    eval_summary <- simChef::eval_summarizer(
      eval_data = eval_data, eval_id = eval_id, value_col = ".estimate",
      summary_funs = summary_funs, custom_summary_funs = custom_summary_funs,
      na_rm = na_rm
    )
  } else {
    eval_summary <- eval_data
  }

  return(eval_summary)
}


eval_subgroup_thresholds <- function(fit_results,
                                     vary_params = NULL,
                                     nested_cols = NULL,
                                     group_cols = NULL,
                                     na_rm = FALSE,
                                     summary_funs = c("mean", "median", "min",
                                                      "max", "sd", "raw"),
                                     custom_summary_funs = list(
                                       "se_threshold" =
                                         function(x) sd(x) / sqrt(length(x))
                                     ),
                                     eval_id = "threshold") {

  group_vars <- c(
    ".dgp_name", ".method_name", vary_params, group_cols,
    ".var", "categorical", "true_var"
  )
  nreps <- length(unique(fit_results$.rep))

  # fit_results <- add_best_distilled_method(
  #   fit_results = fit_results,
  #   vary_params = vary_params
  # )

  subgroup_thresholds <- function(data) {
    model_info <- data[["model_info"]][[1]]
    if (length(unlist(data[["true_thresholds"]])) == 1) {
      true_vars <- names(data[["true_thresholds"]])
    } else {
      true_vars <- names(data[["true_thresholds"]][[1]])
    }
    if (is.null(model_info) || (nrow(model_info) == 0)) {
      return(NULL)
    }
    subgroup_thresholds <- model_info |>
      dplyr::rename(.var = var) |>
      dplyr::mutate(
        categorical = stringr::str_detect(.var, "\\.\\."),
        thr = dplyr::case_when(
          categorical ~ NA,
          TRUE ~ thr
        ),
        cat_thr = dplyr::case_when(
          categorical ~ stringr::str_extract(.var, "[^\\.\\.]*$"),
          TRUE ~ NA
        ),
        .var = collapse_dummy_vars(.var)
      ) |>
      dplyr::group_by(.var, categorical) |>
      dplyr::summarise(
        n_splits = dplyr::n(),
        est_thrs = list(thr),
        n_thrs = sum(!is.na(thr)),
        est_thrs_cat = list(cat_thr),
        n_thrs_cat = sum(!is.na(cat_thr)),
        .groups = "keep"
      ) |>
      dplyr::mutate(
        true_var = .var %in% !!true_vars
      )
    return(subgroup_thresholds)
  }

  eval_data <- simChef::eval_constructor(
    fit_results = fit_results, vary_params = vary_params,
    fun = subgroup_thresholds,
    nested_cols = nested_cols, group_cols = group_cols, na_rm = na_rm
  ) |>
    tidyr::unnest(.eval_result) |>
    dplyr::group_by(dplyr::across(tidyselect::any_of(group_vars)))

  if (!is.null(summary_funs) || !is.null(custom_summary_funs)) {
    n_splits_eval_summary <- eval_data |>
      simChef::eval_summarizer(
        eval_id = "n_splits", value_col = "n_splits",
        summary_funs = c("raw"),
        custom_summary_funs = list(
          "n_splits_per_tree" = function(x) mean(x),
          "n_trees" = function(x) length(x)
        )
      )
    thr_eval_summary <- eval_data |>
      dplyr::filter(n_thrs > 0) |>
      tidyr::unnest(est_thrs) |>
      simChef::eval_summarizer(
        eval_id = eval_id, value_col = "est_thrs",
        summary_funs = summary_funs, custom_summary_funs = custom_summary_funs,
        na_rm = na_rm
      )
    thr_eval_summary <- dplyr::left_join(
      n_splits_eval_summary, thr_eval_summary, by = group_vars
    )
    if (any(eval_data$categorical)) {
      cat_thr_eval_summary <- eval_data |>
        tidyr::unnest(est_thrs_cat) |>
        dplyr::filter(!is.na(est_thrs_cat)) |>
        simChef::eval_summarizer(
          eval_id = paste0(eval_id, "_cat"), value_col = "est_thrs_cat",
          summary_funs = "raw", custom_summary_funs = custom_summary_funs,
          na_rm = na_rm
        )
      thr_eval_summary <- dplyr::left_join(
        thr_eval_summary, cat_thr_eval_summary, by = group_vars
      )
    }
    eval_summary <- thr_eval_summary |>
      dplyr::mutate(
        # hack so that geom_density does not omit this group (originally with n_thresholds = 1) when plotting
        raw_threshold = purrr::map2(
          raw_threshold, sd_threshold,
          function(thr, sd_thr) {
            if (is.na(sd_thr)) {
              return(c(thr, thr))
            } else {
              return(thr)
            }
          }
        )
      )
  } else {
    eval_summary <- eval_data
  }

  return(eval_summary)
}


eval_subgroup_cate_err <- function(fit_results,
                                   vary_params = NULL,
                                   nested_cols = NULL,
                                   group_cols = NULL,
                                   na_rm = FALSE,
                                   summary_funs = c("mean", "median", "min",
                                                    "max", "sd", "raw"),
                                   custom_summary_funs = NULL,
                                   eval_id = "cate_err") {

  group_vars <- c(
    ".dgp_name", ".method_name", vary_params, group_cols, ".metric"
  )

  fit_results <- add_best_distilled_method(
    fit_results = fit_results,
    vary_params = vary_params
  )

  subgroup_cate_err <- function(data) {
    holdout_idxs <- data[["holdout_idxs"]][[1]]
    tau <- data[["tau_denoised"]][[1]]
    group_cates <- data[["group_cates"]][[1]]
    if (is.null(group_cates)) {
      return(NULL)
    }
    if (!is.null(holdout_idxs)) {
      tau <- tau[holdout_idxs]
    }
    tauhat <- rep(NA, length(tau))
    for (i in 1:nrow(group_cates)) {
      tauhat[group_cates$.sample_idxs[[i]]] <- group_cates$estimate[[i]]
    }
    df <- tibble::tibble(truth = tau, estimate = tauhat)
    metric_out <- yardstick::metrics(
      data = df, truth = truth, estimate = estimate, na_rm = TRUE
    )
    return(metric_out)
  }

  eval_data <- simChef::eval_constructor(
    fit_results = fit_results, vary_params = vary_params,
    fun = subgroup_cate_err,
    nested_cols = nested_cols, group_cols = group_cols, na_rm = na_rm
  ) |>
    tidyr::unnest(.eval_result) |>
    dplyr::group_by(dplyr::across(tidyselect::any_of(group_vars)))

  if (!is.null(summary_funs) || !is.null(custom_summary_funs)) {
    eval_summary <- simChef::eval_summarizer(
      eval_data = eval_data, eval_id = eval_id, value_col = ".estimate",
      summary_funs = summary_funs, custom_summary_funs = custom_summary_funs,
      na_rm = na_rm
    )
  } else {
    eval_summary <- eval_data
  }

  return(eval_summary)
}


eval_stability_diagnostics <- function(fit_results,
                                       vary_params = NULL,
                                       max_depths = 1:4) {

  best_methods_df <- get_best_distilled_method_info(
    fit_results,
    vary_params = vary_params,
    max_depths = max_depths
  )

  best_methods_summary <- best_methods_df |>
    dplyr::group_by(
      dplyr::across(
        tidyselect::all_of(c(".dgp_name", ".method_name", vary_params, "depth"))
      )
    ) |>
    dplyr::summarize(
      n = dplyr::n(),
      .groups = "drop"
    ) |>
    tidyr::pivot_wider(
      names_from = .method_name,
      values_from = n
    )

  return(
    list(
      "Best Distilled Methods" = best_methods_df,
      "Best Distilled Methods Summary" = best_methods_summary
    )
  )
}
