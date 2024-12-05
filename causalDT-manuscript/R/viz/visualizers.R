plot_subgroup_feature_selection_err <- function(fit_results = NULL,
                                                eval_results,
                                                vary_params = NULL,
                                                eval_name = "Subgroup Feature Selection Errors Summary",
                                                eval_id = "feature_selection_err",
                                                metrics = c(
                                                  "F1",
                                                  "# True Positives",
                                                  "# False Positives"
                                                ),
                                                facet_type = "wrap",
                                                facet_args = list(
                                                  nrow = 1,
                                                  scales = "free_y",
                                                  strip.position = "left"
                                                ),
                                                show = c("point", "line"),
                                                axis_title_size = 16,
                                                axis_text_size = 12,
                                                legend_title_size = 16,
                                                legend_text_size = 12,
                                                ...) {
  eval_results[[eval_name]] <- eval_results[[eval_name]] |>
    dplyr::mutate(
      .metric = dplyr::case_when(
        # .metric == "fn" ~ "False Negatives",
        .metric == "fp" ~ "# False Positives",
        .metric == "tp" ~ "# True Positives",
        .metric == "nsubgroups" ~ "Number of Subgroups",
        TRUE ~ .metric
      ) |>
        factor(
          levels = metrics
        )
    ) |>
    dplyr::filter(
      !is.na(.metric)
    )

  plt <- plot_pred_err(
    eval_results = eval_results,
    vary_params = vary_params,
    eval_name = eval_name,
    eval_id = eval_id,
    facet_type = facet_type,
    facet_args = facet_args,
    show = show,
    ...
  ) +
    vthemes::theme_vmodern(
      axis_title_size = axis_title_size,
      axis_text_size = axis_text_size,
      legend_title_size = legend_title_size,
      legend_text_size = legend_text_size
    ) +
    ggplot2::theme(
      strip.background = ggplot2::element_rect(
        fill = "transparent", color = "transparent"
      ),
      strip.text = ggplot2::element_text(
        face = "bold", color = "black", size = axis_title_size
      ),
      strip.placement = "outside",
      axis.title.y = ggplot2::element_blank()
    )

  return(plt)
}


plot_subgroup_thresholds <- function(fit_results = NULL,
                                     eval_results,
                                     vary_params = NULL,
                                     eval_name = "Thresholds Summary",
                                     feature_col = ".var",
                                     x_str = ".method_name",
                                     rm_methods = c("Linear Regression", "Lasso"),
                                     method_levels = NULL,
                                     show = c("violin", "boxplot"),
                                     show_true_vars_only = TRUE,
                                     show_true_threshold = TRUE,
                                     ...) {

  if (!is.null(rm_methods)) {
    eval_results[[eval_name]] <- eval_results[[eval_name]] |>
      dplyr::filter(!(.method_name %in% !!rm_methods))
  }
  if (show_true_vars_only & any(eval_results[[eval_name]]$true_var)) {
    eval_results[[eval_name]] <- eval_results[[eval_name]] |>
      dplyr::filter(true_var)
  }
  if (is.null(method_levels)) {
    method_levels <- unique(eval_results[[eval_name]]$.method_name)
  }

  plt_df <- eval_results[[eval_name]] |>
    tidyr::unnest_longer(raw_threshold) |>
    dplyr::mutate(
      .method_name = factor(
        .method_name, levels = method_levels
      )
    )

  if (identical(show, "dotplot")) {
    if (length(vary_params) == 1) {
      facet_layer <- ggplot2::facet_grid(
        rows = ggplot2::vars(!!rlang::sym(vary_params)),
        cols = ggplot2::vars(!!rlang::sym(feature_col))
      )
    } else if (length(vary_params) > 1) {
      facet_layer <- ggplot2::facet_grid(
        rows = ggplot2::vars(!!!rlang::syms(vary_params)),
        cols = ggplot2::vars(!!rlang::sym(feature_col))
      )
    } else {
      facet_layer <- ggplot2::facet_wrap(
        ggplot2::vars(!!rlang::sym(feature_col))
      )
    }
    plt <- plt_df |>
      ggplot2::ggplot() +
      ggplot2::aes(
        x = .method_name,
        y = raw_threshold,
        fill = .method_name
      ) +
      facet_layer +
      # ggplot2::facet_wrap(
      #   ~ .data[[vary_params]], nrow = 1
      # ) +
      ggplot2::geom_dotplot(
        binaxis = "y",
        binwidth = 0.1,
        stackdir = "center"
      ) +
      vthemes::scale_fill_vmodern(discrete = TRUE) +
      vthemes::theme_vmodern(x_text_angle = TRUE)
  } else {
    plt <- plt_df |>
      ggplot2::ggplot()
    if ("violin" %in% show) {
      plt <- plt +
        ggplot2::geom_violin(
          ggplot2::aes(
            x = .method_name,
            y = raw_threshold,
            fill = .method_name,
          ),
          alpha = 0.4,
          color = "transparent",
          position = ggplot2::position_dodge(0.9),
          scale = "width"
        )
    }
    if ("boxplot" %in% show) {
      plt <- plt +
        ggplot2::geom_boxplot(
          ggplot2::aes(
            x = .method_name,
            y = raw_threshold,
            color = .method_name
          ),
          width = 0.2,
          # alpha = 0,
          outlier.size = 0.2,
          position = ggplot2::position_dodge(0.9)
        )
    }
    if ("point" %in% show) {
      plt <- plt +
        ggplot2::geom_point(
          ggplot2::aes(
            x = .method_name,
            y = mean_threshold,
            color = .method_name
          )
        )
    }
    if ("errorbar" %in% show) {
      plt <- plt +
        ggplot2::geom_errorbar(
          ggplot2::aes(
            x = .method_name,
            ymin = mean_threshold - se_threshold,
            ymax = mean_threshold + se_threshold,
            color = .method_name
          ),
          width = 0.1
        )
    }
    if (length(unique(plt_df[[feature_col]])) == 1) {
      plt <- plt +
        ggplot2::facet_grid(
          cols = ggplot2::vars(!!rlang::sym(vary_params[1])),
          rows = ggplot2::vars(
            !!rlang::sym(vary_params[2]),
            .dgp_name
          ),
          scales = "free_y"
        )
    } else {
      if (is.null(vary_params)) {
        plt <- plt +
          ggplot2::facet_wrap(
            ggplot2::vars(!!rlang::sym(feature_col)),
            scales = "free_y"
          )
      } else if (length(vary_params) == 1) {
        plt <- plt +
          ggplot2::facet_grid(
            cols = ggplot2::vars(!!rlang::sym(vary_params)),
            rows = ggplot2::vars(!!rlang::sym(feature_col)),
            scales = "free_y"
          )
      } else {
        plt <- plt +
          ggplot2::facet_grid(
            cols = ggplot2::vars(!!!rlang::syms(vary_params)),
            rows = ggplot2::vars(!!rlang::sym(feature_col)),
            scales = "free_y"
          )
      }
    }
    plt <- plt +
      ggplot2::labs(
        fill = "Method",
        color = "Method",
        y = "Threshold",
        x = "Method"
      ) +
      vthemes::scale_color_vmodern(discrete = TRUE) +
      vthemes::scale_fill_vmodern(discrete = TRUE) +
      vthemes::theme_vmodern(...) +
      ggplot2::theme(
        axis.text.x = ggplot2::element_blank(),
        axis.title.x = ggplot2::element_blank(),
        axis.ticks.x = ggplot2::element_blank()
      )
  }

  if (show_true_threshold & any(eval_results[[eval_name]]$true_var)) {
    if (!is.list(fit_results[["true_thresholds"]][[1]])) {
      true_thresholds_ls <- list(fit_results[["true_thresholds"]][[1]]) |>
        setNames(names(fit_results[["true_thresholds"]][1]))
    } else {
      true_thresholds_ls <- fit_results[["true_thresholds"]][[1]]
    }
    true_thresholds_df <- purrr::map(
      true_thresholds_ls,
      ~ tibble::tibble(true_thrs = list(.x))
    ) |>
      dplyr::bind_rows(.id = ".var") |>
      tidyr::unnest("true_thrs")

    plt <- plt +
      ggplot2::geom_hline(
        ggplot2::aes(yintercept = true_thrs),
        data = true_thresholds_df,
        linetype = "dashed",
        color = "black"
      )
  }

  return(plt)
}


plot_subgroup_nsplits <- function(fit_results = NULL,
                                  eval_results,
                                  vary_params = NULL,
                                  eval_name = "Thresholds Summary",
                                  rm_methods = NULL,
                                  method_levels = NULL,
                                  type = c("n_splits_per_tree", "n_trees"),
                                  ...) {

  type_name <- dplyr::case_when(
    type == "n_splits_per_tree" ~ "Average #\nSplits per Tree",
    type == "n_trees" ~ "# Trees"
  )
  if (!is.null(rm_methods)) {
    eval_results[[eval_name]] <- eval_results[[eval_name]] |>
      dplyr::filter(!(.method_name %in% !!rm_methods))
  }
  if (is.null(method_levels)) {
    method_levels <- unique(eval_results[[eval_name]]$.method_name)
  }

  plt_df <- eval_results[[eval_name]] |>
    dplyr::ungroup() |>
    tidyr::pivot_wider(
      id_cols = c(.dgp_name, .method_name, tidyselect::all_of(vary_params)),
      names_from = ".var",
      values_from = tidyselect::all_of(type),
      values_fill = 0
    ) |>
    tidyr::pivot_longer(
      cols = -c(.dgp_name, .method_name, tidyselect::all_of(vary_params)),
      names_to = ".var",
      values_to = type
    ) |>
    dplyr::mutate(
      dplyr::across(
        tidyselect::all_of(type), ~ round(.x, 1)
      ),
      .method_name = factor(
        .method_name, levels = rev(method_levels)
      )
    )
  if (isTRUE(all(stringr::str_detect(plt_df$.var, "^X[0-9]+$")))) {
    plt_df <- plt_df |>
      dplyr::mutate(
        .var = factor(
          .var,
          levels = paste0(
            "X", sort(unique(as.numeric(stringr::str_remove(.var, "X"))))
          )
        )
      )
  }

  plt <- plt_df |>
    ggplot2::ggplot() +
    ggplot2::aes(
      x = .var, y = .method_name,
      fill = !!rlang::sym(type), label = !!rlang::sym(type)
    ) +
    ggplot2::geom_tile() +
    ggplot2::geom_text() +
    ggplot2::labs(
      y = "Method", fill = type_name
    ) +
    vthemes::scale_fill_vmodern(
      discrete = FALSE, viridis_option = "magma"
    ) +
    vthemes::theme_vmodern(...) +
    ggplot2::coord_cartesian(expand = FALSE)

  if (!is.null(vary_params)) {
    plt <- plt +
      ggplot2::facet_grid(
        rows = ggplot2::vars(!!rlang::sym(vary_params))
      )
  }

  return(plt)
}


plot_subgroup_cates <- function(fit_results,
                                eval_results = NULL,
                                vary_params = NULL,
                                keep_methods = NULL,
                                show = c("point", "density"),
                                ...) {
  show <- match.arg(show)
  id_cols <- c(".rep", ".dgp_name", ".method_name", vary_params)

  plt_df <- fit_results |>
    dplyr::select(
      tidyselect::all_of(id_cols), group_cates
    ) |>
    tidyr::unnest(group_cates)
  if (!is.null(keep_methods)) {
    plt_df <- plt_df |>
      dplyr::filter(.method_name %in% !!keep_methods)
  }

  if (show == "point") {
    plt <- plt_df |>
      ggplot2::ggplot() +
      ggplot2::aes(
        x = estimate, y = .rep#, color_str = ".method_name"
      ) +
      ggplot2::geom_point() +
      vthemes::theme_vmodern(...)
  } else if (show == "density") {
    plt <- plt_df |>
      ggplot2::ggplot() +
      ggplot2::aes(
        x = estimate
      ) +
      ggplot2::geom_density(alpha = 0.4) +
      vthemes::theme_vmodern(...)
  }

  if ("tau_denoised" %in% colnames(fit_results)) {
    true_taus_df <- tibble::tibble(
      tau = c(unique(fit_results$tau_denoised[[1]]))
    )
    plt <- plt +
      ggplot2::geom_vline(
        ggplot2::aes(xintercept = tau), data = true_taus_df
      )
  }

  if (!is.null(vary_params)) {
    plt <- plt +
      ggplot2::facet_grid(
        cols = ggplot2::vars(!!rlang::sym(vary_params)),
        rows = ggplot2::vars(.method_name)
      )
  } else {
    plt <- plt +
      ggplot2::facet_wrap(~ .method_name)
  }
  return(plt)
}


plot_stability_diagnostics <- function(fit_results,
                                       eval_results,
                                       vary_params = NULL,
                                       max_depth = 4,
                                       ...) {
  id_cols <- c(".rep", ".dgp_name", ".method_name", vary_params)
  group_cols <- c(".dgp_name", ".method_name", vary_params)
  n_reps <- length(unique(fit_results$.rep))

  plt_df <- fit_results |>
    dplyr::filter(
      stringr::str_detect(.method_name, "Distilled") |
        (.method_name == "Causal Tree")
    ) |>
    dplyr::mutate(
      jaccard = purrr::map(
        stability_diagnostics,
        ~ tibble::tibble(
          depth = 1:length(.x$jaccard_mean),
          jaccard = .x$jaccard_mean,
          jaccard_scaled = .x$jaccard_scaled_mean
        )
      )
    ) |>
    dplyr::select(
      tidyselect::all_of(id_cols), jaccard
    ) |>
    tidyr::unnest(jaccard) |>
    dplyr::filter(
      depth <= !!max_depth
    )

  plt1 <- plt_df |>
    dplyr::group_by(
      dplyr::across(tidyselect::all_of(c(group_cols, "depth")))
    ) |>
    dplyr::summarise(
      mean_jaccard = mean(jaccard_scaled),
      sd_jaccard = sd(jaccard_scaled),
      se_jaccard = sd(jaccard_scaled) / sqrt(dplyr::n()),
      .groups = "drop"
    ) |>
    ggplot2::ggplot() +
    ggplot2::aes(
      x = depth, y = mean_jaccard, color = .method_name
    ) +
    ggplot2::geom_line() +
    ggplot2::geom_ribbon(
      ggplot2::aes(
        x = depth,
        ymin = mean_jaccard - se_jaccard,
        ymax = mean_jaccard + se_jaccard,
        fill = .method_name
      ),
      inherit.aes = FALSE,
      alpha = 0.2
    ) +
    # ggplot2::facet_grid(
    #   rows = ggplot2::vars(metric)
    # ) +
    vthemes::scale_color_vmodern(discrete = TRUE) +
    vthemes::scale_fill_vmodern(discrete = TRUE) +
    vthemes::theme_vmodern(...)

  feature_plt_df <- fit_results |>
    dplyr::filter(
      stringr::str_detect(.method_name, "Distilled") |
        (.method_name == "Causal Tree")
    ) |>
    dplyr::mutate(
      feature_dist = purrr::map(stability_diagnostics, "feature_distribution")
    ) |>
    dplyr::select(
      tidyselect::all_of(id_cols), feature_dist
    ) |>
    tidyr::unnest(feature_dist) |>
    dplyr::group_by(
      dplyr::across(c(tidyselect::all_of(group_cols), "depth", "feature"))
    ) |>
    dplyr::summarise(
      mean_freq = sum(freq) / n_reps,
      sd_freq = 1 / (n_reps - 1) * sqrt(sum((freq - mean_freq)^2)),
      .groups = "drop"
    )

  if (isTRUE(all(stringr::str_detect(feature_plt_df$feature, "^X[0-9]+$")))) {
    feature_plt_df <- feature_plt_df |>
      dplyr::mutate(
        feature = factor(
          feature,
          levels = paste0(
            "X", sort(unique(as.numeric(stringr::str_remove(feature, "X"))))
          )
        )
      )
  }

  plt3 <- feature_plt_df |>
    dplyr::filter(depth <= !!max_depth) |>
    dplyr::mutate(
      # .method_name = stringr::str_remove(.method_name, " \\(pruned\\)"),
      depth = sprintf("Depth = %s", depth)
    ) |>
    ggplot2::ggplot() +
    ggplot2::geom_bar(
      ggplot2::aes(
        x = .method_name, y = mean_freq, fill = feature
      ),
      stat = "identity",
      position = "fill"
    ) +
    ggplot2::facet_grid(
      cols = ggplot2::vars(depth)
    ) +
    vthemes::scale_fill_vmodern(discrete = TRUE) +
    vthemes::theme_vmodern(...)

  if (!is.null(vary_params)) {
    plt1 <- plt1 +
      ggplot2::facet_grid(
        # rows = ggplot2::vars(metric),
        cols = ggplot2::vars(!!rlang::sym(vary_params))
      )
    plt3 <- plt3 +
      ggplot2::facet_grid(
        rows = ggplot2::vars(!!rlang::sym(vary_params)),
        cols = ggplot2::vars(depth)
      )
  }

  plt <- patchwork::wrap_plots(
    plt1, plt3, nrow = 2, ncol = 1, heights = c(1, 3)
  )

  return(plt)
}

