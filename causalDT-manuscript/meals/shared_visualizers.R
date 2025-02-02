method_levels <- c(
  "Distilled Causal Forest",
  "Distilled Causal Forest (unpruned)",
  "Distilled Rboost",
  "Distilled Rboost (unpruned)",
  "Virtual Twins",
  "Causal Tree",
  "Causal Tree (unpruned)",
  "Linear Regression",
  "Lasso",
  "CART",
  "Rulefit (linear + rules, max depth = 3)",
  "Rulefit (linear + rules, max depth = 2)",
  "Rulefit (rules only, max depth = 3)",
  "Rulefit (rules only, max depth = 2)"
)

COLORS <- c(
  "Distilled Causal Forest" = "#1f5d8f",
  "Distilled Causal Forest (unpruned)" = "#1f5d8f",
  "Distilled Rboost" = "#6aafe4",
  "Distilled Rboost (unpruned)" = "#6aafe4",
  "Virtual Twins" = "#768c50",
  "Causal Tree" = "#953a2f",
  "Causal Tree (unpruned)" = "#953a2f",
  "Linear Regression" = "#b2954a",
  "Lasso" = "#D2BE90",
  "CART" = "#1f5d8f",
  "Rulefit (linear + rules, max depth = 3)" = "#FF9300",
  "Rulefit (linear + rules, max depth = 2)" = "#E7298A",
  "Rulefit (rules only, max depth = 3)" = "#66A61E",
  "Rulefit (rules only, max depth = 2)" = "#7570B3"
)
LINETYPES <- c(
  "Distilled Causal Forest" = "solid",
  "Distilled Causal Forest (unpruned)" = "dashed",
  "Distilled Rboost" = "solid",
  "Distilled Rboost (unpruned)" = "dashed",
  "Virtual Twins" = "solid",
  "Causal Tree" = "solid",
  "Causal Tree (unpruned)" = "dashed",
  "Linear Regression" = "solid",
  "Lasso" = "solid",
  "CART" = "solid",
  "Rulefit (linear + rules, max depth = 3)" = "solid",
  "Rulefit (linear + rules, max depth = 2)" = "solid",
  "Rulefit (rules only, max depth = 3)" = "solid",
  "Rulefit (rules only, max depth = 2)" = "solid"
)

COLORS2 <- COLORS
COLORS2["Distilled Causal Forest (unpruned)"] <- "#96aac6"
COLORS2["Distilled Rboost (unpruned)"] <- "#bad6f2"
COLORS2["Causal Tree (unpruned)"] <- "#d09a90"

CROSSFIT_COLORS <- c(
  "#981643", "#ED724F", "#F8B06E", "#FCE096", "#8FB989", "#4287B7", "#5E519B"
)

num_subgroups_plot <- create_visualizer(
  .viz_fun = plot_subgroup_feature_selection_err,
  .name = "Number of Subgroups Plot",
  show = c("point", "line", "ribbon"),
  metrics = c("Number of Subgroups", "# True Positives", "# False Positives"),
  err_sd_str = "se_feature_selection_err",
  ribbon_args = list(
    alpha = 0.2
  ),
  line_args = list(
    size = 1.1
  ),
  add_ggplot_layers = list(
    ggplot2::scale_color_manual(breaks = names(COLORS), values = COLORS),
    ggplot2::scale_fill_manual(breaks = names(COLORS), values = COLORS),
    ggplot2::scale_linetype_manual(breaks = names(LINETYPES), values = LINETYPES),
    ggplot2::labs(x = "Sample Size", linetype = "Method")
  ),
  .doc_options = list(
    height = 5,
    width = 12
  )
)

subgroup_feature_selection_err_plot <- create_visualizer(
  .viz_fun = plot_subgroup_feature_selection_err,
  .name = "Subgroup Feature Selection Errors Plot",
  show = c("point", "line", "ribbon"),
  err_sd_str = "se_feature_selection_err",
  ribbon_args = list(
    alpha = 0.2
  ),
  line_args = list(
    size = 0.7
  ),
  add_ggplot_layers = list(
    ggplot2::scale_color_manual(breaks = names(COLORS), values = COLORS),
    ggplot2::scale_fill_manual(breaks = names(COLORS), values = COLORS),
    ggplot2::scale_linetype_manual(breaks = names(LINETYPES), values = LINETYPES),
    ggplot2::labs(
      x = expression(
        bold(paste("Proportion of Variance Explained in ", tau, " by Covariates"))
      ),
      linetype = "Method"
    )
  ),
  .doc_options = list(
    height = 5,
    width = 12
  )
)

subgroup_feature_selection_err_max_depth2_plot <- subgroup_feature_selection_err_plot$clone()
subgroup_feature_selection_err_max_depth2_plot$name <- "Subgroup Feature Selection Errors Plot (max depth = 2)"
subgroup_feature_selection_err_max_depth2_plot$viz_params$eval_name <- "Subgroup Feature Selection Errors Summary (max depth = 2)"

subgroup_feature_selection_err_rulefit_plot <- subgroup_feature_selection_err_plot$clone()
subgroup_feature_selection_err_rulefit_plot$viz_params$preprocess_fun <- preprocess_rulefit_results

subgroup_feature_selection_err_crossfit_plot <- subgroup_feature_selection_err_plot$clone()
subgroup_feature_selection_err_crossfit_plot$viz_params$preprocess_fun <- preprocess_crossfit_results
subgroup_feature_selection_err_crossfit_plot$viz_params$show <- c("point", "line")#, "ribbon")
subgroup_feature_selection_err_crossfit_plot$viz_params$x_str <- "tau_heritability"
subgroup_feature_selection_err_crossfit_plot$viz_params$color_str <- "nreps_crossfit"
subgroup_feature_selection_err_crossfit_plot$viz_params["plot_by"] <- list(NULL)
subgroup_feature_selection_err_crossfit_plot$viz_params$add_ggplot_layers <- list(
  ggplot2::scale_color_manual(values = CROSSFIT_COLORS),
  ggplot2::scale_fill_manual(values = CROSSFIT_COLORS),
  ggplot2::scale_linetype_manual(values = "solid"),
  ggplot2::labs(
    x = expression(
      bold(paste("Proportion of Variance Explained in ", tau, " by Covariates"))
    ),
    linetype = "# of Repeated\nCrossfits",
    color = "# of Repeated\nCrossfits",
    fill = "# of Repeated\nCrossfits"
  ),
  ggplot2::guides(linetype = "none")
)

subgroup_thresholds_plot <- create_visualizer(
  .viz_fun = plot_subgroup_thresholds,
  .name = "Subgroup Thresholds Distribution Plot",
  method_levels = method_levels,
  size_preset = "medium",
  add_ggplot_layers = list(
    ggplot2::scale_color_manual(breaks = names(COLORS2), values = COLORS2),
    ggplot2::scale_fill_manual(breaks = names(COLORS2), values = COLORS2)
  ),
  .doc_options = list(
    height = 5,
    width = 12
  )
)

subgroup_thresholds_max_depth2_plot <- subgroup_thresholds_plot$clone()
subgroup_thresholds_max_depth2_plot$name <- "Subgroup Thresholds Distribution Plot (max depth = 2)"
subgroup_thresholds_max_depth2_plot$viz_params$eval_name <- "Thresholds Summary (max depth = 2)"

subgroup_thresholds_rulefit_plot <- subgroup_thresholds_plot$clone()
subgroup_thresholds_rulefit_plot$viz_params$preprocess_fun <- preprocess_rulefit_results

subgroup_threshold_dist_plot <- create_visualizer(
  .viz_fun = plot_errors,
  .name = "Threshold Distances Plot",
  eval_name = "Threshold Distances Summary",
  eval_id = "threshold_dist",
  show = c("point", "line", "ribbon"),
  err_sd_str = "se_threshold_dist",
  facet_formula = ~ .var,
  ribbon_args = list(
    alpha = 0.2
  ),
  line_args = list(
    size = 0.7
  ),
  add_ggplot_layers = list(
    ggplot2::scale_color_manual(breaks = names(COLORS), values = COLORS),
    ggplot2::scale_fill_manual(breaks = names(COLORS), values = COLORS),
    ggplot2::scale_linetype_manual(breaks = names(LINETYPES), values = LINETYPES),
    ggplot2::labs(
      x = expression(
        bold(paste("Proportion of Variance Explained in ", tau, " by Covariates"))
      ),
      y = "Threshold RMSE",
      linetype = "Method"
    )
  ),
  .doc_options = list(
    height = 4.5,
    width = 9
  )
)

subgroup_threshold_dist_max_depth2_plot <- subgroup_threshold_dist_plot$clone()
subgroup_threshold_dist_max_depth2_plot$name <- "Threshold Distances Plot (max depth = 2)"
subgroup_threshold_dist_max_depth2_plot$viz_params$eval_name <- "Threshold Distances Summary (max depth = 2)"

subgroup_threshold_dist_rulefit_plot <- subgroup_threshold_dist_plot$clone()
subgroup_threshold_dist_rulefit_plot$viz_params$preprocess_fun <- preprocess_rulefit_results

subgroup_threshold_dist_crossfit_plot <- subgroup_threshold_dist_plot$clone()
subgroup_threshold_dist_crossfit_plot$viz_params$preprocess_fun <- preprocess_crossfit_results
subgroup_threshold_dist_crossfit_plot$viz_params$show <- c("point", "line")#, "ribbon")
subgroup_threshold_dist_crossfit_plot$viz_params$x_str <- "tau_heritability"
subgroup_threshold_dist_crossfit_plot$viz_params$color_str <- "nreps_crossfit"
subgroup_threshold_dist_crossfit_plot$viz_params["plot_by"] <- list(NULL)
subgroup_threshold_dist_crossfit_plot$viz_params$add_ggplot_layers <- list(
  ggplot2::scale_color_manual(values = CROSSFIT_COLORS),
  ggplot2::scale_fill_manual(values = CROSSFIT_COLORS),
  ggplot2::scale_linetype_manual(values = "solid"),
  ggplot2::labs(
    x = expression(
      bold(paste("Proportion of Variance Explained in ", tau, " by Covariates"))
    ),
    y = "Threshold RMSE",
    linetype = "# of Repeated\nCrossfits",
    color = "# of Repeated\nCrossfits",
    fill = "# of Repeated\nCrossfits"
  ),
  ggplot2::guides(linetype = "none")
)

subgroup_nsplits_plot <- create_visualizer(
  .viz_fun = plot_subgroup_nsplits,
  .name = "Feature/Split Frequency Plot",
  method_levels = method_levels,
  size_preset = "medium",
  .doc_options = list(
    width = 18,
    height = 7.5
  )
)

subgroup_nsplits_max_depth2_plot <- subgroup_nsplits_plot$clone()
subgroup_nsplits_max_depth2_plot$name <- "Feature/Split Frequency Plot (max depth = 2)"
subgroup_nsplits_max_depth2_plot$viz_params$eval_name <- "Thresholds Summary (max depth = 2)"

subgroup_nsplits_rulefit_plot <- subgroup_nsplits_plot$clone()
subgroup_nsplits_rulefit_plot$viz_params$preprocess_fun <- preprocess_rulefit_results

subgroup_ates_plot <- create_visualizer(
  .viz_fun = plot_subgroup_ates,
  .name = "Subgroup ATEs Plot"
)

subgroup_ates_err_plot <- create_visualizer(
  .viz_fun = plot_errors,
  .name = "Subgroup ATE Errors Plot",
  eval_name = "Subgroup ATE Errors Summary",
  eval_id = "ate_err",
  show = c("point", "line", "ribbon"),
  metrics = yardstick::metric_set(yardstick::rmse),
  err_sd_str = "se_ate_err",
  ribbon_args = list(
    alpha = 0.2
  ),
  line_args = list(
    size = 0.7
  ),
  add_ggplot_layers = list(
    ggplot2::scale_color_manual(breaks = names(COLORS), values = COLORS),
    ggplot2::scale_fill_manual(breaks = names(COLORS), values = COLORS),
    ggplot2::scale_linetype_manual(breaks = names(LINETYPES), values = LINETYPES),
    ggplot2::labs(
      x = expression(
        bold(paste("Proportion of Variance Explained in ", tau, " by Covariates"))
      ),
      y = "Subgroup ATE RMSE",
      linetype = "Method"
    ),
    ggplot2::theme(
      strip.text = ggplot2::element_blank(),
      strip.background = ggplot2::element_blank()
    )
  ),
  .doc_options = list(
    height = 4.5,
    width = 8
  )
)

subgroup_ates_err_max_depth2_plot <- subgroup_ates_err_plot$clone()
subgroup_ates_err_max_depth2_plot$name <- "Subgroup ATE Errors Plot (max depth = 2)"
subgroup_ates_err_max_depth2_plot$viz_params$eval_name <- "Subgroup ATE Errors Summary (max depth = 2)"

subgroup_ates_err_rulefit_plot <- subgroup_ates_err_plot$clone()
subgroup_ates_err_rulefit_plot$viz_params$preprocess_fun <- preprocess_rulefit_results

subgroup_ates_err_crossfit_plot <- subgroup_ates_err_plot$clone()
subgroup_ates_err_crossfit_plot$viz_params$preprocess_fun <- preprocess_crossfit_results
subgroup_ates_err_crossfit_plot$viz_params$show <- c("point", "line")#, "ribbon")
subgroup_ates_err_crossfit_plot$viz_params$x_str <- "tau_heritability"
subgroup_ates_err_crossfit_plot$viz_params$color_str <- "nreps_crossfit"
subgroup_ates_err_crossfit_plot$viz_params["plot_by"] <- list(NULL)
subgroup_ates_err_crossfit_plot$viz_params$add_ggplot_layers <- list(
  ggplot2::scale_color_manual(values = CROSSFIT_COLORS),
  ggplot2::scale_fill_manual(values = CROSSFIT_COLORS),
  ggplot2::scale_linetype_manual(values = "solid"),
  ggplot2::labs(
    x = expression(
      bold(paste("Proportion of Variance Explained in ", tau, " by Covariates"))
    ),
    y = "Subgroup ATE RMSE",
    linetype = "# of Repeated\nCrossfits",
    color = "# of Repeated\nCrossfits",
    fill = "# of Repeated\nCrossfits"
  ),
  ggplot2::theme(
    strip.text = ggplot2::element_blank(),
    strip.background = ggplot2::element_blank()
  ),
  ggplot2::guides(linetype = "none")
)

subgroup_stability_plot <- create_visualizer(
  .viz_fun = plot_stability_diagnostics,
  .name = "Subgroup Stability Diagnostics Plot",
  add_ggplot_layers = list(
    ggplot2::scale_color_manual(breaks = names(COLORS), values = COLORS),
    ggplot2::scale_fill_manual(breaks = names(COLORS), values = COLORS)
  ),
  .doc_options = list(
    height = 7,
    width = 11
  )
)
