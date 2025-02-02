method_levels <- c(
  "Distilled Causal Forest",
  "Distilled Causal Forest (unpruned)",
  "Distilled Rboost",
  "Distilled Rboost (unpruned)",
  "Distilled Rlasso",
  "Distilled Rlasso (unpruned)",
  "Virtual Twins",
  "Causal Tree",
  "Causal Tree (unpruned)",
  "Linear Regression",
  "Lasso",
  "Distilled Causal Forest (rulefit v1)",
  "Distilled Causal Forest (rulefit v2)",
  "Distilled Causal Forest (rulefit v3)",
  "Distilled Causal Forest (rulefit v4)",
  "Distilled Causal Forest (rulefit v5)",
  "Distilled Causal Forest (rulefit v6)"
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
  )
)

subgroup_feature_selection_err_max_depth2_plot <- create_visualizer(
  .viz_fun = plot_subgroup_feature_selection_err,
  .name = "Subgroup Feature Selection Errors Plot (max depth = 2)",
  eval_name = "Subgroup Feature Selection Errors Summary (max depth = 2)",
  show = c("point", "line", "ribbon"),
  err_sd_str = "se_feature_selection_err",
  ribbon_args = list(
    alpha = 0.2
  ),
  line_args = list(
    size = 0.7
  )
)

subgroup_feature_selection_err_crossfit_plot <- create_visualizer(
  .viz_fun = plot_subgroup_feature_selection_err,
  .name = "Subgroup Feature Selection Errors Plot",
  show = c("point", "line"),#, "ribbon"),
  err_sd_str = "se_feature_selection_err",
  x_str = "tau_heritability",
  color_str = "nreps_crossfit",
  plot_by = NULL,
  ribbon_args = list(
    alpha = 0.2
  ),
  line_args = list(
    size = 0.7
  )
)


subgroup_thresholds_plot <- create_visualizer(
  .viz_fun = plot_subgroup_thresholds,
  .name = "Subgroup Thresholds Distribution Plot",
  method_levels = method_levels,
  size_preset = "medium"
)

subgroup_thresholds_max_depth2_plot <- create_visualizer(
  .viz_fun = plot_subgroup_thresholds,
  .name = "Subgroup Thresholds Distribution Plot (max depth = 2)",
  eval_name = "Thresholds Summary (max depth = 2)",
  method_levels = method_levels,
  size_preset = "medium"
)

subgroup_thresholds_crossfit_plot <- create_visualizer(
  .viz_fun = plot_subgroup_thresholds,
  .name = "Subgroup Thresholds Distribution Plot",
  method_levels = method_levels,
  size_preset = "medium"
)

subgroup_thresholds_errorbar_plot <- create_visualizer(
  .viz_fun = plot_subgroup_thresholds,
  .name = "Subgroup Thresholds Distribution Plot (errorbar)",
  method_levels = method_levels,
  show = c("point", "errorbar"),
  size_preset = "medium"
)

subgroup_threshold_dist_plot <- create_visualizer(
  .viz_fun = plot_pred_err,
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
  )
)

subgroup_threshold_dist_max_depth2_plot <- create_visualizer(
  .viz_fun = plot_pred_err,
  .name = "Threshold Distances Plot (max depth = 2)",
  eval_name = "Threshold Distances Summary (max depth = 2)",
  eval_id = "threshold_dist",
  show = c("point", "line", "ribbon"),
  err_sd_str = "se_threshold_dist",
  facet_formula = ~ .var,
  ribbon_args = list(
    alpha = 0.2
  ),
  line_args = list(
    size = 0.7
  )
)

subgroup_threshold_dist_crossfit_plot <- create_visualizer(
  .viz_fun = plot_pred_err,
  .name = "Threshold Distances Plot",
  eval_name = "Threshold Distances Summary",
  eval_id = "threshold_dist",
  show = c("point", "line"),#, "ribbon"),
  err_sd_str = "se_threshold_dist",
  x_str = "tau_heritability",
  color_str = "nreps_crossfit",
  plot_by = NULL,
  facet_formula = ~ .var,
  ribbon_args = list(
    alpha = 0.2
  ),
  line_args = list(
    size = 0.7
  )
)

subgroup_nsplits_plot <- create_visualizer(
  .viz_fun = plot_subgroup_nsplits,
  .name = "Number of Splits Plot",
  method_levels = method_levels,
  type = "n_splits_per_tree",
  size_preset = "medium"
)

subgroup_nsplits_max_depth2_plot <- create_visualizer(
  .viz_fun = plot_subgroup_nsplits,
  .name = "Number of Splits Plot (max depth = 2)",
  eval_name = "Thresholds Summary (max depth = 2)",
  method_levels = method_levels,
  type = "n_splits_per_tree",
  size_preset = "medium"
)

subgroup_ntrees_plot <- create_visualizer(
  .viz_fun = plot_subgroup_nsplits,
  .name = "Number of Trees Plot",
  method_levels = method_levels,
  type = "n_trees",
  size_preset = "medium"
)

subgroup_ntrees_max_depth2_plot <- create_visualizer(
  .viz_fun = plot_subgroup_nsplits,
  .name = "Number of Trees Plot (max depth = 2)",
  eval_name = "Thresholds Summary (max depth = 2)",
  method_levels = method_levels,
  type = "n_trees",
  size_preset = "medium"
)

subgroup_cates_plot <- create_visualizer(
  .viz_fun = plot_subgroup_cates,
  .name = "Subgroup CATEs Plot"
)

subgroup_cates_err_plot <- create_visualizer(
  .viz_fun = plot_pred_err,
  .name = "Subgroup CATE Errors Plot",
  eval_name = "Subgroup CATE Errors Summary",
  eval_id = "cate_err",
  show = c("point", "line", "ribbon"),
  metrics = yardstick::metric_set(yardstick::rmse),
  err_sd_str = "se_cate_err",
  ribbon_args = list(
    alpha = 0.2
  ),
  line_args = list(
    size = 0.7
  )
)

subgroup_cates_err_max_depth2_plot <- create_visualizer(
  .viz_fun = plot_pred_err,
  .name = "Subgroup CATE Errors Plot (max depth = 2)",
  eval_name = "Subgroup CATE Errors Summary (max depth = 2)",
  eval_id = "cate_err",
  show = c("point", "line", "ribbon"),
  metrics = yardstick::metric_set(yardstick::rmse),
  err_sd_str = "se_cate_err",
  ribbon_args = list(
    alpha = 0.2
  ),
  line_args = list(
    size = 0.7
  )
)

subgroup_cates_err_crossfit_plot <- create_visualizer(
  .viz_fun = plot_pred_err,
  .name = "Subgroup CATE Errors Plot",
  eval_name = "Subgroup CATE Errors Summary",
  eval_id = "cate_err",
  show = c("point", "line"),#, "ribbon"),
  metrics = yardstick::metric_set(yardstick::rmse),
  err_sd_str = "se_cate_err",
  x_str = "tau_heritability",
  color_str = "nreps_crossfit",
  plot_by = NULL,
  ribbon_args = list(
    alpha = 0.2
  ),
  line_args = list(
    size = 0.7
  )
)

subgroup_stability_plot <- create_visualizer(
  .viz_fun = plot_stability_diagnostics,
  .name = "Subgroup Stability Diagnostics Plot"
)
