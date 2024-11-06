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
    size = 0.7
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
subgroup_feature_selection_err_max_depth3_plot <- create_visualizer(
  .viz_fun = plot_subgroup_feature_selection_err,
  .name = "Subgroup Feature Selection Errors Plot (max depth = 3)",
  eval_name = "Subgroup Feature Selection Errors Summary (max depth = 3)",
  show = c("point", "line", "ribbon"),
  err_sd_str = "se_feature_selection_err",
  ribbon_args = list(
    alpha = 0.2
  ),
  line_args = list(
    size = 0.7
  )
)
subgroup_feature_selection_err_max_depth4_plot <- create_visualizer(
  .viz_fun = plot_subgroup_feature_selection_err,
  .name = "Subgroup Feature Selection Errors Plot (max depth = 4)",
  eval_name = "Subgroup Feature Selection Errors Summary (max depth = 4)",
  show = c("point", "line", "ribbon"),
  err_sd_str = "se_feature_selection_err",
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
  size_preset = "medium"
)

subgroup_thresholds_max_depth2_plot <- create_visualizer(
  .viz_fun = plot_subgroup_thresholds,
  .name = "Subgroup Thresholds Distribution Plot (max depth = 2)",
  eval_name = "Thresholds Summary (max depth = 2)",
  size_preset = "medium"
)
subgroup_thresholds_max_depth3_plot <- create_visualizer(
  .viz_fun = plot_subgroup_thresholds,
  .name = "Subgroup Thresholds Distribution Plot (max depth = 3)",
  eval_name = "Thresholds Summary (max depth = 3)",
  size_preset = "medium"
)
subgroup_thresholds_max_depth4_plot <- create_visualizer(
  .viz_fun = plot_subgroup_thresholds,
  .name = "Subgroup Thresholds Distribution Plot (max depth = 4)",
  eval_name = "Thresholds Summary (max depth = 4)",
  size_preset = "medium"
)

subgroup_thresholds_errorbar_plot <- create_visualizer(
  .viz_fun = plot_subgroup_thresholds,
  .name = "Subgroup Thresholds Distribution Plot (errorbar)",
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

subgroup_threshold_dist_max_depth3_plot <- create_visualizer(
  .viz_fun = plot_pred_err,
  .name = "Threshold Distances Plot (max depth = 3)",
  eval_name = "Threshold Distances Summary (max depth = 3)",
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

subgroup_threshold_dist_max_depth4_plot <- create_visualizer(
  .viz_fun = plot_pred_err,
  .name = "Threshold Distances Plot (max depth = 4)",
  eval_name = "Threshold Distances Summary (max depth = 4)",
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

subgroup_nsplits_plot <- create_visualizer(
  .viz_fun = plot_subgroup_nsplits,
  .name = "Number of Splits Plot",
  type = "n_splits_per_tree",
  size_preset = "medium"
)

subgroup_ntrees_plot <- create_visualizer(
  .viz_fun = plot_subgroup_nsplits,
  .name = "Number of Trees Plot",
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

subgroup_stability_plot <- create_visualizer(
  .viz_fun = plot_stability_diagnostics,
  .name = "Subgroup Stability Diagnostics Plot"
)
