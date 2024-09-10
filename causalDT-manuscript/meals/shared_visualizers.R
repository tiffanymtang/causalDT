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

subgroup_thresholds_plot <- create_visualizer(
  .viz_fun = plot_subgroup_thresholds,
  .name = "Subgroup Thresholds Distribution Plot",
  size_preset = "medium"
)

subgroup_thresholds_errorbar_plot <- create_visualizer(
  .viz_fun = plot_subgroup_thresholds,
  .name = "Subgroup Thresholds Distribution Plot (errorbar)",
  show = c("point", "errorbar"),
  size_preset = "medium"
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
