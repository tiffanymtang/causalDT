experiment <- create_experiment(
  name = EXP_NAME, save_dir = file.path(SAVE_DIR, "results_bcf", EXP_NAME)
) |>
  ### distillation methods
  add_method(distilled_causal_forest_stability_pruned_method) |>
  # add_method(distilled_rboost_stability_pruned_method) |>
  add_method(distilled_bcf_stability_pruned_method) |>
  ### baseline causal tree methods
  add_method(causal_tree_stability_pruned_method) |>
  ### other existing methods
  add_method(virtual_twins_method) |>
  add_method(lm_method) |>
  add_method(lasso_method) |>
  ### evaluators
  add_evaluator(subgroup_feature_selection_errors_summary) |>
  add_evaluator(subgroup_feature_selection_errors_max_depth2_summary) |>
  add_evaluator(subgroup_thresholds_summary) |>
  add_evaluator(subgroup_thresholds_max_depth2_summary) |>
  add_evaluator(subgroup_threshold_dist_summary) |>
  add_evaluator(subgroup_threshold_dist_max_depth2_summary) |>
  add_evaluator(subgroup_ate_err_summary) |>
  add_evaluator(subgroup_ate_err_max_depth2_summary) |>
  ### visualizers
  add_visualizer(subgroup_feature_selection_err_plot) |>
  add_visualizer(subgroup_thresholds_plot) |>
  add_visualizer(subgroup_threshold_dist_plot) |>
  add_visualizer(subgroup_ates_err_plot) |>
  add_visualizer(subgroup_nsplits_plot) |>
  add_visualizer(subgroup_stability_plot) |>
  add_visualizer(subgroup_feature_selection_err_max_depth2_plot) |>
  add_visualizer(subgroup_thresholds_max_depth2_plot) |>
  add_visualizer(subgroup_threshold_dist_max_depth2_plot) |>
  add_visualizer(subgroup_ates_err_max_depth2_plot) |>
  add_visualizer(subgroup_nsplits_max_depth2_plot)
  # add_visualizer(subgroup_ates_plot) |>

rulefit_experiment <- create_experiment(
  name = EXP_NAME, save_dir = file.path(SAVE_DIR, "results_bcf", EXP_NAME)
) |>
  ### distillation methods
  add_method(distilled_causal_forest_method) |>
  add_method(distilled_causal_forest_rulefit_v1_method) |>
  add_method(distilled_causal_forest_rulefit_v2_method) |>
  add_method(distilled_causal_forest_rulefit_v3_method) |>
  add_method(distilled_causal_forest_rulefit_v4_method) |>
  ### evaluators
  add_evaluator(subgroup_feature_selection_errors_summary) |>
  add_evaluator(subgroup_thresholds_summary) |>
  add_evaluator(subgroup_threshold_dist_summary) |>
  add_evaluator(subgroup_ate_err_summary) |>
  ### visualizers
  add_visualizer(subgroup_feature_selection_err_rulefit_plot) |>
  add_visualizer(subgroup_thresholds_rulefit_plot) |>
  add_visualizer(subgroup_threshold_dist_rulefit_plot) |>
  add_visualizer(subgroup_ates_err_rulefit_plot) |>
  add_visualizer(subgroup_nsplits_rulefit_plot)

crossfit_experiment <- create_experiment(
  name = EXP_NAME, save_dir = file.path(SAVE_DIR, "results_bcf", EXP_NAME)
) |>
  ### distillation methods without crossfit
  add_method(distilled_rboost_no_crossfit_method) |>
  ### distillation methods with crossfitting
  add_method(distilled_rboost_crossfit_method) |>
  add_vary_across(
    .method = distilled_rboost_crossfit_method$name,
    nreps_crossfit = c(1, 5, 10, 20, 50, 100)
  ) |>
  ### evaluators
  add_evaluator(subgroup_feature_selection_errors_summary) |>
  add_evaluator(subgroup_thresholds_summary) |>
  add_evaluator(subgroup_threshold_dist_summary) |>
  add_evaluator(subgroup_ate_err_summary) |>
  ### visualizers
  add_visualizer(subgroup_feature_selection_err_crossfit_plot) |>
  add_visualizer(subgroup_threshold_dist_crossfit_plot) |>
  add_visualizer(subgroup_ates_err_crossfit_plot)

rwd_experiment <- create_experiment(
  name = EXP_NAME, save_dir = file.path(SAVE_DIR, "results_bcf", EXP_NAME)
) |>
  ### distillation methods
  add_method(distilled_causal_forest_stability_pruned_method) |>
  add_method(distilled_rboost_stability_pruned_method) |>
  add_method(distilled_bcf_stability_pruned_method) |>
  ### baseline causal tree methods
  add_method(causal_tree_stability_pruned_method) |>
  ### other existing methods
  add_method(virtual_twins_method) |>
  add_method(lm_method) |>
  add_method(lasso_method) |>
  ### evaluators
  add_evaluator(subgroup_thresholds_summary) |>
  ### visualizers
  add_visualizer(subgroup_thresholds_plot) |>
  add_visualizer(subgroup_nsplits_plot) |>
  # add_visualizer(subgroup_ates_plot) |>
  add_visualizer(subgroup_stability_plot)

example_experiment <- create_experiment(
  name = EXP_NAME, save_dir = file.path(SAVE_DIR, "results_bcf", EXP_NAME)
) |>
  add_method(distilled_causal_forest_method) |>
  add_method(distilled_bcf_method) |>
  add_method(causal_tree_method) |>
  add_evaluator(subgroup_feature_selection_errors_summary) |>
  add_visualizer(num_subgroups_plot)
