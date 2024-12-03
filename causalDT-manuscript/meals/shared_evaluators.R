#### Subgroup Feature Selection Evaluators ####
subgroup_feature_selection_errors_summary <- create_evaluator(
  .eval_fun = eval_subgroup_feature_selection_err,
  .name = "Subgroup Feature Selection Errors Summary"
)

subgroup_feature_selection_errors_max_depth2_summary <- create_evaluator(
  .eval_fun = eval_subgroup_feature_selection_err,
  .name = "Subgroup Feature Selection Errors Summary (max depth = 2)",
  max_depth = 2
)
subgroup_feature_selection_errors_max_depth3_summary <- create_evaluator(
  .eval_fun = eval_subgroup_feature_selection_err,
  .name = "Subgroup Feature Selection Errors Summary (max depth = 3)",
  max_depth = 3
)
subgroup_feature_selection_errors_max_depth4_summary <- create_evaluator(
  .eval_fun = eval_subgroup_feature_selection_err,
  .name = "Subgroup Feature Selection Errors Summary (max depth = 4)",
  max_depth = 4
)

subgroup_feature_selection_errors_eval <- create_evaluator(
  .eval_fun = eval_subgroup_feature_selection_err,
  .name = "Subgroup Feature Selection Errors",
  summary_funs = NULL
)

#### Subgroup Threshold Evaluators ####
## threshold distribution
subgroup_thresholds_summary <- create_evaluator(
  .eval_fun = eval_subgroup_thresholds,
  .name = "Thresholds Summary"
)

subgroup_thresholds_max_depth2_summary <- create_evaluator(
  .eval_fun = eval_subgroup_thresholds,
  .name = "Thresholds Summary (max depth = 2)",
  max_depth = 2
)
subgroup_thresholds_max_depth3_summary <- create_evaluator(
  .eval_fun = eval_subgroup_thresholds,
  .name = "Thresholds Summary (max depth = 3)",
  max_depth = 3
)
subgroup_thresholds_max_depth4_summary <- create_evaluator(
  .eval_fun = eval_subgroup_thresholds,
  .name = "Thresholds Summary (max depth = 4)",
  max_depth = 4
)

subgroup_thresholds_eval <- create_evaluator(
  .eval_fun = eval_subgroup_thresholds,
  .name = "Thresholds",
  summary_funs = NULL
)

# threshold distances
subgroup_threshold_dist_summary <- create_evaluator(
  .eval_fun = eval_subgroup_threshold_dist,
  .name = "Threshold Distances Summary"
)

subgroup_threshold_dist_max_depth2_summary <- create_evaluator(
  .eval_fun = eval_subgroup_threshold_dist,
  .name = "Threshold Distances Summary (max depth = 2)",
  max_depth = 2
)
subgroup_threshold_dist_max_depth3_summary <- create_evaluator(
  .eval_fun = eval_subgroup_threshold_dist,
  .name = "Threshold Distances Summary (max depth = 3)",
  max_depth = 3
)
subgroup_threshold_dist_max_depth4_summary <- create_evaluator(
  .eval_fun = eval_subgroup_threshold_dist,
  .name = "Threshold Distances Summary (max depth = 4)",
  max_depth = 4
)

#### Subgroup CATE Evaluators ####
subgroup_cate_err_summary <- create_evaluator(
  .eval_fun = eval_subgroup_cate_err,
  .name = "Subgroup CATE Errors Summary",
  custom_summary_funs = list(
    "se_cate_err" = function(x) sd(x, na.rm = TRUE) / sqrt(sum(!is.na(x)))
  )
)

subgroup_cate_err_max_depth2_summary <- create_evaluator(
  .eval_fun = eval_subgroup_cate_err,
  .name = "Subgroup CATE Errors Summary (max depth = 2)",
  max_depth = 2,
  custom_summary_funs = list(
    "se_cate_err" = function(x) sd(x, na.rm = TRUE) / sqrt(sum(!is.na(x)))
  )
)

subgroup_cate_err_eval <- create_evaluator(
  .eval_fun = eval_subgroup_cate_err,
  .name = "Subgroup CATE Errors",
  summary_funs = NULL
)

#### Stability Diagnostic Evaluators ####
# subgroup_stability_eval <- create_evaluator(
#   .eval_fun = eval_stability_diagnostics,
#   .name = "Subgroup Stability Diagnostics"
# )
