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

#### Subgroup ATE Evaluators ####
subgroup_ate_err_summary <- create_evaluator(
  .eval_fun = eval_subgroup_ate_err,
  .name = "Subgroup ATE Errors Summary",
  custom_summary_funs = list(
    "se_ate_err" = function(x) sd(x, na.rm = TRUE) / sqrt(sum(!is.na(x)))
  )
)

subgroup_ate_err_max_depth2_summary <- create_evaluator(
  .eval_fun = eval_subgroup_ate_err,
  .name = "Subgroup ATE Errors Summary (max depth = 2)",
  max_depth = 2,
  custom_summary_funs = list(
    "se_ate_err" = function(x) sd(x, na.rm = TRUE) / sqrt(sum(!is.na(x)))
  )
)

subgroup_ate_err_eval <- create_evaluator(
  .eval_fun = eval_subgroup_ate_err,
  .name = "Subgroup ATE Errors",
  summary_funs = NULL
)
