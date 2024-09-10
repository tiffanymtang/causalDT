#### Subgroup Feature Selection Evaluators ####
subgroup_feature_selection_errors_summary <- create_evaluator(
  .eval_fun = eval_subgroup_feature_selection_err,
  .name = "Subgroup Feature Selection Errors Summary"
)

subgroup_feature_selection_errors_eval <- create_evaluator(
  .eval_fun = eval_subgroup_feature_selection_err,
  .name = "Subgroup Feature Selection Errors",
  summary_funs = NULL
)

#### Subgroup Threshold Evaluators ####
subgroup_thresholds_summary <- create_evaluator(
  .eval_fun = eval_subgroup_thresholds,
  .name = "Thresholds Summary"
)

subgroup_thresholds_eval <- create_evaluator(
  .eval_fun = eval_subgroup_thresholds,
  .name = "Thresholds",
  summary_funs = NULL
)

#### Subgroup CATE Evaluators ####
subgroup_cate_err_summary <- create_evaluator(
  .eval_fun = eval_subgroup_cate_err,
  .name = "Subgroup CATE Errors Summary",
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
subgroup_stability_eval <- create_evaluator(
  .eval_fun = eval_stability_diagnostics,
  .name = "Subgroup Stability Diagnostics"
)
