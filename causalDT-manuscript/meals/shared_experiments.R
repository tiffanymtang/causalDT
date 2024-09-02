experiment <- create_experiment(
  name = EXP_NAME, save_dir = file.path(SAVE_DIR, "results", EXP_NAME)
) |>
  ### distillation methods
  add_method(distilled_causal_forest_stability_method) |>
  add_method(distilled_rboost_stability_method) |>
  add_method(distilled_rlasso_stability_method) |>
  ### baseline causal tree methods
  add_method(causal_tree_stability_method) |>
  add_method(causal_tree_unpruned_stability_method) |>
  ### other existing methods
  add_method(virtual_twins_method) |>
  add_method(lm_method) |>
  add_method(lasso_method)
