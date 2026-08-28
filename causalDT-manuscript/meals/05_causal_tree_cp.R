rm(list = ls())
EXP_NAME <- "Causal Tree Simulations"
here::i_am("meals/05_causal_tree_cp.R")
source(here::here(file.path("meals", "setup.R")))

#### DGPs ####

source(here::here(file.path("meals", "shared_dgps.R")))

#### Methods ####

source(here::here(file.path("meals", "shared_method_params.R")))
source(here::here(file.path("meals", "shared_methods.R")))

#### Evaluators and Visualizers ####

source(here::here(file.path("meals", "shared_evaluators.R")))
source(here::here(file.path("meals", "shared_visualizers.R")))

#### Run Experiment ####
print(dgp$name)

set.seed(331)

causal_tree_method <- create_method(
  .method_fun = causal_tree,
  .name = "Causal Tree",
  prune = "min",
  causaltree_args = list(
    split.Rule = "CT",
    cv.option = "CT",
    split.Honest = TRUE,
    cv.Honest = TRUE,
    split.Bucket = FALSE,
    xval = 10,
    cp = CP,
    minsize = 20,
    propensity = 0.5
  )
)
causal_tree_unpruned_method <- create_method(
  .method_fun = causal_tree,
  .name = "Causal Tree (unpruned)",
  prune = "none",
  causaltree_args =  list(
    split.Rule = "CT",
    cv.option = "CT",
    split.Honest = TRUE,
    cv.Honest = TRUE,
    split.Bucket = FALSE,
    xval = 10,
    cp = CP,
    minsize = 20,
    propensity = 0.5
  )
)

experiment <- create_experiment(
  name = EXP_NAME,
  save_dir = file.path(SAVE_DIR, "results", sprintf("%s (cp=%s)", EXP_NAME, CP))
) |>
  add_dgp(dgp) |>
  add_method(causal_tree_method) |>
  add_method(causal_tree_unpruned_method) |>
  add_method(distilled_causal_forest_method) |>
  add_evaluator(subgroup_feature_selection_errors_summary) |>
  add_evaluator(subgroup_feature_selection_errors_max_depth2_summary) |>
  add_evaluator(subgroup_thresholds_summary) |>
  add_evaluator(subgroup_thresholds_max_depth2_summary) |>
  add_evaluator(subgroup_threshold_dist_summary) |>
  add_evaluator(subgroup_threshold_dist_max_depth2_summary) |>
  add_evaluator(subgroup_ate_err_summary) |>
  add_evaluator(subgroup_ate_err_max_depth2_summary) |>
  add_vary_across(
    .dgp = dgp$name,
    n = c(100, 500, 1000, 2000)
  )

out <- run_experiment(
  experiment, n_reps = N_REPS, save = SAVE,
  use_cached = USE_CACHED, checkpoint_n_reps = CHECKPOINT_N_REPS,
  future.globals = FUTURE_GLOBALS, future.packages = FUTURE_PACKAGES
)
