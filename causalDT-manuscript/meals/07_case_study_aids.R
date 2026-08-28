rm(list = ls())
EXP_NAME <- "AIDS"
set.seed(4194)
here::i_am("meals/07_case_study_aids.R")
source(here::here(file.path("meals", "setup.R")))

#### DGPs ####

source(here::here(file.path("meals", "shared_dgps.R")))
dgp <- aids_dgp
print(dgp$name)

#### Methods ####

source(here::here(file.path("meals", "shared_method_params.R")))
source(here::here(file.path("meals", "shared_methods.R")))
distilled_causal_forest_stability_pruned_method$method_params$holdout_prop <- 0.5
distilled_rboost_stability_pruned_method$method_params$holdout_prop <- 0.5
lm_method$method_params$train_prop <- 0.5
lasso_method$method_params$train_prop <- 0.5

#### Evaluators and Visualizers ####

source(here::here(file.path("meals", "shared_evaluators.R")))
source(here::here(file.path("meals", "shared_visualizers.R")))

#### Run Case Study ####
EXP_NAME <- "AIDS"
source(here::here(file.path("meals", "shared_experiments.R")))
rwd_experiment <- rwd_experiment |>
  add_dgp(dgp)
out <- run_experiment(
  rwd_experiment, n_reps = 1, save = SAVE,
  use_cached = USE_CACHED, checkpoint_n_reps = CHECKPOINT_N_REPS,
  future.globals = FUTURE_GLOBALS, future.packages = FUTURE_PACKAGES
)

#### Run Stability Experiment ####
EXP_NAME <- "AIDS (stability)"
source(here::here(file.path("meals", "shared_experiments.R")))
rwd_experiment <- rwd_experiment |>
  add_dgp(dgp)
out <- run_experiment(
  rwd_experiment, n_reps = N_REPS, save = SAVE,
  use_cached = USE_CACHED, checkpoint_n_reps = CHECKPOINT_N_REPS,
  future.globals = FUTURE_GLOBALS, future.packages = FUTURE_PACKAGES
)

#### Compute ATE ####
data_full <- rwd_dgp("aids_small")
estimatr::lm_robust(data_full$Y ~ data_full$Z)

# Note: all treatment effects have flipped signs due to reverse coding of Y's
