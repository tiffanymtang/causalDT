rm(list = ls())
EXP_NAME <- "Correlated Simulations"
set.seed(331)
here::i_am("meals/03_vary_correlation.R")
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

source(here::here(file.path("meals", "shared_experiments.R")))
experiment <- experiment |>
  add_dgp(dgp) |>
  add_vary_across(
    .dgp = dgp$name,
    .X_corr = c(0, 0.1, 0.3, 0.5, 0.7, 0.9)
  )

out <- run_experiment(
  experiment, n_reps = N_REPS, save = SAVE,
  use_cached = USE_CACHED, checkpoint_n_reps = CHECKPOINT_N_REPS,
  future.globals = FUTURE_GLOBALS, future.packages = FUTURE_PACKAGES
)
