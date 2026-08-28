rm(list = ls())
EXP_NAME <- "Main Simulations"
set.seed(331)
here::i_am("meals/02_vary_n.R")
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
example_experiment <- example_experiment |>
  add_dgp(dgp) |>
  add_vary_across(
    .dgp = dgp$name,
    n = c(100, 500, 1000, 2000, 3000)
  )

out <- run_experiment(
  example_experiment, n_reps = N_REPS, save = SAVE,
  use_cached = USE_CACHED, checkpoint_n_reps = CHECKPOINT_N_REPS,
  future.globals = FUTURE_GLOBALS, future.packages = FUTURE_PACKAGES
)
