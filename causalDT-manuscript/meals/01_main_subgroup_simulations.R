rm(list = ls())
EXP_NAME <- "Main Simulations"
set.seed(331)
here::i_am("meals/01_main_subgroup_simulations.R")
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
    tau_heritability = c(0.2, 0.4, 0.6, 0.8, 1)
  )

out <- run_experiment(
  experiment, n_reps = N_REPS, save = SAVE,
  use_cached = USE_CACHED, checkpoint_n_reps = CHECKPOINT_N_REPS,
  future.globals = FUTURE_GLOBALS, future.packages = FUTURE_PACKAGES
)
