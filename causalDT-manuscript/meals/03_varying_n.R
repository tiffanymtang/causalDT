rm(list = ls())
EXP_NAME <- "Main Simulations"
N_REPS <- 100
SAVE <- TRUE
# USE_CACHED <- FALSE
USE_CACHED <- TRUE
CHECKPOINT_N_REPS <- 0
set.seed(331)

source(here::here(file.path("meals", "setup.R")))
N_REPS <- 100

#### Cluster setup for parallelization (or comment out) ####
# n_workers <- min(N_REPS, availableCores() - 1)
n_workers <- 8
plan(multisession, workers = n_workers)

#### DGPs ####

source(here::here(file.path("meals", "shared_dgps.R")))

#### Methods ####

source(here::here(file.path("meals", "shared_methods.R")))

#### Evaluators and Visualizers ####

source(here::here(file.path("meals", "shared_evaluators.R")))
source(here::here(file.path("meals", "shared_visualizers.R")))

#### Run Experiment ####
dgps <- list(
  gaussian_X_unbiased_Z_and,
  gaussian_X_unbiased_Z_or,
  gaussian_X_unbiased_Z_additive,
  gaussian_X_unbiased_Z_and_cov,
  gaussian_X_unbiased_Z_or_cov,
  gaussian_X_unbiased_Z_additive_cov
)
for (dgp in dgps) {
  print(dgp$name)
  source(here::here(file.path("meals", "shared_experiments.R")))
  example_experiment <- example_experiment |>
    add_dgp(dgp) |>
    add_vary_across(
      .dgp = dgp$name,
      n = c(100, 500, 1000, 2000)
    )
  # out <- run_experiment(example_experiment)
  out <- run_experiment(
    example_experiment, n_reps = N_REPS, save = SAVE,
    use_cached = USE_CACHED, checkpoint_n_reps = CHECKPOINT_N_REPS,
    future.globals = FUTURE_GLOBALS, future.packages = FUTURE_PACKAGES
  )
  export_visualizers(example_experiment)
}
