rm(list = ls())
EXP_NAME <- "Rulefit Simulations"
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
# n_workers <- 2
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
  rulefit_experiment <- rulefit_experiment |>
    add_dgp(dgp) |>
    add_vary_across(
      .dgp = dgp$name,
      tau_heritability = c(0.2, 0.4, 0.6, 0.8, 1)
    )
  fit_results <- fit_experiment(
    rulefit_experiment, n_reps = N_REPS, save = SAVE,
    use_cached = USE_CACHED, checkpoint_n_reps = CHECKPOINT_N_REPS,
    future.globals = FUTURE_GLOBALS, future.packages = FUTURE_PACKAGES
  )
  eval_results <- evaluate_experiment(
    rulefit_experiment, fit_results, save = SAVE, use_cached = USE_CACHED
  )
}

render_docs(save_dir = file.path(rulefit_experiment$get_save_dir(), dgp$name))

