rm(list = ls())
EXP_NAME <- "Varying nrep crossfits"
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
dgp <- gaussian_X_unbiased_Z_or
print(dgp$name)

source(here::here(file.path("meals", "shared_experiments.R")))
crossfit_experiment <- crossfit_experiment |>
  add_dgp(dgp) |>
  add_vary_across(
    .dgp = dgp$name,
    tau_heritability = c(0.2, 0.4, 0.6, 0.8, 1)
  )
# out <- run_experiment(crossfit_experiment)
out <- run_experiment(
  crossfit_experiment, n_reps = N_REPS, save = SAVE,
  use_cached = USE_CACHED, checkpoint_n_reps = CHECKPOINT_N_REPS,
  future.globals = FUTURE_GLOBALS, future.packages = FUTURE_PACKAGES
)

render_docs(save_dir = file.path(crossfit_experiment$get_save_dir(), dgp$name))

