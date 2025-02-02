rm(list = ls())
EXP_NAME <- "AIDS"
N_REPS <- 100
SAVE <- c("fit", "eval")
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
dgp <- aids_dgp
print(dgp$name)

source(here::here(file.path("meals", "shared_experiments.R")))

rwd_experiment <- rwd_experiment |>
  add_dgp(dgp)
# out <- run_experiment(rwd_experiment)
out <- run_experiment(
  rwd_experiment, n_reps = N_REPS, save = SAVE,
  use_cached = USE_CACHED, checkpoint_n_reps = CHECKPOINT_N_REPS,
  future.globals = FUTURE_GLOBALS, future.packages = FUTURE_PACKAGES
)

render_docs(rwd_experiment)

