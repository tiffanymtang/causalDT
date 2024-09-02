library(simChef)
library(future)
library(causalTree)  # needed otherwise will receive rpart.control missing error

options(simChef.plot_theme = "vthemes")

for (obj in c("dgp", "method", "eval", "viz")) {
  for (f in list.files(file.path(here::here("R", obj)), pattern = "*.R")) {
    source(here::here(file.path("R", obj, f)))
  }
}

cat(sprintf("Experiment Name: %s\n", EXP_NAME))

n_cores <- Sys.getenv("SLURM_CPUS_PER_TASK")
if (n_cores != "") {
  n_cores <- as.integer(n_cores)
  print(n_cores)
  if (n_cores > 1) {
    plan(multicore, workers = n_cores)
    # plan(multisession, workers = n_cores)
    N_REPS <- n_cores
  }
}

SAVE_DIR <- here::here()
cat(sprintf("Saving results to: %s\n", SAVE_DIR))

FUTURE_GLOBALS <- c(
  "dummy_code", "causaltree_wrapper", "get_interaction_formula",
  "tidy_lm", "tidy_glmnet", "get_lm_info", "get_lm_subgroups"
)
FUTURE_PACKAGES <- c("causalDT", "causalTree")
