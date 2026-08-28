library(simChef)
library(future)
library(causalTree)  # needed otherwise will receive rpart.control missing error
library(optparse)

options(simChef.plot_theme = "vthemes")

simChef::load_all()

cat(sprintf("Experiment Name: %s\n", EXP_NAME))

# command line arguments
option_list <- list(
  make_option(
    "--dgp", type = "character", default = "and",
    help = "Data generating process to use for the experiment [default %default]"
  ),
  make_option(
    "--nsamples", type = "integer", default = 500,
    help = "Number of total samples in simulated data [default %default]"
  ),
  make_option(
    "--nreps", type = "integer", default = 1,
    help = "Number of repetitions for the experiment [default %default]"
  ),
  make_option(
    "--cp", type = "double", default = 0.0,
    help = "cp parameter in Causal Tree [default %default]"
  ),
  make_option(
    "--save", action = "store_true", default = FALSE,
    help = "Whether to save the experiment results"
  ),
  make_option(
    "--use_cached", action = "store_true", default = FALSE,
    help = "Whether to use cached results"
  ),
  make_option(
    "--checkpoint_n_reps", type = "integer", default = 0,
    help = "Number of repetitions between checkpoints [default %default]"
  )
)
# parse the command line options
opt_parser <- OptionParser(option_list = option_list)
opt <- parse_args(opt_parser)
str(opt)
N_REPS <- opt$nreps
USE_CACHED <- opt$use_cached
CHECKPOINT_N_REPS <- opt$checkpoint_n_reps
CP <- opt$cp
N_SAMPLES <- opt$nsamples
if (N_SAMPLES != 500) {
  EXP_NAME <- sprintf("%s (n=%s)", EXP_NAME, N_SAMPLES)
}
if (opt$save) {
  SAVE <- c("fit", "eval")
} else {
  SAVE <- FALSE
}

source(here::here(file.path("meals", "shared_dgps.R")))
dgp <- switch(
  opt$dgp,
  and = gaussian_X_unbiased_Z_and,
  or = gaussian_X_unbiased_Z_or,
  additive = gaussian_X_unbiased_Z_additive,
  and_cov = gaussian_X_unbiased_Z_and_cov,
  or_cov = gaussian_X_unbiased_Z_or_cov,
  additive_cov = gaussian_X_unbiased_Z_additive_cov,
  and_cor_cov = correlated_X_unbiased_Z_and_cov,
  or_cor_cov = correlated_X_unbiased_Z_or_cov,
  additive_cor_cov = correlated_X_unbiased_Z_additive_cov,
  simple = gaussian_X_unbiased_Z_simple,
  stop("Invalid dgp option")
)

n_cores <- Sys.getenv("NSLOTS")
# n_cores <- Sys.getenv("SLURM_CPUS_PER_TASK")
if (n_cores != "") {
  n_cores <- as.integer(n_cores)
  print(n_cores)
  if (n_cores > 1) {
    plan(multicore, workers = n_cores)
    # plan(multisession, workers = n_cores)
  }
}

SAVE_DIR <- here::here()
cat(sprintf("Saving results to: %s\n", SAVE_DIR))

FUTURE_GLOBALS <- c(
  "dummy_code", "causaltree_wrapper", "get_interaction_formula",
  "tidy_lm", "tidy_glmnet", "get_lm_info", "get_lm_subgroups",
  "student_rulefit", "get_rulefit_str", "get_rulefit_subgroups",
  "clean_pre_rules"
)
FUTURE_PACKAGES <- c("causalDT", "causalTree")
