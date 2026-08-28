rm(list = ls())
EXP_NAME <- "Timing Simulations"
set.seed(331)
here::i_am("meals/08_timing_simulations.R")
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
dgp <- gaussian_X_unbiased_Z_additive_cov

max_depth_stability <- 4
rpart_control <- NULL
student_model <- purrr::partial(
  causalDT::student_rpart,
  rpart_control = rpart_control,
  prune = "min"
)
student_stability_model <- causalDT::student_rpart

method <- distilled_causal_forest_stability_method
method$method_params$max_depth_stability <- max_depth_stability
method$method_params$return_data <- TRUE

# time full CDT method
experiment <- create_experiment(
  name = EXP_NAME, save_dir = file.path(SAVE_DIR, "results", EXP_NAME)
) |>
  add_dgp(dgp) |>
  add_method(method) |>
  add_vary_across(
    .dgp = dgp$name,
    n = c(100, 500, 1000, 2000, 5000)
  )
out <- run_experiment(
  experiment, n_reps = N_REPS, save = SAVE,
  use_cached = USE_CACHED, checkpoint_n_reps = CHECKPOINT_N_REPS,
  future.globals = FUTURE_GLOBALS, future.packages = FUTURE_PACKAGES
)

# time only student model part
student_times <- c()
stability_times <- c()
jaccard_times <- c()
for (i in 1:nrow(out$fit_results)) {
  print(i)
  tauhat <- out$fit_results$teacher_predictions[[i]]
  holdout_idxs <- out$fit_results$holdout_idxs[[i]]
  X_train <- out$fit_results$X[[i]][-holdout_idxs, , drop = FALSE]
  n <- out$fit_results$n[[i]]

  # time student model
  tictoc::tic()
  student_fit_out <- student_model(X_train, tauhat)
  student_time <- tictoc::toc(quiet = TRUE)

  # time subgroup stability diagnostics
  tictoc::tic()
  stability_out <- causalDT::evaluate_subgroup_stability(
    estimator = student_stability_model,
    fit = student_fit_out$fit,
    X = X_train,
    y = tauhat,
    rpart_control = rpart_control,
    B = 100,
    max_depth = max_depth_stability
  )
  stability_time <- tictoc::toc(quiet = TRUE)

  # time jaccard SSI
  sample1 <- sample(1:max_depth_stability, size = n, replace = TRUE) - 1
  sample2 <- sample(1:max_depth_stability, size = n, replace = TRUE) - 1
  tictoc::tic()
  jaccard_out <- causalDT::jaccardSSI(sample1, sample2)
  jaccard_time <- tictoc::toc(quiet = TRUE)

  student_times[i] <- student_time$toc - student_time$tic
  stability_times[i] <- stability_time$toc - stability_time$tic
  jaccard_times[i] <- jaccard_time$toc - jaccard_time$tic
}
saveRDS(
  list(
    student_times = student_times,
    stability_times = stability_times,
    jaccard_times = jaccard_times
  ),
  file.path(experiment$get_save_dir(), dgp$name, "timing_results.rds")
)
