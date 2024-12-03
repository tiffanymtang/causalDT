rm(list = ls())
EXP_NAME <- "Rulefit Simulations"
N_REPS <- 100
SAVE <- TRUE
# USE_CACHED <- FALSE
USE_CACHED <- TRUE
CHECKPOINT_N_REPS <- 10
set.seed(331)

source(here::here(file.path("meals", "setup.R")))
N_REPS <- 100

#### DGPs ####

source(here::here(file.path("meals", "shared_dgps.R")))

#### Methods ####

source(here::here(file.path("meals", "shared_methods.R")))

#### Evaluators and Visualizers ####

source(here::here(file.path("meals", "shared_evaluators.R")))
source(here::here(file.path("meals", "shared_visualizers.R")))

#### Run Experiment ####
dgp <- gaussian_X_unbiased_Z_and
print(dgp$name)

source(here::here(file.path("meals", "shared_experiments.R")))
rulefit_experiment <- rulefit_experiment |>
  add_dgp(dgp) |>
  add_vary_across(
    .dgp = dgp$name,
    tau_heritability = c(0.2, 0.4, 0.6, 0.8, 1)
  )
# out <- run_experiment(rulefit_experiment)
out <- run_experiment(
  rulefit_experiment, n_reps = N_REPS, save = SAVE,
  use_cached = USE_CACHED, checkpoint_n_reps = CHECKPOINT_N_REPS,
  future.globals = FUTURE_GLOBALS, future.packages = FUTURE_PACKAGES
)


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
  h2o::h2o.init()
  fit_results <- fit_experiment(
    rulefit_experiment, n_reps = N_REPS, save = SAVE,
    use_cached = USE_CACHED, checkpoint_n_reps = CHECKPOINT_N_REPS,
    future.globals = FUTURE_GLOBALS, future.packages = FUTURE_PACKAGES
  )
  eval_results <- evaluate_experiment(
    rulefit_experiment, fit_results, save = SAVE, use_cached = USE_CACHED
  )
  h2o::h2o.shutdown(prompt = FALSE)
}


fit_results_orig <- get_cached_results(experiment, "fit")
nontree_fit_results <- fit_results_orig |>
  dplyr::filter(
    !stringr::str_detect(.method_name, "Distilled") &
      !stringr::str_detect(.method_name, "Causal Tree")
  )
tree_fit_results <- fit_results_orig |>
  dplyr::filter(
    stringr::str_detect(.method_name, "Distilled") |
      stringr::str_detect(.method_name, "Causal Tree")
  ) |>
  dplyr::mutate(
    model_info = purrr::map2(
      model_info, .method_name,
      ~ if (is.data.frame(.x)) {
        if (stringr::str_detect(.y, "Causal Tree")) {
          list(
            "none" = .x,
            `0.01` = NULL,
            min = NULL,
            `1se` = NULL
          )
        } else {
          list(
            "none" = .x,
            min = NULL,
            `1se` = NULL
          )
        }
      } else {
        .x
      }
    )
  ) |>
  tidyr::unnest_longer(
    c(fit, model_info, subgroups, group_cates)
  ) |>
  dplyr::mutate(
    .method_name = sprintf("%s (%s-pruned)", .method_name, fit_id)
  )
all.equal(tree_fit_results$fit_id, tree_fit_results$model_info_id)
all.equal(tree_fit_results$fit_id, tree_fit_results$subgroups_id)
all.equal(tree_fit_results$fit_id, tree_fit_results$group_cates_id)

fit_results <- dplyr::bind_rows(tree_fit_results, nontree_fit_results)
# eval_results <- get_cached_results(experiment, "eval")
eval_results <- evaluate_experiment(experiment, fit_results)
viz_results <- visualize_experiment(experiment, fit_results, eval_results)

eval_results <- evaluate_experiment(experiment, fit_results)
# eval_results <- get_cached_results(experiment, "eval")
viz_results <- visualize_experiment(experiment, fit_results, eval_results)
plotly::ggplotly(viz_results$`Subgroup Feature Selection Errors Plot`)
plotly::ggplotly(viz_results$`Subgroup CATE Errors Plot`)
viz_results$`Subgroup Thresholds Distribution Plot`
viz_results$`Number of Splits Plot`
viz_results$`Number of Trees Plot`
viz_results$`Subgroup CATEs Plot`
viz_results$`Subgroup Stability Diagnostics Plot`
viz_results$`Subgroup Stability Diagnostics Plot`[[3]]

render_docs(save_dir = file.path(experiment$get_save_dir(), dgp$name))

