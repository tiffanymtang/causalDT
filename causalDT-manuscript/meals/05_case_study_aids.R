rm(list = ls())
EXP_NAME <- "AIDS"
N_REPS <- 100
SAVE <- TRUE
USE_CACHED <- FALSE
# USE_CACHED <- TRUE
CHECKPOINT_N_REPS <- 0
set.seed(331)

source(here::here(file.path("meals", "setup.R")))
N_REPS <- 100

# #### Cluster setup for parallelization (or comment out) ####
# # n_workers <- min(N_REPS, availableCores() - 1)
# n_workers <- 8
# plan(multisession, workers = n_workers)

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
  add_dgp(dgp) #|>
  # add_vary_across(
  #   .dgp = dgp$name,
  #   subsample = c(100, 200, 300, 400, 500, 600, 792) / 792
  # )
# out <- run_experiment(rwd_experiment)
out <- run_experiment(
  rwd_experiment, n_reps = N_REPS, save = SAVE,
  use_cached = USE_CACHED, checkpoint_n_reps = CHECKPOINT_N_REPS,
  future.globals = FUTURE_GLOBALS, future.packages = FUTURE_PACKAGES
)

keep_methods <- c(
  "Causal Tree (pruned)",
  "Causal Tree (unpruned)",
  "Distilled Causal Forest",
  "Distilled Rboost",
  "Distilled Rlasso",
  "Lasso",
  "Linear Regression",
  "Virtual Twins"
)
fit_results <- get_cached_results(rwd_experiment, "fit") |>
  dplyr::filter(
    .method_name %in% !!keep_methods
  )
# eval_results <- get_cached_results(rwd_experiment, "eval")
eval_results <- evaluate_experiment(rwd_experiment, fit_results)
# eval_results <- evaluate_experiment(rwd_experiment, fit_results, save = TRUE)
viz_results <- visualize_experiment(rwd_experiment, fit_results, eval_results)
# viz_results <- visualize_experiment(rwd_experiment, fit_results, eval_results, save = TRUE)

viz_results$`Subgroup Thresholds Distribution Plot`
viz_results$`Number of Splits Plot`
viz_results$`Number of Trees Plot`
viz_results$`Subgroup CATEs Plot`
viz_results$`Subgroup Stability Diagnostics Plot`
plotly::ggplotly(
  viz_results$`Subgroup Stability Diagnostics Plot`[[1]]
)

plt1 <- viz_results$`Subgroup Thresholds Distribution Plot`

plt <- dplyr::bind_rows(
  viz_results$`Number of Trees Plot`$data |>
    dplyr::mutate(
      .metric = "Selection Stability"
    ) |>
    dplyr::rename(
      .estimate = n_trees
    ),
  viz_results$`Number of Splits Plot`$data |>
    dplyr::mutate(
      .metric = "Average # of Splits per Tree"
    ) |>
    dplyr::rename(
      .estimate = n_splits_per_tree
    )
) |>
  dplyr::mutate(
    .metric = factor(
      .metric,
      levels = c("Selection Stability", "Average # of Splits per Tree")
    )
  ) |>
  dplyr::mutate(
    .color = dplyr::case_when(
      .method_name %in% c("Causal Tree (pruned)", "Causal Tree (unpruned)") ~ "Causal Tree",
      .method_name %in% c("Distilled Causal Forest", "Distilled Rboost", "Distilled Rlasso") ~ "Distilled",
      .method_name %in% c("Lasso", "Linear Regression") ~ "Linear",
      .method_name %in% c("Virtual Twins") ~ "Virtual Twins"
    ) |>
      factor(
        levels = c("Causal Tree", "Distilled", "Linear", "Virtual Twins")
      )
  ) |>
  ggplot2::ggplot() +
  ggplot2::geom_bar(
    ggplot2::aes(
      x = .method_name,
      y = .estimate,
      fill = .color
    ),
    stat = "identity",
    position = "dodge"
  ) +
  ggplot2::facet_grid(.metric ~ .var, scales = "free_y", switch = "y") +
  ggplot2::labs(
    x = "Variable", y = "", fill = "Method"
  ) +
  vthemes::scale_fill_vmodern(discrete = TRUE) +
  vthemes::theme_vmodern() +
  ggplot2::theme(
    strip.placement = "outside",
    strip.background.y = ggplot2::element_rect(
      fill = "transparent", color = "transparent"
    ),
    strip.text.y = ggplot2::element_text(
      color = "black", face = "bold", size = 16
    ),
    axis.text.x = ggplot2::element_text(
      angle = 90, hjust = 1, vjust = 0.5
    )
  )

render_docs(save_dir = file.path(experiment$get_save_dir(), dgp$name))

