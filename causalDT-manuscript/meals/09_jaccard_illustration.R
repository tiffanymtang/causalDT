rm(list = ls())
EXP_NAME <- "Jaccard Illustration"
here::i_am("meals/09_jaccard_illustration.R")
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
methods <- list(
  "Distilled Causal Forest" = distilled_causal_forest_method,
  "Distilled BCF" = distilled_bcf_method,
  "Distilled Rlasso" = distilled_rlasso_method,
  "Distilled Rboost" = distilled_rboost_method,
  "Distilled Rspline" = distilled_rspline_method
)
train_prop <- 0.7
B <- 100

RESULTS_DIR <- file.path(SAVE_DIR, "Jaccard Illustration")
if (!dir.exists(RESULTS_DIR)) {
  dir.create(RESULTS_DIR, recursive = TRUE)
}

extract_primary_splits <- function(fit) {
  fr <- fit$frame
  sp <- fit$splits

  internal <- fr$var != "<leaf>"
  node_ids <- as.integer(row.names(fr))

  out <- list()
  split_row <- 1L

  for (i in seq_len(nrow(fr))) {
    if (fr$var[i] != "<leaf>") {
      out[[length(out) + 1L]] <- data.frame(
        node = node_ids[i],
        variable = rownames(sp)[split_row],
        threshold = sp[split_row, "index"],
        n = fr$n[i],
        stringsAsFactors = FALSE
      )

      split_row <- split_row + 1L + fr$ncompete[i] + fr$nsurrogate[i]
    }
  }

  do.call(rbind, out)
}

# fit results
n <- 500
dgp <- gaussian_X_unbiased_Z_simple
dgp_name <- "Simple"
for (tau_heritability in c(0.2, 0.6, 1)) {
  set.seed(331)
  print(sprintf("%s: n = %s, tau_heritability = %s", dgp_name, n, tau_heritability))
  dgp_out <- dgp$generate(n = n, tau_heritability = tau_heritability)

  # n <- nrow(dgp_out$X)
  holdout_idxs <- round(nrow(dgp_out$X) * train_prop):n
  X <- dgp_out$X[-holdout_idxs, , drop = FALSE]
  Y <- dgp_out$Y[-holdout_idxs]
  Z <- dgp_out$Z[-holdout_idxs]
  X_est <- dgp_out$X[holdout_idxs, , drop = FALSE]
  Y_est <- dgp_out$Y[holdout_idxs]
  Z_est <- dgp_out$Z[holdout_idxs]

  # fit distilled_causal_forest_method and distilled_rlasso_method on multiple bootstraps of tauhat
  method_out <- purrr::map(
    methods,
    ~ .x$fit(dgp_out, holdout_idxs = holdout_idxs, return_data = TRUE)
  )

  # compute subgroup ATE
  subgroup_ate_errs <- purrr::map(
    method_out,
    function(.x) {
      group_cates <- .x$group_cates[[1]]
      tauhat <- rep(NA, nrow(X_est))
      for (i in 1:nrow(group_cates)) {
        tauhat[group_cates$.sample_idxs[[i]]] <- group_cates$estimate[[i]]
      }
      return(mean((.x$tau[[1]] - tauhat)^2))
    }
  )

  # get tauhats for each method
  tauhats_df <- purrr::map(
    method_out,
    function(.x) {
      tauhat <- data.frame(
        tauhat = .x$teacher_predictions[[1]],
        tau = .x$tau[[1]][-.x$holdout_idxs[[1]]],
        x1 = .x$X[[1]][-.x$holdout_idxs[[1]], 1],
        x2 = .x$X[[1]][-.x$holdout_idxs[[1]], 2]
      )
    }
  ) |>
    dplyr::bind_rows(.id = ".method_name")

  # perform stability model selection
  out <- list()
  for (method_name in names(methods)) {
    fit_orig <- partykit::as.party(method_out[[method_name]]$fit[[1]])
    node_depths_orig <- causalDT:::get_party_node_depths(fit_orig)
    leaf_ids_orig <- predict(fit_orig, data.frame(X), type = "node")

    # modify rpart controls so that the tree is forced to make a split when possible
    rpart_control <- list(
      minsplit = 2,
      minbucket = 1,
      cp = 0,
      maxdepth = 2
    )
    estimator <- purrr::partial(causalDT::student_rpart, rpart_control = rpart_control)

    bootstrap_out <- purrr::map(
      1:(2 * B),
      function(b) {
        bootstrap_idx <- sample(1:nrow(X), size = nrow(X), replace = TRUE)
        X_b <- X[bootstrap_idx, , drop = FALSE]
        y_b <- method_out[[method_name]]$teacher_predictions[[1]][bootstrap_idx]
        fit_b <- estimator(X = X_b, y = y_b, fit_only = TRUE)
        if (!is.null(fit_b)) {
          node_depths_b <- causalDT:::get_party_node_depths(partykit::as.party(fit_b))
          splits_b <- extract_primary_splits(fit_b)
          return(
            list(
              "fit" = partykit::as.party(fit_b),
              "node_depths" = node_depths_b,
              "split_vars" = splits_b$variable,
              "split_thrs" = splits_b$threshold
            )
          )
        } else {
          return(NULL)
        }
      }
    ) |>
      purrr::compact()

    bootstrap_fits <- purrr::map(bootstrap_out, "fit")
    node_depths <- purrr::map(bootstrap_out, "node_depths")
    split_vars <- purrr::map(bootstrap_out, "split_vars")
    split_thrs <- purrr::map(bootstrap_out, "split_thrs")

    Js <- list()
    for (n_depth in 1) {
      bootstrap_leaf_ids <- purrr::map2(
        bootstrap_fits, node_depths,
        function(fit_b, node_depths_b) {
          if (any(node_depths_b > n_depth)) {
            fit_b_pruned <- partykit::nodeprune(
              fit_b, ids = names(node_depths_b)[node_depths_b == n_depth]
            )
          } else {
            fit_b_pruned <- fit_b
          }
          leaf_ids_b <- predict(fit_b_pruned, data.frame(X), type = "node")
          return(leaf_ids_b)
        }
      )

      J <- purrr::map_dbl(
        1:floor(length(bootstrap_fits) / 2),
        ~ causalDT::jaccardSSI(
          as.numeric(as.factor(bootstrap_leaf_ids[[.x * 2 - 1]])) - 1,
          as.numeric(as.factor(bootstrap_leaf_ids[[.x * 2]])) - 1
        )
      )
      Js[[n_depth]] <- J
    }

    out[[method_name]] <- list(
      "jaccard_mean" = sapply(Js, mean),
      "jaccard_distribution" = Js,
      "split_vars" = split_vars,
      "split_thrs" = split_thrs
    )
  }

  # get jaccard distribution at depth 1
  jaccard_dist_df <- purrr::map(
    out,
    function(.x) {
      data.frame(
        depth = 1,
        jaccard = .x$jaccard_distribution[[1]]
      )
    }
  ) |>
    dplyr::bind_rows(.id = ".method_name")

  # get jaccardSSI
  jaccard_df <- purrr::map(
    out,
    function(.x) {
      data.frame(
        depth = 1:length(.x$jaccard_mean),
        jaccardSSI = .x$jaccard_mean
      )
    }
  ) |>
    dplyr::bind_rows(.id = ".method_name")

  # get variable splits
  splits_df <- purrr::map(
    out,
    function(.x) {
      purrr::map2(
        .x$split_vars,
        .x$split_thrs,
        function(.var, .thr) {
          data.frame(
            depth = c(1, 2, 2)[1:length(.var)],
            var = .var,
            thr = .thr
          )
        }
      ) |>
        purrr::list_rbind()
    }
  ) |>
    dplyr::bind_rows(.id = ".method_name")
  nsplits <- splits_df |>
    dplyr::filter(depth == 1) |>
    dplyr::group_by(.method_name, var) |>
    dplyr::summarise(
      n = dplyr::n()
    )

  save(
    dgp_out, method_out, subgroup_ate_errs,
    jaccard_df, jaccard_dist_df, splits_df, nsplits, tauhats_df,
    file = file.path(
      RESULTS_DIR,
      sprintf("%s_n%s_heritability%s.Rdata", dgp_name, n, tau_heritability)
    )
  )
}

# load in results
n <- 500
dgp <- gaussian_X_unbiased_Z_simple
dgp_name <- "Simple"
KEEP_METHODS <- c(
  "Distilled Causal Forest",
  "Distilled Rboost",
  "Distilled Rspline",
  "Distilled BCF",
  "Distilled Rlasso"
)
COLORS <- c(
  "Distilled Rboost" = "#6aafe4",
  "Distilled Causal Forest" = "#1f5d8f",
  "Distilled BCF" = "#1c3145",
  "Distilled Rspline" = "#93ACBF",
  "Distilled Rlasso" = "#C8D7E3"
)
tauhats_df_ls <- list()
splits_df_ls <- list()
jaccard_df_ls <- list()
jaccard_dist_df_ls <- list()
for (tau_heritability in c(0.2, 0.6, 1)) {
  set.seed(331)
  print(sprintf("%s: n = %s, tau_heritability = %s", dgp_name, n, tau_heritability))
  load(
    # dgp_out, method_out, subgroup_ate_errs, jaccard_df, jaccard_dist_df, splits_df, nsplits, tauhats_df,
    file = file.path(
      RESULTS_DIR,
      sprintf("%s_n%s_heritability%s.Rdata", dgp_name, n, tau_heritability)
    )
  )
  tauhats_df_ls[[as.character(tau_heritability)]] <- tauhats_df |>
    dplyr::filter(.method_name %in% !!KEEP_METHODS)
  splits_df_ls[[as.character(tau_heritability)]] <- splits_df |>
    dplyr::filter(.method_name %in% !!KEEP_METHODS)
  jaccard_df_ls[[as.character(tau_heritability)]] <- jaccard_df |>
    dplyr::filter(.method_name %in% !!KEEP_METHODS)
  jaccard_dist_df_ls[[as.character(tau_heritability)]] <- jaccard_dist_df |>
    dplyr::filter(.method_name %in% !!KEEP_METHODS)
}

HERITABILITY_LABS <- list(
  "0.2" = "Weak (0.2)",
  "0.6" = "Moderate (0.6)",
  "1" = "Strong (1)"
)
signal_labeller <- function(labels) {
  labellers <- list(
    tau_heritability = HERITABILITY_LABS
  )
  for (var in names(labels)) {
    if (var %in% names(labellers)) {
      labels[[var]] <- unname(
        labellers[[var]][as.character(labels[[var]])]
      )
    }
  }
  labels
}

tauhats_df <- dplyr::bind_rows(tauhats_df_ls, .id = "tau_heritability") |>
  dplyr::mutate(
    .method_name = stringr::str_replace(.method_name, "Distilled ", "Distilled\n")
  )
splits_df <- dplyr::bind_rows(splits_df_ls, .id = "tau_heritability") |>
  dplyr::mutate(
    .method_name = stringr::str_replace(.method_name, "Distilled ", "Distilled\n")
  )
jaccard_dist_df <- dplyr::bind_rows(jaccard_dist_df_ls, .id = "tau_heritability")

tauhat_plt <- tauhats_df |>
  ggplot2::ggplot() +
  ggplot2::aes(x = x1, y = tauhat) +
  ggplot2::geom_point(color = "darkgrey", size = 0.75) +
  ggplot2::geom_vline(
    ggplot2::aes(xintercept = thr),
    data = splits_df |> dplyr::filter(depth == 1, var == "X1"),
    inherit.aes = FALSE, color = "black", linewidth = 0.1
  ) +
  ggplot2::geom_segment(
    ggplot2::aes(x = min(tauhats_df$x1), xend = 0, y = 0, yend = 0),
    linewidth = 1, color = "orange", inherit.aes = FALSE
  ) +
  ggplot2::geom_segment(
    ggplot2::aes(x = 0, xend = max(tauhats_df$x1), y = 1, yend = 1),
    linewidth = 1, color = "orange", inherit.aes = FALSE
  ) +
  ggplot2::facet_grid(
    .method_name ~ tau_heritability, labeller = signal_labeller
  ) +
  ggplot2::labs(x = "X1", y = expression(hat(tau)(X))) +
  vthemes::theme_vmodern(size_preset = "medium")

jaccard_dist_plt <- jaccard_dist_df |>
  dplyr::mutate(
    .method_name_str = stringr::str_replace_all(.method_name, " ", "\n")
  ) |>
  ggplot2::ggplot() +
  ggplot2::aes(
    x = .method_name_str,
    y = jaccard,
    fill = .method_name
  ) +
  ggplot2::geom_boxplot() +
  ggplot2::facet_grid(
    ~ tau_heritability, labeller = signal_labeller
  ) +
  ggplot2::scale_fill_manual(
    values = COLORS
  ) +
  ggplot2::labs(
    x = "Method",
    y = "JaccardSSI\nBootstrap Distribution",
    fill = "Method"
  ) +
  vthemes::theme_vmodern(size_preset = "medium") +
  ggplot2::guides(
    fill = "none"
  )

plt <- patchwork::wrap_plots(
  tauhat_plt, jaccard_dist_plt, heights = c(4, 1)
) +
  patchwork::plot_annotation(
    tag_levels = "A",
    tag_prefix = "(",
    tag_suffix = ")",
    theme = ggplot2::theme(
      plot.title = ggplot2::element_text(
        size = 18, face = "italic", hjust = 0.42
      )
    )
  ) &
  ggplot2::theme(
    plot.tag = ggplot2::element_text(size = 18, face = 'italic')
  )
ggplot2::ggsave(
  plt,
  filename = here::here("results", "figures", "jaccard_example.pdf"),
  width = 11, height = 12
)
