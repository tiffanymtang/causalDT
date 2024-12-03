causalDT_method <- function(X, Y, Z,
                            holdout_idxs = NULL,
                            holdout_prop = 0.3,
                            teacher_model = "causal_forest",
                            teacher_predict = NULL,
                            student_model = "rpart",
                            rpart_control = NULL,
                            rpart_prune = c("none", "min", "1se"),
                            nfolds_crossfit = NULL,
                            nreps_crossfit = NULL,
                            B_stability = 0,
                            max_depth_stability = NULL,
                            causalDT_args = NULL,
                            return_details = FALSE,
                            return_permuted_results = FALSE,
                            ...) {

  rpart_prune <- match.arg(rpart_prune, several.ok = TRUE)
  if (length(rpart_prune) > 1) {
    rpart_prune0 <- "none"
  } else {
    rpart_prune0 <- rpart_prune
  }

  start_time <- Sys.time()
  X_dummy <- dummy_code(X)
  results <- do.call(
    causalDT::causalDT,
    args = c(
      list(
        X = X_dummy,
        Y = Y,
        Z = Z,
        holdout_idxs = holdout_idxs,
        holdout_prop = holdout_prop,
        teacher_model = teacher_model,
        teacher_predict = teacher_predict,
        student_model = student_model,
        rpart_control = rpart_control,
        rpart_prune = rpart_prune0,
        nfolds_crossfit = nfolds_crossfit,
        nreps_crossfit = nreps_crossfit,
        B_stability = B_stability,
        max_depth_stability = max_depth_stability
      ),
      causalDT_args
    )
  )

  X_est <- X_dummy[results$holdout_idxs, , drop = FALSE]
  Y_est <- Y[results$holdout_idxs]
  Z_est <- Z[results$holdout_idxs]

  fit_ls <- list()
  predictions_ls <- list()
  group_cates_ls <- list()
  tree_info_ls <- list()
  subgroups_ls <- list()
  if ((length(rpart_prune) > 1) && (identical(student_model, "rpart"))) {
    for (rpart_prune_mode in rpart_prune) {
      fit <- results$student_fit$fit
      if (is.null(fit)) {
        return(NULL)
      } else if (rpart_prune_mode != "none") {
        best_cp <- as.data.frame(fit$cptable) |>
          dplyr::filter(xerror == min(xerror, na.rm = TRUE)) |>
          dplyr::slice(1)
        if (rpart_prune_mode == "min") {
          fit <- rpart::prune(fit, cp = best_cp$CP)
        } else if (rpart_prune_mode == "1se") {
          best1se_cp <- as.data.frame(fit$cptable) |>
            dplyr::filter(xerror <= (best_cp$xerror + best_cp$xstd)) |>
            dplyr::filter(nsplit == min(nsplit, na.rm = TRUE)) |>
            dplyr::slice(1)
          fit <- rpart::prune(fit, cp = best1se_cp$CP)
        }
      }
      fit_ls[[rpart_prune_mode]] <- fit
      subgroups_ls[[rpart_prune_mode]] <- causalDT::get_rpart_paths(fit)
      tree_info_ls[[rpart_prune_mode]] <- causalDT::get_rpart_tree_info(fit)
      predictions_ls[[rpart_prune_mode]] <- predict(fit)
      group_cates_ls[[rpart_prune_mode]] <- causalDT::estimate_group_cates(
        fit = fit, X = X_est, Y = Y_est, Z = Z_est
      )
    }
  } else {
    fit_ls[[rpart_prune[1]]] <- results$student_fit$fit
    subgroups_ls[[rpart_prune[1]]] <- results$student_fit$subgroups
    tree_info_ls[[rpart_prune[1]]] <- results$student_fit$tree_info
    predictions_ls[[rpart_prune[1]]] <- results$student_fit$predictions
    group_cates_ls[[rpart_prune[1]]] <- results$estimate
  }
  end_time <- Sys.time()

  permuted_results <- NULL
  if (return_permuted_results) {
    Y_permuted <- sample(Y, size = length(Y), replace = FALSE)
    permuted_results <- causalDT_method(
      X = X,
      Y = Y_permuted,
      Z = Z,
      holdout_idxs = holdout_idxs,
      holdout_prop = holdout_prop,
      teacher_model = teacher_model,
      teacher_predict = teacher_predict,
      student_model = student_model,
      rpart_control = rpart_control,
      nfolds_crossfit = nfolds_crossfit,
      nreps_crossfit = nreps_crossfit,
      B_stability = B_stability,
      max_depth_stability = max_depth_stability,
      causalDT_args = causalDT_args
    )
  }

  detailed_out <- NULL
  if (return_details) {
    detailed_out <- list(
      teacher_fit = results$teacher_fit,
      teacher_predictions_ls = results$teacher_predictions_ls,
      crossfit_idxs_ls = results$crossfit_idxs_ls
    )
  }

  out <- c(
    list(
      est_data = list(
        X = X_est,
        Y = Y_est,
        Z = Z_est
      ),
      fit = fit_ls,
      model_info = tree_info_ls,
      subgroups = subgroups_ls,
      group_cates = group_cates_ls,
      teacher_predictions = results$teacher_predictions,
      student_predictions = predictions_ls,
      stability_diagnostics = results$stability_diagnostics,
      holdout_idxs = results$holdout_idxs,
      permuted_results = permuted_results,
      time_elapsed = as.numeric(difftime(end_time, start_time, units = "secs")),
      ...
    ),
    detailed_out
  )
  return(out)
}


virtual_twins <- function(X, Y, Z,
                          ranger_args = list(),
                          rpart_control = NULL,
                          rpart_prune = c("none", "min", "1se"),
                          holdout_prop = 0.3,
                          return_details = FALSE,
                          ...) {

  rpart_prune <- match.arg(rpart_prune)
  start_time <- Sys.time()
  X <- dummy_code(X)
  group_cates <- NULL

  if (holdout_prop > 0) {
    holdout_idxs <- sample(1:nrow(X), size = round(holdout_prop * nrow(X)))
    X_est <- X[holdout_idxs, , drop = FALSE]
    Y_est <- Y[holdout_idxs]
    Z_est <- Z[holdout_idxs]
    X <- X[-holdout_idxs, , drop = FALSE]
    Y <- Y[-holdout_idxs]
    Z <- Z[-holdout_idxs]
  } else {
    X_est <- X
    Y_est <- Y
    Z_est <- Z
  }

  augmented_df <- data.frame(X, Z, Y, Z * X, (1 - Z) * X)

  # set default ranger arguments if not provided as input
  if (!("mtry" %in% ranger_args)) {
    ranger_args[["mtry"]] <- ncol(augmented_df) / 3
  }

  # Step 1: Fit random forest to predict y
  Y_forest <- do.call(
    ranger::ranger,
    args = c(list(formula = Y ~ ., data = augmented_df), ranger_args)
  )

  # Step 2: Get oob and predict counterfactual y
  Yhat <- Y_forest$predictions
  Z <- 1 - Z
  augmented_counter_df <- data.frame(X, Z, Y, Z * X, (1 - Z) * X)
  Yhat_counter <- predict(Y_forest, augmented_counter_df)$predictions

  # Step 3: Get tauhat from predictions in step 2
  tauhat <- ifelse(
    augmented_df$Z == 1, Yhat - Yhat_counter, Yhat_counter - Yhat
  )

  # Step 4: Train decision tree to predict tau.hat
  student_fit_out <- causalDT::student_rpart(
    X, tauhat, rpart_control = rpart_control, prune = rpart_prune
  )

  # Step 5: Estimate group CATEs
  group_cates <- causalDT::estimate_group_cates(
    fit = student_fit_out$fit,
    X = X_est,
    Y = Y_est,
    Z = Z_est
  )

  end_time <- Sys.time()

  detailed_out <- NULL
  if (return_details) {
    detailed_out <- list(
      vt_fit = Y_forest
    )
  }

  out <- c(
    list(
      est_data = list(
        X = X_est,
        Y = Y_est,
        Z = Z_est
      ),
      fit = student_fit_out$fit,
      model_info = student_fit_out$tree_info,
      subgroups = student_fit_out$subgroups,
      group_cates = group_cates,
      teacher_predictions = tauhat,
      student_predictions = student_fit_out$predictions,
      holdout_idxs = holdout_idxs,
      time_elapsed = as.numeric(difftime(end_time, start_time, units = "secs")),
      ...
    ),
    detailed_out
  )
  return(out)
}


causal_tree <- function(X, Y, Z,
                        prune = c("none", "min", "1se"),
                        causaltree_args = list(
                          split.Rule = "CT",
                          cv.option = "CT",
                          split.Honest = TRUE,
                          cv.Honest = TRUE,
                          split.Bucket = FALSE,
                          xval = 5,
                          cp = 0,
                          minsize = 20,
                          propensity = 0.5
                        ),
                        B_stability = 0,
                        max_depth_stability = NULL,
                        return_details = FALSE,
                        ...) {

  prune <- match.arg(prune, several.ok = TRUE)
  rpart_control <- causaltree_args$control
  causaltree_args$control <- NULL

  start_time <- Sys.time()

  # Step 1: Fit a causal tree
  # Make sure to load causalTree package before calling. Otherwise, will
  # receive 'rpart.control' not found error.
  fit <- causaltree_wrapper(
    X = X, Y = Y, Z = Z,
    rpart_control = rpart_control,
    causaltree_args = causaltree_args
  )

  # Prune tree
  # pruning
  fit_ls <- list()
  tauhat_ls <- list()
  group_cates_ls <- list()
  tree_info_ls <- list()
  subgroups_ls <- list()
  for (prune_mode in prune) {
    if (prune_mode != "none") {
      best_cp <- as.data.frame(fit$cptable) |>
        dplyr::filter(xerror == min(xerror, na.rm = TRUE)) |>
        dplyr::slice(1)
      if (prune_mode == "min") {
        pruned_fit <- rpart::prune(fit, cp = best_cp$CP)
      } else if (prune_mode == "1se") {
        best1se_cp <- as.data.frame(fit$cptable) |>
          dplyr::filter(xerror <= (best_cp$xerror + best_cp$xstd)) |>
          dplyr::filter(nsplit == min(nsplit, na.rm = TRUE)) |>
          dplyr::slice(1)
        pruned_fit <- rpart::prune(fit, cp = best1se_cp$CP)
      }
    } else {
      pruned_fit <- fit
    }
    tauhat <- predict(pruned_fit)
    group_cates <- tibble::tibble(
      estimate = tauhat,
      Z = Z
    ) |>
      dplyr::group_by(estimate) |>
      dplyr::summarise(
        .n1 = sum(Z == 1),
        .n0 = sum(Z == 0),
        .sample_idxs = list(dplyr::cur_group_rows()),
        .groups = "drop"
      )

    # Step 2: Extract subgroups from causal tree
    tree_info <- causalDT::get_rpart_tree_info(pruned_fit)
    subgroups <- causalDT::get_rpart_paths(pruned_fit)

    fit_ls[[prune_mode]] <- pruned_fit
    tauhat_ls[[prune_mode]] <- tauhat
    group_cates_ls[[prune_mode]] <- group_cates
    tree_info_ls[[prune_mode]] <- tree_info
    subgroups_ls[[prune_mode]] <- subgroups
  }

  # Evaluate stability diagnostics
  stability_out <- causalDT::evaluate_subgroup_stability(
    estimator = purrr::partial(
      causaltree_wrapper, causaltree_args = causaltree_args
    ),
    fit = fit,
    X = X,
    y = Y,
    Z = Z,
    B = B_stability,
    rpart_control = rpart_control,
    max_depth = max_depth_stability
  )
  end_time <- Sys.time()

  out <- c(
    list(
      est_data = list(
        X = X,
        Y = Y,
        Z = Z
      ),
      fit = fit_ls,
      model_info = tree_info_ls,
      subgroups = subgroups_ls,
      group_cates = group_cates_ls,
      teacher_predictions = tauhat_ls,
      student_predictions = rep(NA_real_, length(tauhat_ls[[1]])),  # dummy placeholder for now
      stability_diagnostics = stability_out,
      time_elapsed = as.numeric(difftime(end_time, start_time, units = "secs")),
      ...
    )
  )
  return(out)
}


linear_reg_subgroups <- function(X, Y, Z, max_int = 1,
                                 return_details = FALSE,
                                 ...) {

  start_time <- Sys.time()
  X <- dummy_code(X, fullRank = TRUE)

  # Step 1: Fit a linear regression
  df <- data.frame(X, Z, Y)
  df1 <- df |>
    dplyr::mutate(Z = 1)
  df0 <- df |>
    dplyr::mutate(Z = 0)
  formula <- get_interaction_formula(df, max_int)
  fit <- lm(formula, data = df)
  tauhat <- predict(fit, df1) - predict(fit, df0)

  # Step 2: Extract subgroups from linear regression
  tidy_fit <- tidy_lm(fit)
  model_info <- get_lm_info(tidy_fit)
  subgroups <- get_lm_subgroups(tidy_fit)

  # evaluate CATE
  group_cates <- tibble::tibble(
    estimate = tauhat,
    Z = Z
  ) |>
    dplyr::group_by(estimate) |>
    dplyr::summarise(
      .n1 = sum(Z == 1),
      .n0 = sum(Z == 0),
      .sample_idxs = list(dplyr::cur_group_rows()),
      .groups = "drop"
    )
  end_time <- Sys.time()

  out <- c(
    list(
      fit = fit,
      model_info = model_info,
      subgroups = subgroups,
      group_cates = group_cates,
      teacher_predictions = tauhat,
      student_predictions = rep(NA_real_, length(tauhat)),  # dummy placeholder for now
      time_elapsed = as.numeric(difftime(end_time, start_time, units = "secs")),
      ...
    )
  )
  return(out)
}


lasso_reg_subgroups <- function(X, Y, Z, max_int = 1,
                                glmnet_args = list(nfolds = 5),
                                return_details = FALSE,
                                ...) {

  start_time <- Sys.time()
  X <- dummy_code(X, fullRank = TRUE)

  # Step 1: Fit Lasso
  df <- data.frame(X, Z, Y)
  df1 <- df |>
    dplyr::mutate(Z = 1)
  df0 <- df |>
    dplyr::mutate(Z = 0)
  formula <- get_interaction_formula(df, max_int)
  Xmat <- model.matrix(formula, df)[, -1] # remove intercept
  Xmat1 <- model.matrix(formula, df1)[, -1]
  Xmat0 <- model.matrix(formula, df0)[, -1]
  fit <- do.call(
    glmnet::cv.glmnet,
    args = c(list(x = Xmat, y = Y, alpha = 1), glmnet_args)
  )
  tauhat <- c(predict(fit, Xmat1)) - c(predict(fit, Xmat0))

  # Step 2: Extract subgroups from Lasso
  tidy_fit <- tidy_glmnet(fit)
  model_info <- get_lm_info(tidy_fit)
  subgroups <- get_lm_subgroups(tidy_fit)

  # evaluate CATE
  group_cates <- tibble::tibble(
    estimate = tauhat,
    Z = Z
  ) |>
    dplyr::group_by(estimate) |>
    dplyr::summarise(
      .n1 = sum(Z == 1),
      .n0 = sum(Z == 0),
      .sample_idxs = list(dplyr::cur_group_rows()),
      .groups = "drop"
    )
  end_time <- Sys.time()

  out <- c(
    list(
      fit = fit,
      model_info = model_info,
      subgroups = subgroups,
      group_cates = group_cates,
      teacher_predictions = tauhat,
      student_predictions = rep(NA_real_, length(tauhat)),  # dummy placeholder for now
      time_elapsed = as.numeric(difftime(end_time, start_time, units = "secs")),
      ...
    )
  )
  return(out)
}
