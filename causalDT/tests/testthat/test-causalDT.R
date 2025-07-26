test_that("causalDT works", {

  n <- 200
  p <- 4
  X <- matrix(rnorm(n * p), nrow = n, ncol = p)
  Y <- rnorm(n)
  Z <- rbinom(n, 1, 0.5)
  teacher_models <- list(
    "causal_forest"
    # "bcf",
    # rboost,
    # rlasso
  )
  expected_names <- c(
    "estimate", "student_fit",
    "teacher_fit", "teacher_predictions", "teacher_predictions_ls",
    "crossfit_idxs_ls", "stability_diagnostics", "holdout_idxs"
  )

  # testing basic functionality
  for (teacher_model in teacher_models) {
    out <- causalDT(
      X = X, Y = Y, Z = Z,
      teacher_model = teacher_model
    )
    expect_equal(names(out), expected_names)
  }

  # testing with weights
  W <- runif(n)
  for (teacher_model in teacher_models) {
    out <- causalDT(
      X = X, Y = Y, Z = Z, W = W,
      teacher_model = teacher_model
    )
    expect_equal(names(out), expected_names)
  }

  # test with custom cross-fitting parameters
  out <- causalDT(
    X = X, Y = Y, Z = Z, W = W,
    teacher_model = "causal_forest",
    # teacher_model = rlasso,
    nfolds_crossfit = 2,
    nreps_crossfit = 10
  )
  expect_equal(length(out$crossfit_idxs_ls), 10)
})
