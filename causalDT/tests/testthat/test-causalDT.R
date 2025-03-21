test_that("causalDT works", {

  n <- 100
  p <- 10
  X <- matrix(rnorm(n * p), nrow = n, ncol = p)
  Y <- rnorm(n)
  Z <- rbinom(n, 1, 0.5)

  teacher_model <- "causal_forest"
  teacher_model <- rlasso
  teacher_models <- list(
    "causal_forest",
    rboost,
    rlasso
  )

  out <- causalDT(
    X = X, Y = Y, Z = Z,
    teacher_model = "causal_forest"
  )

  out <- causalDT(
    X = X, Y = Y, Z = Z,
    teacher_model = "bcf"
  )

  out <- causalDT(
    X = X, Y = Y, Z = Z,
    teacher_model = rlasso,
    nfolds_crossfit = 2,
    nreps_crossfit = 50
  )

  for (teacher_model in teacher_models) {
    out <- causalDT(
      X = X, Y = Y, Z = Z,
      teacher_model = teacher_model
    )
  }
})
