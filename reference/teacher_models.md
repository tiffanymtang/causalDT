# Teacher models for causal distillation trees

These functions are wrappers around various heterogeneous treatment
effect learners that can be easily used as teacher models in the causal
distillation tree framework.

- `causal_forest()`: wrapper around
  [`grf::causal_forest()`](https://rdrr.io/pkg/grf/man/causal_forest.html).

- `bcf()`: wrapper around
  [`bcf::bcf()`](https://rdrr.io/pkg/bcf/man/bcf.html).

- `rboost()`: (defunct) wrapper around `rlearner::rboost()`.

- `rlasso()`: (defunct) wrapper around `rlearner::rlasso()`.

- `rkern()`: (defunct) wrapper around `rlearner::rkern()`.

Warning: The `rboost()`, `rlasso()`, and `rkern()` functions are defunct
as of version 1.0.0. Use
[`rlearner_teacher()`](https://tiffanymtang.github.io/causalDT/reference/rlearner_teacher.md)
(e.g., `rlearner_teacher(rlearner::rboost)`) instead to convert
`rlearner` functions into correct format for use as teacher model in
CDT.

## Usage

``` r
causal_forest(X, Y, Z, W = NULL, ...)

rboost(X, Y, Z, W = NULL, ...)

rlasso(X, Y, Z, W = NULL, ...)

rkern(X, Y, Z, W = NULL, ...)

bcf(
  X,
  Y,
  Z,
  W = NULL,
  pihat = "default",
  w = NULL,
  nburn = 2000,
  nsim = 1000,
  n_threads = 1,
  no_output = TRUE,
  ...
)
```

## Arguments

- X:

  A tibble, data.frame, or matrix of covariates.

- Y:

  A vector of outcomes.

- Z:

  A vector of treatments.

- W:

  A vector of weights corresponding to treatment propensities.

- ...:

  Additional arguments to pass to the base model functions.

- pihat:

  Length n estimates of propensity score

- w:

  An optional vector of weights. When present, BCF fits a model \\y \| x
  ~ N(f(x), \sigma^2 / w)\\, where \\f(x)\\ is the unknown function.

- nburn:

  Number of burn-in MCMC iterations

- nsim:

  Number of MCMC iterations to save after burn-in. The chain will run
  for nsim\*nthin iterations after burn-in

- n_threads:

  An optional integer of the number of threads to parallelize within
  chain bcf operations on

- no_output:

  logical, whether to suppress writing trees and training log to text
  files, defaults to FALSE.

## Value

Outputs of the respective base model functions:

- `causal_forest()`: see output of
  [`grf::causal_forest()`](https://rdrr.io/pkg/grf/man/causal_forest.html).

- `rboost()` (defunct): see output of `rlearner::rboost()`.

- `rlasso()` (defunct): see output of `rlearner::rlasso()`.

- `rkern()` (defunct): see output of `rlearner::rkern()`.
