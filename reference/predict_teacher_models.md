# Predict wrappers for teacher models for causal distillation trees

These functions are [`predict()`](https://rdrr.io/r/stats/predict.html)
method wrappers for various heterogeneous treatment effect learners that
can be easily used as teacher models in the causal distillation tree
framework.

- `predict_causal_forest()`: wrapper around
  [`predict()`](https://rdrr.io/r/stats/predict.html) for
  [`causal_forest()`](https://tiffanymtang.github.io/causalDT/reference/teacher_models.md)
  models.

- `predict_bcf()`: wrapper around
  [`predict()`](https://rdrr.io/r/stats/predict.html) for
  [`bcf()`](https://tiffanymtang.github.io/causalDT/reference/teacher_models.md)
  models.

## Usage

``` r
predict_causal_forest(...)

predict_bcf(...)
```

## Arguments

- ...:

  Additional arguments to pass to the base model `predict` functions.

## Value

Vector of predicted conditional average treatment effects (CATEs).
