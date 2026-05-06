# Rlearner teacher model wrapper for causal distillation trees

This is a wrapper function to convert any of the `rlearner` model
functions into a format that can be used as teacher model in the causal
distillation tree framework.

## Usage

``` r
rlearner_teacher(rlearner_fun, ...)
```

## Arguments

- rlearner_fun:

  One of `rlearner::rboost`, `rlearner::rlasso`, or `rlearner::rkern` to
  be transformed to teacher model format for CDT.

- ...:

  Additional arguments to pass to the base model functions.

## Value

Outputs a function that can be used as teacher model in the causal
distillation tree framework. The returned function has the signature
`function(X, Y, Z, W = NULL, ...)`.
