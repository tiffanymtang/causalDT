# Arguments that are shared across functions

Arguments that are shared across functions

## Arguments

- X:

  A tibble, data.frame, or matrix of covariates.

- Y:

  A vector of outcomes.

- y:

  A vector of responses to predict.

- Z:

  A vector of treatments.

- W:

  A vector of weights corresponding to treatment propensities.

- rpart_control:

  A list of control parameters for the `rpart` algorithm. See
  `? rpart.control` for details.

- rpart_fit:

  An `rpart` object.

- party_fit:

  A `party` object.

- prune:

  Method for pruning the tree. Default is `"none"`. Options are
  `"none"`, `"min"`, and `"1se"`. If `"min"`, the tree is pruned using
  the complexity threshold which minimizes the cross-validation error.
  If `"1se"`, the tree is pruned using the largest complexity threshold
  which yields a cross-vaidation error within one standard error of the
  minimum. If `"none"`, the tree is not pruned.

- rpart_prune:

  Method for pruning the tree. Default is `"none"`. Options are
  `"none"`, `"min"`, and `"1se"`. If `"min"`, the tree is pruned using
  the complexity threshold which minimizes the cross-validation error.
  If `"1se"`, the tree is pruned using the largest complexity threshold
  which yields a cross-vaidation error within one standard error of the
  minimum. If `"none"`, the tree is not pruned.
