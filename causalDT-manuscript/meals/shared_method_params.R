RETURN_DATA <- FALSE
ORACLE_CAUSAL_TREE_ARGS <- list(
  split.Rule = "CT",
  cv.option = "CT",
  split.Honest = TRUE,
  cv.Honest = TRUE,
  split.Bucket = FALSE,
  xval = 10,
  cp = 1e-4,
  minsize = 20,
  propensity = 0.5
)
