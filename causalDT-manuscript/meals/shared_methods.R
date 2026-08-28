### Main Distillation Methods ####

# without stability diagnostics
distilled_causal_forest_method <- create_method(
  .method_fun = causalDT_method,
  .name = "Distilled Causal Forest",
  teacher_model = "causal_forest",
  rpart_prune = "min",
  return_data = RETURN_DATA
)

distilled_rboost_method <- create_method(
  .method_fun = causalDT_method,
  .name = "Distilled Rboost",
  teacher_model = causalDT::rlearner_teacher(purrr::partial(rlearner::rboost, nthread = 1)),
  nfolds_crossfit = 1,
  nreps_crossfit = 1,
  rpart_prune = "min",
  return_data = RETURN_DATA
)

distilled_rlasso_method <- create_method(
  .method_fun = causalDT_method,
  .name = "Distilled Rlasso",
  teacher_model = causalDT::rlearner_teacher(rlearner::rlasso),
  nfolds_crossfit = 1,
  nreps_crossfit = 1,
  rpart_prune = "min",
  return_data = RETURN_DATA
)

distilled_rspline_method <- create_method(
  .method_fun = causalDT_method,
  .name = "Distilled Rspline",
  teacher_model = causalDT::rlearner_teacher(rspline),
  teacher_predict = predict_rspline,
  nfolds_crossfit = 1,
  nreps_crossfit = 1,
  rpart_prune = "min",
  return_data = RETURN_DATA
)

distilled_bcf_method <- create_method(
  .method_fun = causalDT_method,
  .name = "Distilled BCF",
  teacher_model = purrr::partial(
    causalDT::bcf, pihat = 0.5, n_threads = 1, verbose = FALSE, no_output = TRUE
  ),
  teacher_predict = causalDT::predict_bcf,
  nfolds_crossfit = 1,
  nreps_crossfit = 1,
  rpart_prune = "min",
  return_data = RETURN_DATA
)


# rulefit methods without stability diagnostics
distilled_causal_forest_rulefit_v1_method <- create_method(
  .method_fun = causalDT_method,
  .name = "Distilled Causal Forest (rulefit v1)",
  teacher_model = "causal_forest",
  student_model = "rulefit",
  rulefit_args = NULL,
  return_data = RETURN_DATA
)

distilled_causal_forest_rulefit_v2_method <- create_method(
  .method_fun = causalDT_method,
  .name = "Distilled Causal Forest (rulefit v2)",
  teacher_model = "causal_forest",
  student_model = "rulefit",
  rulefit_args = list(maxdepth = 2),
  return_data = RETURN_DATA
)

distilled_causal_forest_rulefit_v3_method <- create_method(
  .method_fun = causalDT_method,
  .name = "Distilled Causal Forest (rulefit v3)",
  teacher_model = "causal_forest",
  student_model = "rulefit",
  rulefit_args = list(type = "rules"),
  return_data = RETURN_DATA
)

distilled_causal_forest_rulefit_v4_method <- create_method(
  .method_fun = causalDT_method,
  .name = "Distilled Causal Forest (rulefit v4)",
  teacher_model = "causal_forest",
  student_model = "rulefit",
  rulefit_args = list(type = "rules", maxdepth = 2),
  return_data = RETURN_DATA
)

# with stability diagnostics
distilled_causal_forest_stability_method <- create_method(
  .method_fun = causalDT_method,
  .name = "Distilled Causal Forest",
  teacher_model = "causal_forest",
  B_stability = 100,
  rpart_prune = "min",
  return_data = RETURN_DATA
)

distilled_rboost_stability_method <- create_method(
  .method_fun = causalDT_method,
  .name = "Distilled Rboost",
  teacher_model = causalDT::rlearner_teacher(purrr::partial(rlearner::rboost, nthread = 1)),
  nfolds_crossfit = 1,
  nreps_crossfit = 1,
  B_stability = 100,
  rpart_prune = "min",
  return_data = RETURN_DATA
)

distilled_rlasso_stability_method <- create_method(
  .method_fun = causalDT_method,
  .name = "Distilled Rlasso",
  teacher_model = causalDT::rlearner_teacher(rlearner::rlasso),
  nfolds_crossfit = 1,
  nreps_crossfit = 1,
  B_stability = 100,
  rpart_prune = "min",
  return_data = RETURN_DATA
)

distilled_bcf_stability_method <- create_method(
  .method_fun = causalDT_method,
  .name = "Distilled BCF",
  teacher_model = purrr::partial(
    causalDT::bcf, pihat = 0.5, n_threads = 1, verbose = FALSE, no_output = TRUE
  ),
  teacher_predict = causalDT::predict_bcf,
  nfolds_crossfit = 1,
  nreps_crossfit = 1,
  B_stability = 100,
  rpart_prune = "min",
  return_data = RETURN_DATA
)

# with stability diagnostics and pruning options
distilled_causal_forest_stability_pruned_method <- create_method(
  .method_fun = causalDT_method,
  .name = "Distilled Causal Forest",
  teacher_model = "causal_forest",
  B_stability = 100,
  rpart_prune = c("none", "min"),
  return_data = RETURN_DATA
)

distilled_rboost_stability_pruned_method <- create_method(
  .method_fun = causalDT_method,
  .name = "Distilled Rboost",
  teacher_model = causalDT::rlearner_teacher(purrr::partial(rlearner::rboost, nthread = 1)),
  nfolds_crossfit = 1,
  nreps_crossfit = 1,
  B_stability = 100,
  rpart_prune = c("none", "min"),
  return_data = RETURN_DATA
)

distilled_rlasso_stability_pruned_method <- create_method(
  .method_fun = causalDT_method,
  .name = "Distilled Rlasso",
  teacher_model = causalDT::rlearner_teacher(rlearner::rlasso),
  nfolds_crossfit = 1,
  nreps_crossfit = 1,
  B_stability = 100,
  rpart_prune = c("none", "min"),
  return_data = RETURN_DATA
)

distilled_rspline_stability_pruned_method <- create_method(
  .method_fun = causalDT_method,
  .name = "Distilled Rspline",
  teacher_model = causalDT::rlearner_teacher(rspline),
  teacher_predict = predict_rspline,
  nfolds_crossfit = 1,
  nreps_crossfit = 1,
  B_stability = 100,
  rpart_prune = c("none", "min"),
  return_data = RETURN_DATA
)

distilled_bcf_stability_pruned_method <- create_method(
  .method_fun = causalDT_method,
  .name = "Distilled BCF",
  teacher_model = purrr::partial(
    causalDT::bcf, pihat = 0.5, n_threads = 1, verbose = FALSE, no_output = TRUE
  ),
  teacher_predict = causalDT::predict_bcf,
  nfolds_crossfit = 1,
  nreps_crossfit = 1,
  B_stability = 100,
  rpart_prune = c("none", "min"),
  return_data = RETURN_DATA
)

#### Causal Tree Methods ####
causal_tree_method <- create_method(
  .method_fun = causal_tree,
  .name = "Causal Tree",
  prune = "min"
)

causal_tree_unpruned_method <- create_method(
  .method_fun = causal_tree,
  .name = "Causal Tree (unpruned)",
  prune = "none"
)

causal_tree_stability_method <- create_method(
  .method_fun = causal_tree,
  .name = "Causal Tree",
  prune = "min",
  B_stability = 100
)

causal_tree_unpruned_stability_method <- create_method(
  .method_fun = causal_tree,
  .name = "Causal Tree (unpruned)",
  prune = "none",
  B_stability = 100
)

causal_tree_stability_pruned_method <- create_method(
  .method_fun = causal_tree,
  .name = "Causal Tree",
  B_stability = 100,
  prune = c("none", "min")
)

causal_tree_oracle_stability_pruned_method <- create_method(
  .method_fun = causal_tree,
  .name = "Causal Tree (cp = 1E-4)",
  B_stability = 100,
  prune = c("none", "min"),
  causaltree_args = ORACLE_CAUSAL_TREE_ARGS
)

#### Other Baseline Methods ####
virtual_twins_method <- create_method(
  .method_fun = virtual_twins,
  rpart_prune = "min",
  .name = "Virtual Twins"
)

lm_method <- create_method(
  .method_fun = linear_reg_subgroups,
  .name = "Linear Regression"
)

lasso_method <- create_method(
  .method_fun = lasso_reg_subgroups,
  .name = "Lasso"
)

#### Distillation Methods with varying # crossfits ####
distilled_causal_forest_no_crossfit_method <- create_method(
  .method_fun = causalDT_method,
  .name = "Distilled Causal Forest (no crossfit)",
  teacher_model = "causal_forest",
  nfolds_crossfit = 1,
  nreps_crossfit = 1,
  prune = "min"
)

distilled_rboost_no_crossfit_method <- create_method(
  .method_fun = causalDT_method,
  .name = "Distilled Rboost (no crossfit)",
  teacher_model = causalDT::rlearner_teacher(purrr::partial(rlearner::rboost, nthread = 1)),
  nfolds_crossfit = 1,
  nreps_crossfit = 1,
  prune = "min"
)

distilled_causal_forest_crossfit_method <- create_method(
  .method_fun = causalDT_method,
  .name = "Distilled Causal Forest (crossfit)",
  teacher_model = "causal_forest",
  nfolds_crossfit = 1,
  nreps_crossfit = 1,
  prune = "min"
)

distilled_rboost_crossfit_method <- create_method(
  .method_fun = causalDT_method,
  .name = "Distilled Rboost (crossfit)",
  teacher_model = causalDT::rlearner_teacher(purrr::partial(rlearner::rboost, nthread = 1)),
  nfolds_crossfit = 1,
  nreps_crossfit = 1,
  prune = "min"
)
