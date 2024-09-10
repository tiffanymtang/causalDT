### Main Distillation Methods ####
NREPS_CROSSFIT <- 50

# without stability diagnostics
distilled_causal_forest_method <- create_method(
  .method_fun = causalDT_method,
  .name = "Distilled Causal Forest",
  teacher_model = "causal_forest"
)

distilled_rboost_method <- create_method(
  .method_fun = causalDT_method,
  .name = "Distilled Rboost",
  teacher_model = causalDT::rboost
)

distilled_rlasso_method <- create_method(
  .method_fun = causalDT_method,
  .name = "Distilled Rlasso",
  teacher_model = causalDT::rlasso
)

# with stability diagnostics
distilled_causal_forest_stability_method <- create_method(
  .method_fun = causalDT_method,
  .name = "Distilled Causal Forest",
  teacher_model = "causal_forest",
  B_stability = 100
)

distilled_rboost_stability_method <- create_method(
  .method_fun = causalDT_method,
  .name = "Distilled Rboost",
  teacher_model = causalDT::rboost,
  B_stability = 100
)

distilled_rlasso_stability_method <- create_method(
  .method_fun = causalDT_method,
  .name = "Distilled Rlasso",
  teacher_model = causalDT::rlasso,
  B_stability = 100
)

distilled_rboost_no_crossfit_stability_method <- create_method(
  .method_fun = causalDT_method,
  .name = "Distilled Rboost (no crossfit)",
  teacher_model = causalDT::rboost,
  B_stability = 100,
  nfolds_crossfit = 1,
  nreps_crossfit = 1
)

distilled_rlasso_no_crossfit_stability_method <- create_method(
  .method_fun = causalDT_method,
  .name = "Distilled Rlasso (no crossfit)",
  teacher_model = causalDT::rlasso,
  B_stability = 100,
  nfolds_crossfit = 1,
  nreps_crossfit = 1
)

#### Causal Tree Methods ####
causal_tree_method <- create_method(
  .method_fun = causal_tree,
  .name = "Causal Tree (pruned)"
)

causal_tree_unpruned_method <- create_method(
  .method_fun = causal_tree,
  .name = "Causal Tree (unpruned)",
  prune = FALSE
)

causal_tree_stability_method <- create_method(
  .method_fun = causal_tree,
  .name = "Causal Tree (pruned)",
  B_stability = 100
)

causal_tree_unpruned_stability_method <- create_method(
  .method_fun = causal_tree,
  .name = "Causal Tree (unpruned)",
  prune = FALSE,
  B_stability = 100
)

#### Other Baseline Methods ####
virtual_twins_method <- create_method(
  .method_fun = virtual_twins,
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
  nreps_crossfit = 1
)

distilled_rboost_no_crossfit_method <- create_method(
  .method_fun = causalDT_method,
  .name = "Distilled Rboost (no crossfit)",
  teacher_model = causalDT::rboost,
  nfolds_crossfit = 1,
  nreps_crossfit = 1
)

distilled_rlasso_no_crossfit_method <- create_method(
  .method_fun = causalDT_method,
  .name = "Distilled Rlasso (no crossfit)",
  teacher_model = causalDT::rlasso,
  nfolds_crossfit = 1,
  nreps_crossfit = 1
)


distilled_causal_forest_crossfit_method <- create_method(
  .method_fun = causalDT_method,
  .name = "Distilled Causal Forest (crossfit)",
  teacher_model = "causal_forest",
  nfolds_crossfit = 2,
  nreps_crossfit = 1
)

distilled_rboost_crossfit_method <- create_method(
  .method_fun = causalDT_method,
  .name = "Distilled Rboost (crossfit)",
  teacher_model = causalDT::rboost,
  nfolds_crossfit = 2,
  nreps_crossfit = 1
)

distilled_rlasso_crossfit_method <- create_method(
  .method_fun = causalDT_method,
  .name = "Distilled Rlasso (crossfit)",
  teacher_model = causalDT::rlasso,
  nfolds_crossfit = 2,
  nreps_crossfit = 1
)

#### Stump Methods for Theory Sims ####
distilled_causal_forest_stump_method <- create_method(
  .method_fun = causalDT_method,
  .name = "Distilled Causal Forest (stump)",
  teacher_model = "causal_forest",
  rpart_control = list(maxdepth = 1)
)

causal_tree_stump_method <- create_method(
  .method_fun = causal_tree,
  .name = "Causal Tree (stump)",
  causaltree_args = list(
    split.Rule = "CT",
    cv.option = "CT",
    split.Honest = TRUE,
    cv.Honest = TRUE,
    split.Bucket = FALSE,
    xval = 5,
    cp = 0,
    minsize = 1,
    propensity = 0.5,
    maxdepth = 1
  )
)
