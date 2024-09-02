### Distillation methods ####
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

#### Causal tree methods ####
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
