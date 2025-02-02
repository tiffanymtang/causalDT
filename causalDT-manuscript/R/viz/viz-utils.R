preprocess_crossfit_results <- function(eval_results) {
  eval_results <- purrr::map(
    eval_results,
    ~ .x |>
      dplyr::filter(
        !stringr::str_detect(.method_name, "unpruned"),
        !stringr::str_detect(.method_name, "Causal Forest")
      ) |>
      dplyr::mutate(
        .method_name = stringr::str_remove_all(.method_name, " \\(.*\\)"),
        nreps_crossfit = tidyr::replace_na(nreps_crossfit, 0) |>
          as.factor() |>
          forcats::fct_inseq()
      )
  )
  return(eval_results)
}


preprocess_rulefit_results <- function(eval_results) {
  eval_results <- purrr::map(
    eval_results,
    ~ .x |>
      dplyr::mutate(
        .method_name = dplyr::case_when(
          .method_name == "Distilled Causal Forest" ~ "CART",
          .method_name == "Distilled Causal Forest (rulefit v1)" ~
            "Rulefit (linear + rules, max depth = 3)",
          .method_name == "Distilled Causal Forest (rulefit v2)" ~
            "Rulefit (linear + rules, max depth = 2)",
          .method_name == "Distilled Causal Forest (rulefit v3)" ~
            "Rulefit (rules only, max depth = 3)",
          .method_name == "Distilled Causal Forest (rulefit v4)" ~
            "Rulefit (rules only, max depth = 2)",
          TRUE ~ .method_name
        )
      )
  )
  return(eval_results)
}
