collapse_dummy_vars <- function(x) {
  stringr::str_remove(x, "\\.\\..*")
}


get_subgroup_vars <- function(subgroups) {
  purrr::map(
    subgroups,
    function(subgroup) {
      stringr::str_remove(setdiff(subgroup, "root"), ">.*|<.*") |>
        collapse_dummy_vars() |>
        stringr::str_trim()
    }
  ) |>
    purrr::compact()
}


add_best_distilled_method <- function(fit_results,
                                      vary_params = NULL,
                                      max_depths = 1:4) {
  id_cols <- c(".rep", ".dgp_name", ".method_name", vary_params)
  return(fit_results)

  best_methods_df <- get_best_distilled_method_info(
    fit_results,
    vary_params = vary_params,
    max_depths = max_depths
  ) |>
    dplyr::select(-jaccard, -cum_jaccard) |>
    dplyr::left_join(fit_results, by = id_cols) |>
    dplyr::mutate(
      .method_name = "Most Stable Distilled Method"
    ) |>
    tidyr::unite(
      col = ".method_name", .method_name, depth, sep = " Depth "
    ) |>
    dplyr::ungroup()

  fit_results <- dplyr::bind_rows(fit_results, best_methods_df)
  return(fit_results)
}
