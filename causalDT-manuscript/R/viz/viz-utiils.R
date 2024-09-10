get_best_distilled_method_info <- function(fit_results,
                                           vary_params = NULL,
                                           max_depths = 1:4) {
  id_cols <- c(".rep", ".dgp_name", ".method_name", vary_params)

  best_methods_df <- fit_results |>
    dplyr::filter(
      stringr::str_detect(.method_name, "Distilled")
    ) |>
    dplyr::mutate(
      jaccard = purrr::map(
        stability_diagnostics,
        ~ tibble::tibble(
          depth = 1:length(.x$jaccard_mean),
          jaccard = .x$jaccard_mean
        )
      )
    ) |>
    dplyr::select(
      tidyselect::all_of(id_cols), jaccard
    ) |>
    tidyr::unnest(jaccard) |>
    dplyr::filter(depth <= max(!!max_depths)) |>
    dplyr::group_by(
      dplyr::across(tidyselect::all_of(id_cols))
    ) |>
    dplyr::mutate(
      cum_jaccard = cumsum(jaccard)
    ) |>
    dplyr::filter(depth %in% !!max_depths) |>
    dplyr::group_by(
      dplyr::across(
        tidyselect::all_of(c(".rep", ".dgp_name", vary_params, "depth"))
      )
    ) |>
    dplyr::slice_max(cum_jaccard)

  return(best_methods_df)
}

