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
