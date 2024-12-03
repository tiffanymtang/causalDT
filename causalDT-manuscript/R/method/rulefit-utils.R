#' fit rulefit as the student model in CDT
student_rulefit <- function(X, y, ...) {
  # return "null" values if y is the constant vector
  if (length(unique(y)) == 1) {
    out <- list(
      fit = NULL,
      tree_info = NULL,
      subgroups = list(),
      predictions = rep(unique(y), nrow(X))
    )
  } else {
    df <- data.frame(X, y)
    fit <- pre::pre(y ~ ., data = df, ...)
    tree_info <- get_rulefit_str(fit)
    subgroups <- get_rulefit_subgroups(fit)
    predictions <- predict(fit, df)
    out <- list(
      fit = fit,
      tree_info = tree_info,
      subgroups = subgroups,
      predictions = predictions
    )
  }
  return(out)
}


#' Get model structure from rulefit fit
get_rulefit_str <- function(fit) {
  # columns in splits_df:
  # rule, description, imp, coefficient, sd, var, dir, thr
  rulefit_imp_df <- pre::importance(fit, standardize = TRUE, plot = FALSE)
  splits_df <- rulefit_imp_df$baseimps |>
    dplyr::filter(stringr::str_starts(rule, "rule")) |>
    dplyr::mutate(
      description = clean_pre_rules(description)
    ) |>
    tidyr::unnest_longer(description) |>
    dplyr::mutate(
      var = stringr::str_trim(stringr::str_extract(description, "^[^<>]+")),
      dir = dplyr::case_when(
        stringr::str_detect(description, " > ") ~ ">",
        stringr::str_detect(description, " <= ") ~ "<="
      ),
      thr = stringr::str_extract(description, "(?:>\\s*|<=\\s*)(.*)") |>
        stringr::str_trim() |>
        stringr::str_remove_all("(?:>\\s*|<=\\s*)") |>
        stringr::str_trim() |>
        as.numeric()
    )
  return(splits_df)
}


#' Get subgroups from rulefit fit
get_rulefit_subgroups <- function(fit) {
  # want a list of lists where each list is a subgroup
  # ex. (X1 >= 2.0964964642189443E-4 or X1 is NA) & (X10 < 2.444741725921631 or X10 is NA) & (X7 >= -1.7999589443206787 or X7 is NA)
  # want to turn that into
  # "X1 >= 2.0964964642189443E-4" "X10 < 2.444741725921631" "X7 >= -1.7999589443206787"
  rulefit_imp_df <- pre::importance(fit, plot = FALSE)$baseimps |>
    dplyr::filter(stringr::str_starts(rule, "rule"))
  subgroups <- clean_pre_rules(rulefit_imp_df$description) |>
    purrr::compact()
  return(subgroups)
}


# clean and reduce rules from pre model fit
clean_pre_rules <- function(description) {
  rules <- stringr::str_split(description, "&") |>
    purrr::map(
      function(rule) {
        rule <- stringr::str_trim(rule)
        rule_df <- data.frame(
          var = stringr::str_trim(stringr::str_extract(rule, "^[^<>]+")),
          dir = dplyr::case_when(
            stringr::str_detect(rule, " > ") ~ ">",
            stringr::str_detect(rule, " <= ") ~ "<="
          ),
          thr = stringr::str_extract(rule, "(?:>\\s*|<=\\s*)(.*)") |>
            stringr::str_trim() |>
            stringr::str_remove_all("(?:>\\s*|<=\\s*)") |>
            stringr::str_trim()
        ) |>
          dplyr::group_by(var, dir) |>
          dplyr::summarise(
            thrs = list(thr),
            .groups = "drop"
          ) |>
          dplyr::rowwise() |>
          dplyr::mutate(
            thr = dplyr::case_when(
              dir == "<=" ~ min(thrs),
              dir == ">" ~ max(thrs)
            ),
            description = paste(var, dir, thr)
          )
        return(rule_df$description)
      }
    )
}
