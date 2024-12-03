#' fit rulefit as the student model in CDT
student_rulefit <- function(X, y, rulefit_args = list(seed = 1)) {
  # return "null" values if y is the constant vector
  if (length(unique(y)) == 1) {
    out <- list(
      fit = NULL,
      tree_info = NULL,
      subgroups = list(),
      predictions = rep(unique(y), nrow(X))
    )
  } else {
    # h2o::h2o.init()

    df <- h2o::as.h2o(data.frame(X, y))

    # Set the predictors and response; set the factors:
    response <- "y"
    predictors <- colnames(data.frame(X))

    fit <- do.call(
      h2o::h2o.rulefit,
      args = c(
        list(
          y = response,
          x = predictors,
          training_frame = df
        ),
        rulefit_args
      )
    )

    tree_info <- get_rulefit_str(fit)
    subgroups <- get_rulefit_subgroups(fit)
    predictions <- as.vector(predict(fit, df))
    out <- list(
      fit = fit,
      tree_info = tree_info,
      subgroups = subgroups,
      predictions = predictions
    )
    # h2o::h2o.shutdown(prompt = FALSE)
  }
  return(out)
}


#' Get model structure from rulefit fit
get_rulefit_str <- function(fit) {
  rulefit_imp_df <- h2o::h2o.rule_importance(fit)

  # columns: variable, coefficient, support, rule
  # columns in model_str:
  # var type node ix left count ncat improve thr adj
  splits_list <- unlist(stringr::str_split(rulefit_imp_df$rule, "[[or]]|&")) |>
    stringr::str_trim() |>
    stringr::str_remove_all("^\\(|\\)$")
  splits_df <- data.frame(rule = splits_list) |>
    dplyr::filter(rule != "") |>
    dplyr::mutate(
      rule_split = stringr::str_split(rule, " "),
      var = purrr::map_chr(rule_split, ~ .x[1]),
      thr = purrr::map_dbl(rule_split, ~ suppressWarnings(as.numeric(.x[3]))),
      left = purrr::map_chr(rule_split, ~ paste(.x[2], .x[3]))
    ) |>
    dplyr::filter(!is.na(thr))

  return(splits_df)
}


#' Get subgroups from rulefit fit
get_rulefit_subgroups <- function(fit) {
  # want a list of lists where each list is a subgroup
  # ex. (X1 >= 2.0964964642189443E-4 or X1 is NA) & (X10 < 2.444741725921631 or X10 is NA) & (X7 >= -1.7999589443206787 or X7 is NA)
  # want to turn that into
  # "X1 >= 2.0964964642189443E-4" "X10 < 2.444741725921631" "X7 >= -1.7999589443206787"
  rulefit_imp_df <- h2o::h2o.rule_importance(fit)

  subgroups <- stringr::str_split(rulefit_imp_df$rule, "[[or]]|&") |>
    purrr::map(
      function(rule) {
        cleaned_rule <- stringr::str_trim(rule) |>
          stringr::str_remove_all("^\\(|\\)$")
        rm_na_rule <- stringr::str_detect(cleaned_rule, "is NA$") |
          (cleaned_rule == "")
        cleaned_rule[!rm_na_rule]
      }
    ) |>
    purrr::compact()
  return(subgroups)
}
