#' Plot causal distillation tree object
#'
#' @description Visualize the subgroups (i.e., the student tree) from a causal
#'   distillation tree object.
#'
#' @param cdt A causal distillation tree object, typically the output of
#'   \code{\link{causalDT}}.
#' @param show_digits Number of digits to show in the plot labels. Default is 2.
#'
#' @return A plot of the causal distillation tree.
#'
#' @examples
#' n <- 200
#' p <- 10
#' X <- matrix(rnorm(n * p), nrow = n, ncol = p)
#' Z <- rbinom(n, 1, 0.5)
#' Y <- 2 * Z * (X[, 1] > 0) + X[, 2] + rnorm(n, 0.1)
#'
#' cdt <- causalDT(X, Y, Z)
#' plot_cdt(cdt)
#'
#' @export
plot_cdt <- function(cdt, show_digits = 2) {
  party_obj <- partykit::as.party(cdt$student_fit$fit)
  plt <- ggparty::ggparty(party_obj) +
    ggparty::geom_edge() +
    ggparty::geom_edge_label(
      ggplot2::aes(
        label = substr(breaks_label, start = 1, stop = 12 + show_digits)
      ),
    ) +
    ggparty::geom_node_label(ggplot2::aes(label = splitvar), ids = "inner")
  subgroup_ates <- data.frame(id = plt$data$id) |>
    dplyr::left_join(cdt$estimate, by = c("id" = "leaf_id")) |>
    dplyr::mutate(
      label = sprintf("Subgroup ATE\n= %.3f", estimate)
    ) |>
    dplyr::pull(label)
  plt$data$info <- subgroup_ates
  plt <- plt +
    ggparty::geom_node_label(ggplot2::aes(label = info), ids = "terminal")
  return(plt)
}


#' Plot Jaccard subgroup similarity index (SSI) for causal distillation tree objects
#'
#' @description The Jaccard subgroup similiarity index (SSI) is a measure of the
#'   similarity between two candidate partitions of subgroups. To select an
#'   appropriate teacher model in CDT, the Jaccard SSI can be used to select the
#'   teacher model that recovers the most stable subgroups.
#'
#' @param ... Two or more causal distillation tree objects, each is typically
#'   the output of \code{\link{causalDT}}. Arguments should be named (so that
#'   they are properly labeled in the resulting plot).
#'
#' @return A plot of the Jaccard SSI for each tree depth.
#'
#' @examples
#' n <- 50
#' p <- 2
#' X <- matrix(rnorm(n * p), nrow = n, ncol = p)
#' Z <- rbinom(n, 1, 0.5)
#' Y <- 2 * Z * (X[, 1] > 0) + X[, 2] + rnorm(n, 0.1)
#'
#' cdt1 <- causalDT(X, Y, Z)
#' cdt2 <- causalDT(X, Y, Z, teacher_model = rboost)
#' plot_jaccard(`Causal Forest` = cdt1, `Rboost` = cdt2)
#'
#' @export
plot_jaccard <- function(...) {
  dots_ls <- rlang::dots_list(...)

  default_names <- paste0("Model", 1:length(dots_ls))
  if (is.null(names(dots_ls))) {
    names(dots_ls) <- default_names
  } else {
    names(dots_ls)[names(dots_ls) == ""] <- default_names[names(dots_ls) == ""]
  }

  ssi_df <- purrr::map(
    dots_ls,
    function(cdt) {
      tibble::tibble(
        `Tree Depth` = 1:length(cdt$stability_diagnostics$jaccard_mean),
        `Jaccard SSI` = cdt$stability_diagnostics$jaccard_mean
      )
    }
  ) |>
    dplyr::bind_rows(.id = "Teacher Model")

  plt <- ggplot2::ggplot(ssi_df) +
    ggplot2::aes(x = `Tree Depth`, y = `Jaccard SSI`, color = `Teacher Model`) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::theme_classic()
  return(plt)
}
