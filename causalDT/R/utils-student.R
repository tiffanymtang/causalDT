#' Get decision paths from an rpart model.
#'
#' @description
#' Return the decision paths for each leaf node in an `rpart` model as character
#' strings.
#'
#' @inheritParams shared_args
#'
#' @returns
#' A list of character vectors, where each element corresponds to the decision
#' path for a leaf node in the `rpart_fit` model.
#'
#' @export
get_rpart_paths <- function(rpart_fit) {
  leaf_node_ids <- rpart_fit$frame |>
    tibble::rownames_to_column("id") |>
    dplyr::filter(var == "<leaf>") |>
    dplyr::pull("id") |>
    as.numeric()
  subgroups <- rpart::path.rpart(rpart_fit, leaf_node_ids, print.it = FALSE) |>
    purrr::map(~ setdiff(.x, "root")) |>
    purrr::compact()
  return(subgroups)
}


#' Get split information from an rpart model.
#'
#' @description
#' Return the split information for each node in an `rpart` model as a data frame.
#'
#' @inheritParams shared_args
#' @param digits Number of digits to round the split values to.
#'
#' @returns A data.frame with information regarding the feature/threshold used
#'   for each split in the `rpart` model.
#'
#' @export
get_rpart_tree_info <- function(rpart_fit, digits = getOption("digits")) {
  out <- NULL
  splits <- rpart_fit$splits
  if (!is.null(splits) && isTRUE(nrow(splits) > 0)) {
    ff <- rpart_fit$frame
    is.leaf <- ff$var == "<leaf>"
    n <- nrow(splits)
    nn <- ff$ncompete + ff$nsurrogate + !is.leaf
    ix <- cumsum(c(1L, nn))
    ix_prim <- unlist(
      mapply(ix, ix + c(ff$ncompete, 0), FUN = seq, SIMPLIFY = F)
    )
    type <- rep.int("surrogate", n)
    type[ix_prim[ix_prim <= n]] <- "primary"
    type[ix[ix <= n]] <- "main"
    left <- character(nrow(splits))
    side <- splits[, 2L]
    for (i in seq_along(left)) {
      left[i] <- if (side[i] == -1L)
        paste("<", format(signif(splits[i, 4L], digits)))
      else if (side[i] == 1L)
        paste(">=", format(signif(splits[i, 4L], digits)))
      else {
        catside <- rpart_fit$csplit[splits[i, 4L], 1:side[i]]
        paste(c("L", "-", "R")[catside], collapse = "", sep = "")
      }
    }
    out <- cbind(
      data.frame(
        var = rownames(splits),
        type = type,
        node = rep(as.integer(row.names(ff)), times = nn),
        ix = rep(seq_len(nrow(ff)), nn),
        left = left
      ),
      as.data.frame(splits, row.names = F)
    ) |>
      dplyr::filter(type == "main") |>
      dplyr::rename(thr = index) |>
      dplyr::mutate(
        cat_thr = purrr::pmap_chr(
          list(v = var, l = left),
          function(v, l) {
            if (grepl("[RL]", l)) {
              l_split <- strsplit(l, "")[[1]]
              R_idx <- which(l_split == "R")[1]
              L_idx <- which(l_split == "L")[1]
              if (R_idx < L_idx) {
                th <- levels(X[[v]])[L_idx]
              } else {
                th <- levels(X[[v]])[R_idx]
              }
            } else {
              return(NA)
            }
          }
        )
      )
  }
  return(out)
}


#' Get list of rules from a party model.
#'
#' @description
#' This is a copy of \code{partykit:::.list.rules.party()} that is exported for
#' use in the causal distillation tree framework.
#'
#' @keywords internal
.list.rules.party <- function(x, i = NULL, ...) {
  if (is.null(i)) {
    i <- partykit::nodeids(x, terminal = TRUE)
  }
  if (length(i) > 1) {
    ret <- sapply(i, .list.rules.party, x = x)
    names(ret) <- if (is.character(i)) i else names(x)[i]
    return(ret)
  }
  if (is.character(i) && !is.null(names(x))) {
    i <- which(names(x) %in% i)
  }
  stopifnot(length(i) == 1 & is.numeric(i))
  stopifnot(i <= length(x) & i >= 1)
  i <- as.integer(i)
  dat <- partykit::data_party(x, i)
  if (!is.null(x$fitted)) {
    findx <- which("(fitted)" == names(dat))[1]
    fit <- dat[, findx:ncol(dat), drop = FALSE]
    dat <- dat[, -(findx:ncol(dat)), drop = FALSE]
    if (ncol(dat) == 0) {
      dat <- x$data
    }
  } else {
    fit <- NULL
    dat <- x$data
  }

  rule <- c()

  recFun <- function(node) {
    if (partykit::id_node(node) == i) return(NULL)
    kid <- sapply(partykit::kids_node(node), partykit::id_node)
    whichkid <- max(which(kid <= i))
    split <- partykit::split_node(node)
    ivar <- partykit::varid_split(split)
    svar <- names(dat)[ivar]
    index <- partykit::index_split(split)
    if (is.factor(dat[, svar])) {
      if (is.null(index)) {
        index <- ((1:nlevels(dat[, svar])) > breaks_split(split)) + 1
      }
      slevels <- levels(dat[, svar])[index == whichkid]
      srule <- paste(
        svar, " %in% c(\"",
        paste(slevels, collapse = "\", \"", sep = ""), "\")",
        sep = ""
      )
    } else {
      if (is.null(index)) index <- 1:length(kid)
      breaks <- cbind(
        c(-Inf, partykit::breaks_split(split)),
        c(partykit::breaks_split(split), Inf)
      )
      sbreak <- breaks[index == whichkid,]
      right <- partykit::right_split(split)
      srule <- c()
      if (is.finite(sbreak[1])) {
        srule <- c(srule, paste(svar, ifelse(right, ">", ">="), sbreak[1]))
      }
      if (is.finite(sbreak[2])) {
        srule <- c(srule, paste(svar, ifelse(right, "<=", "<"), sbreak[2]))
      }
      srule <- paste(srule, collapse = " & ")
    }
    rule <<- c(rule, srule)
    return(recFun(node[[whichkid]]))
  }
  node <- recFun(partykit::node_party(x))
  paste(rule, collapse = " & ")
}


#' Get decision paths from a party model.
#'
#' @description
#' Return the decision paths for each leaf node in a `party` model as character
#' strings.
#'
#' @inheritParams shared_args
#'
#' @returns
#' A list of character vectors, where each element corresponds to the decision
#' path for a leaf node in the `party_fit` model.
#'
#' @keywords internal
get_party_paths <- function(party_fit) {
  purrr::map(
    .list.rules.party(party_fit),
    ~ tibble::tibble(
      subgroup = list(stringr::str_split(.x, " & ")[[1]])
    )
  ) |>
    dplyr::bind_rows(.id = "leaf_id") |>
    dplyr::mutate(
      leaf_id = as.numeric(leaf_id)
    )
}


#' Get depth of each node in a party model.
#'
#' @inheritParams shared_args
#'
#' @keywords internal
get_party_node_depths <- function(party_fit, return_features = FALSE) {
  printed_tree <- capture.output(party_fit)
  id_counter <- 1
  depths <- rep(NA, length(party_fit))
  names(depths) <- 1:length(party_fit)
  features <- rep(NA, length(party_fit))
  names(features) <- 1:length(party_fit)
  for (idx in seq_along(printed_tree)) {
    if (grepl("\\[[0-9]+\\]", printed_tree[idx])) {
      depths[id_counter] <- stringr::str_count(printed_tree[idx], "\\|")
      if (return_features) {
        features[id_counter] <- stringr::str_extract(
          printed_tree[idx],
          "(?<=\\]).*?(?=<|>)"
        ) |>
          stringr::str_trim()
      }
      id_counter <- id_counter + 1
    }
  }
  if (return_features) {
    return(
      tibble::tibble(
        depth = depths,
        feature = features
      )
    )
  } else {
    return(depths)
  }
}
