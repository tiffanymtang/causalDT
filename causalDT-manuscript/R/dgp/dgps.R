# General subgroup DGP constructor function
generate_subgroup_dgp <- function(n = 2000,
                                  p = 10,
                                  X_fun,
                                  Z_fun,
                                  tau_fun,
                                  tau_err_sd = 0,
                                  tau_heritability = NULL,
                                  beta = NULL,
                                  y_err_sd = 1,
                                  y_heritability = NULL,
                                  ...) {
  dot_args <- rlang::dots_list(...)
  X_args <- dot_args[stringr::str_starts(names(dot_args), ".X_")]
  Z_args <- dot_args[stringr::str_starts(names(dot_args), ".Z_")]
  tau_args <- dot_args[stringr::str_starts(names(dot_args), ".tau_")]

  if (length(X_args) == 0) {
    X_args <- NULL
  } else {
    names(X_args) <- stringr::str_replace(names(X_args), "^.X_", "")
  }
  if (length(Z_args) == 0) {
    Z_args <- NULL
  } else {
    names(Z_args) <- stringr::str_replace(names(Z_args), "^.Z_", "")
  }
  if (length(tau_args) == 0) {
    tau_args <- NULL
  } else {
    names(tau_args) <- stringr::str_replace(names(tau_args), "^.tau_", "")
  }

  X <- do.call(X_fun, args = c(list(n = n, p = p), X_args))
  Z <- rbinom(n, 1, do.call(Z_fun, args = c(list(X), Z_args)))
  tau_out <- do.call(tau_fun, args = c(list(X), tau_args))
  if (!is.null(tau_heritability)) {
    tau_err_sd <- sqrt(
      var(tau_out$result) * (1 - tau_heritability) / tau_heritability
    )
  }
  tau <- tau_out$result + rnorm(n, sd = tau_err_sd)
  Y <- Z * tau
  if (!is.null(beta)) {
    Y <- Y + X %*% beta
  }
  if (!is.null(y_heritability)) {
    y_err_sd <- sqrt(
      var(Y) * (1 - y_heritability) / y_heritability
    )
  }
  Y <- Y + rnorm(n, sd = y_err_sd)
  out <- list(
    X = X,
    Y = Y,
    Z = Z,
    tau = tau,
    tau_denoised = tau_out$result,
    true_thresholds = tau_out$thresholds
  )
  return(out)
}


# Real world data
rwd_dgp <- function(name, subsample = 1, permute = FALSE) {

  if (stringr::str_starts(name, "aids")) {
    data <- speff2trial::ACTG175 |>
      dplyr::filter(arms %in% c(0, 2))
    if (subsample < 1) {
      subsample_idx <- sample(1:nrow(data), floor(nrow(data) * subsample))
      data <- data[subsample_idx, , drop = FALSE]
    }
    vars_all <- c(
      "age", "wtkg", "hemo", "homo", "drugs", "karnof", "race", "gender",
      "symptom", "preanti", "strat", "str2", "oprior", "z30", "zprior",
      "cd80"
    )
    if (name == "aids_full") {
      X <- data |>
        dplyr::select(tidyselect::all_of(vars_all))
    } else if (name == "aids_small") {
      X <- data |>
        dplyr::select(
          tidyselect::all_of(
            setdiff(vars_all, c("str2", "oprior", "z30", "zprior"))
          )
        )
    }
    Z <- data |>
      dplyr::pull(treat)
    Y <- data |>
      dplyr::pull(cens)
  } else {
    stop("Unknown data set name.")
  }

  if (permute) {
    permute_idx <- sample(1:nrow(X), size = nrow(X), replace = FALSE)
    X <- X[permute_idx, , drop = FALSE]
    Z <- Z[permute_idx]
  }

  out <- list(
    X = as.matrix(X),
    Y = Y,
    Z = Z,
    tau = NULL,
    true_thresholds = NULL
  )
}
