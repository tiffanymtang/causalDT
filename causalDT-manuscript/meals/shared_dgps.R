gaussian_X_unbiased_Z_and <- create_dgp(
  .dgp_fun = generate_subgroup_dgp,
  .name = "AND DGP",
  n = 500, p = 10,
  X_fun = generate_gaussian_X,
  Z_fun = generate_unbiased_Z,
  tau_fun = generate_tau_and,
  tau_heritability = 0.2,
  y_err_sd = 0.1
)

gaussian_X_unbiased_Z_or <- create_dgp(
  .dgp_fun = generate_subgroup_dgp,
  .name = "OR DGP",
  n = 500, p = 10,
  X_fun = generate_gaussian_X,
  Z_fun = generate_unbiased_Z,
  tau_fun = generate_tau_or,
  tau_heritability = 0.2,
  y_err_sd = 0.1
)

gaussian_X_unbiased_Z_additive <- create_dgp(
  .dgp_fun = generate_subgroup_dgp,
  .name = "Additive DGP",
  n = 500, p = 10,
  X_fun = generate_gaussian_X,
  Z_fun = generate_unbiased_Z,
  tau_fun = generate_tau_additive,
  tau_heritability = 0.2,
  y_err_sd = 0.1
)

gaussian_X_unbiased_Z_and_cov <- create_dgp(
  .dgp_fun = generate_subgroup_dgp,
  .name = "AND with Linear Covariates DGP",
  n = 500, p = 10,
  X_fun = generate_gaussian_X,
  Z_fun = generate_unbiased_Z,
  tau_fun = generate_tau_and,
  tau_heritability = 0.2,
  beta = c(0, 0, 1, 1, 0, 0, 0, 0, 0, 0),
  y_err_sd = 0.1
)

gaussian_X_unbiased_Z_or_cov <- create_dgp(
  .dgp_fun = generate_subgroup_dgp,
  .name = "OR with Linear Covariates DGP",
  n = 500, p = 10,
  X_fun = generate_gaussian_X,
  Z_fun = generate_unbiased_Z,
  tau_fun = generate_tau_or,
  tau_heritability = 0.2,
  beta = c(0, 0, 1, 1, 0, 0, 0, 0, 0, 0),
  y_err_sd = 0.1
)

gaussian_X_unbiased_Z_additive_cov <- create_dgp(
  .dgp_fun = generate_subgroup_dgp,
  .name = "Additive with Linear Covariates DGP",
  n = 500, p = 10,
  X_fun = generate_gaussian_X,
  Z_fun = generate_unbiased_Z,
  tau_fun = generate_tau_additive,
  tau_heritability = 0.2,
  beta = c(0, 0, 1, 1, 0, 0, 0, 0, 0, 0),
  y_err_sd = 0.1
)

aids_dgp <- create_dgp(
  .dgp_fun = rwd_dgp,
  .name = "AIDS DGP",
  name = "aids_small",
  subsample = 0.75
)
