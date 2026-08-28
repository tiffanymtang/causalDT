rspline <- function(x, w, y, ...) {
  spline_fits <- purrr::map(
    1:ncol(x),
    ~ splines::ns(x[, .x], df = 3)
  )
  spline_x_ls <- purrr::map(
    spline_fits,
    function(.fit) {
      matrix(.fit, nrow = nrow(x), 3)
    }
  )
  spline_x <- do.call(cbind, spline_x_ls)
  fit <- rlearner::rlasso(x = spline_x, w = w, y = y, ...)
  fit$spline_object <- spline_fits
  return(fit)
}


predict_rspline <- function(object, newx = NULL, ...) {
  spline_newx_ls <- purrr::map2(
    object$spline_object, 1:ncol(newx),
    function(.fit, .col) {
      predict(.fit, newx[, .col])
    }
  )
  spline_newx <- do.call(cbind, spline_newx_ls)
  predict(object, spline_newx, ...)
}
