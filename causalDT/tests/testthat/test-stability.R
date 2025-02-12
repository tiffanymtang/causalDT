test_that("jaccard stability works", {
  # compute jaccard stability manually
  true_jaccard <- function(x, y) {
    n <- length(x)
    G <- length(unique(x))
    Cx <- matrix(0, n, n)
    Cy <- matrix(0, n, n)
    for (i in 1:n) {
      for (j in 1:n) {
        if (i == j) {
          next
        }
        if (x[i] == x[j]) {
          Cx[i, j] <- 1
        }
        if (y[i] == y[j]) {
          Cy[i, j] <- 1
        }
      }
    }
    j_subgroup <- 0
    for (g in c("x", "y")) {
      for (gp in 0:(G - 1)) {
        if (g == "x") {
          in_gp <- x == gp
        } else if (g == "y") {
          in_gp <- y == gp
        }
        N11 <- sum((Cx == 1) & (Cy == 1) & in_gp)
        N10 <- sum((Cx == 1) & (Cy == 0) & in_gp)
        N01 <- sum((Cx == 0) & (Cy == 1) & in_gp)
        j_subgroup <- j_subgroup + (1 / (2 * G)) * (N11 / (N11 + N10 + N01))
      }
    }
    return(j_subgroup)
  }

  G <- 4
  x <- rep(1:G, each = 10) - 1
  y <- c(rep(1, 30), rep(2, 4), rep(3, 4), rep(4, 2)) - 1
  expect_equal(jaccardSSI(x, y), true_jaccard(x, y))

  n <- 100
  x <- sample(1:G, size = n, replace = TRUE) - 1
  y <- sample(1:G, size = n, replace = TRUE) - 1
  expect_equal(jaccardSSI(x, y), true_jaccard(x, y))
})
