# test-fct_partial_cors.R

test_that("partial_cors returns a matrix with correct dimensions and NA diagonal", {
  set.seed(123)
  mat <- matrix(rnorm(100), nrow = 10, ncol = 10)
  colnames(mat) <- paste0("Gene", 1:10)
  rownames(mat) <- paste0("Sample", 1:10)

  result <- partial_cors(mat, rho = 0.25)

  # check dimensions
  expect_equal(dim(result), c(10, 10))
  # diagonal should be NA
  expect_true(all(is.na(diag(result))))
})

test_that("partial_cors handles missing values correctly", {
  set.seed(123)
  mat <- matrix(rnorm(100), nrow = 10, ncol = 10)
  mat[sample(length(mat), 5)] <- NA
  colnames(mat) <- paste0("Gene", 1:10)
  rownames(mat) <- paste0("Sample", 1:10)

  result <- partial_cors(mat, rho = 0.25)

  expect_equal(dim(result), c(10, 10))
  expect_true(all(is.na(diag(result))))
  expect_true(is.numeric(result[!is.na(result)]))
})

test_that("partial_cors scales the data correctly", {
  set.seed(123)
  mat <- scale(matrix(rnorm(100), nrow = 10, ncol = 10))
  colnames(mat) <- paste0("Gene", 1:10)
  rownames(mat) <- paste0("Sample", 1:10)

  result <- partial_cors(mat, rho = 0.25)

  expect_equal(dim(result), c(10, 10))
  expect_true(all(is.na(diag(result))))
  expect_true(all(abs(result[upper.tri(result, diag = FALSE)]) <= 1))
})

