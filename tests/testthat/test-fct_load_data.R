test_that("load_data works with simulated matrix", {
  set.seed(123)
  mat <- matrix(rnorm(1000), nrow = 10, ncol = 100)
  colnames(mat) <- paste0("Gene", 1:100)
  rownames(mat) <- paste0("Sample", 1:10)
  mat[sample(length(mat), 50)] <- NA  # introduce some NAs

  Expression_mat <- cbind(Feature_ID = paste0("F", 1:10), mat)
  Expression_mat <- as.data.frame(Expression_mat)
  Expression_mat$Feature_ID <- as.character(Expression_mat$Feature_ID)

  processed <- load_data(Expression_mat)

  expect_true(is.matrix(processed))
  expect_equal(nrow(processed), 100)        # features are transposed, so rows = samples
  expect_true(all(!is.na(processed)))       # missing values should be imputed
  expect_true(all(abs(colMeans(processed)) < 1e-6))  # centered by scale()
})

test_that("load_data filters rows with too many missing values", {
  mat <- matrix(NA, nrow = 5, ncol = 10)
  colnames(mat) <- paste0("Gene", 1:10)
  rownames(mat) <- paste0("Sample", 1:5)

  Expression_mat <- cbind(Feature_ID = paste0("F", 1:5), mat)
  Expression_mat <- as.data.frame(Expression_mat)
  Expression_mat$Feature_ID <- as.character(Expression_mat$Feature_ID)

  # All rows should be filtered out -> expect an error in imputation
  expect_error(load_data(Expression_mat))
})

test_that("load_data limits to 20000 features", {
  set.seed(42)
  mat <- matrix(rnorm(21 * 21000), nrow = 21, ncol = 21000)
  colnames(mat) <- paste0("Gene", 1:21000)
  rownames(mat) <- paste0("Sample", 1:21)

  Expression_mat <- cbind(Feature_ID = paste0("F", 1:21), mat)
  Expression_mat <- as.data.frame(Expression_mat)
  Expression_mat$Feature_ID <- as.character(Expression_mat$Feature_ID)

  processed <- load_data(Expression_mat)
  expect_true(ncol(processed) <= 20000)
})

test_that("load_data throws error with invalid input", {
  bad_input <- data.frame(x = 1:5, y = 6:10)
  expect_error(load_data(bad_input))
})
