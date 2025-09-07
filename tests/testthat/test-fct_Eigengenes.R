test_that("Eigengenes returns correct structure", {
  set.seed(123)
  # Simulate a small expression matrix
  mat <- matrix(rnorm(100), nrow = 10, ncol = 10)
  colnames(mat) <- paste0("Gene", 1:10)
  rownames(mat) <- paste0("Sample", 1:10)

  # Simulate cluster assignments
  clusters <- rep(1:2, each = 5)

  # Run Eigengenes function
  result <- Eigengenes(load_data = mat, cluster_assignments = clusters)

  # Check that result is a list with expected names
  expect_true(is.list(result))
  expect_true(all(c("module_eigenmetab_List_Me", "module_eigenmetab_Me") %in% names(result)))

  # Check that module_eigenmetab_Me is a matrix
  expect_true(is.matrix(result$module_eigenmetab_Me))

  # Check that number of rows matches number of samples
  expect_equal(nrow(result$module_eigenmetab_Me), nrow(mat))

  # Check that number of columns matches number of unique clusters
  expect_equal(ncol(result$module_eigenmetab_Me), length(unique(clusters)))
})
