test_that("hierarchical_cluster works with TOM = TRUE", {
  set.seed(123)
  # simulate matrix and compute parcor
  mat <- matrix(rnorm(100), nrow = 10, ncol = 10)
  colnames(mat) <- paste0("Gene", 1:10)
  rownames(mat) <- paste0("Sample", 1:10)

  cov_mat <- cov(mat)
  parcor_mat <- glassoFast::glassoFast(cov_mat, rho = 0.25)$wi
  diag(parcor_mat) <- NA
  colnames(parcor_mat) <- rownames(parcor_mat) <- paste0("Gene", 1:10)

  res <- hierarchical_cluster(parcor_mat, tom = TRUE, min_module_size = 2)

  expect_type(res, "list")
  expect_true("tom_diss" %in% names(res))
  expect_s3_class(res$hclustTree, "hclust")
  expect_true(all(c("cluster", "feature", "col") %in% names(res$cluster_assignments)))
  expect_equal(res$mod_count, max(res$dynamicMods_numeric, na.rm = TRUE))
})

test_that("hierarchical_cluster works with TOM = FALSE", {
  set.seed(456)
  mat <- matrix(rnorm(100), nrow = 10, ncol = 10)
  colnames(mat) <- paste0("Gene", 1:10)
  rownames(mat) <- paste0("Sample", 1:10)

  cov_mat <- cov(mat)
  parcor_mat <- glassoFast::glassoFast(cov_mat, rho = 0.25)$wi
  diag(parcor_mat) <- NA
  colnames(parcor_mat) <- rownames(parcor_mat) <- paste0("Gene", 1:10)

  res <- hierarchical_cluster(parcor_mat, tom = FALSE, min_module_size = 2)

  expect_type(res, "list")
  expect_false("tom_diss" %in% names(res)) # no tom_diss expected
  expect_s3_class(res$hclustTree, "hclust")
  expect_true(all(c("cluster", "feature", "col") %in% names(res$cluster_assignments)))
  expect_equal(res$mod_count, max(res$dynamicMods_numeric, na.rm = TRUE))
})

