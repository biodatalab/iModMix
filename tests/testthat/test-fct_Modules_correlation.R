test_that("Modules_correlation works with toy data", {
  set.seed(123)

  # Simulated eigengenes
  eig1 <- matrix(rnorm(50), nrow = 10, ncol = 5)
  colnames(eig1) <- paste0("ME", 1:5)

  eig2 <- matrix(rnorm(50), nrow = 10, ncol = 5)
  colnames(eig2) <- paste0("ME", 1:5)

  # Simulated cluster assignments
  cluster1 <- data.frame(
    feature = paste0("F", 1:5),
    cluster = paste0("cluster_", 1:5),
    col = RColorBrewer::brewer.pal(5, "Set1"),
    stringsAsFactors = FALSE
  )

  cluster2 <- data.frame(
    feature = paste0("F", 6:10),
    cluster = paste0("cluster_", 6:10),
    col = RColorBrewer::brewer.pal(5, "Set2"),
    stringsAsFactors = FALSE
  )

  # Run function
  result <- Modules_correlation(
    eigengenes_list = list(eig1, eig2),
    cluster_list = list(cluster1, cluster2),
    threshold = 0.3
  )

  # Check structure
  expect_type(result, "list")
  expect_true(all(c("Top_cor_Prot_metab", "Cor_list", "Correlation_Plot",
                    "nodes", "edges", "n") %in% names(result)))

  # Check n
  expect_equal(result$n, 2)

  # Check correlations
  expect_true(all(result$Top_cor_Prot_metab$value >= -1 &
                    result$Top_cor_Prot_metab$value <= 1))

  # Check nodes
  expect_true(all(c("id", "label", "value", "shape", "title", "color", "shadow") %in% colnames(result$nodes)))

  # Check edges
  expect_true(all(c("from", "to", "label", "dashes", "title", "smooth", "shadow") %in% colnames(result$edges)))
})
