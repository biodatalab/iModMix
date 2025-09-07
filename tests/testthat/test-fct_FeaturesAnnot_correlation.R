test_that("FeaturesAnnot_correlation returns correct structure and content", {
  # Simulated correlation data
  Cor_Datai_Dataj <- data.frame(
    from = c("blue", "green"),
    to = c("red", "yellow"),
    value = c(0.9, 0.85),
    stringsAsFactors = FALSE
  )

  # Simulated cluster assignments
  cluster_assignments_D1 <- data.frame(
    feature = c("M1", "M2"),
    col = c("blue", "green"),
    stringsAsFactors = FALSE
  )
  cluster_assignments_D2 <- data.frame(
    feature = c("P1", "P2"),
    col = c("red", "yellow"),
    stringsAsFactors = FALSE
  )

  # Simulated expression matrices
  load_data1 <- matrix(rnorm(20), nrow = 5, ncol = 4)
  colnames(load_data1) <- c("M1", "M2", "X1", "X2")
  rownames(load_data1) <- paste0("Sample", 1:5)

  load_data2 <- matrix(rnorm(20), nrow = 5, ncol = 4)
  colnames(load_data2) <- c("P1", "P2", "Y1", "Y2")
  rownames(load_data2) <- paste0("Sample", 1:5)

  # Run the function
  result <- FeaturesAnnot_correlation(
    Cor_Datai_Dataj = Cor_Datai_Dataj,
    cluster_assignments_D1 = cluster_assignments_D1,
    cluster_assignments_D2 = cluster_assignments_D2,
    load_data1 = load_data1,
    load_data2 = load_data2,
    top_n = 2
  )

  # Check that result is a list with the right components
  expect_type(result, "list")
  expect_true(all(c(
    "Top_correlations",
    "cluster_assignments",
    "expression_matrices",
    "correlation_matrices",
    "Important_features",
    "correlation_List"
  ) %in% names(result)))

  # Check Top_correlations structure
  expect_s3_class(result$Top_correlations, "data.frame")
  expect_true(all(c("from", "to", "value") %in% colnames(result$Top_correlations)))
  expect_equal(nrow(result$Top_correlations), 2)

  # Check cluster assignments contain the expected sublists
  expect_true(any(grepl("cluster_assignments_D1_", names(result$cluster_assignments))))
  expect_true(any(grepl("cluster_assignments_D2_", names(result$cluster_assignments))))

  # Check expression matrices are matrices
  expect_true(all(sapply(result$expression_matrices, is.matrix)))

  # Check correlation matrices are matrices
  expect_true(all(sapply(result$correlation_matrices, is.matrix)))

  # Check Important_features lists contain characters
  expect_true(all(sapply(result$Important_features, function(x) is.character(x) || length(x) == 0)))

  # Check correlation_List are data.frames with expected columns
  expect_true(all(sapply(result$correlation_List, function(x) {
    is.data.frame(x) && all(c("Data1", "Data2", "Correlation") %in% colnames(x))
  })))
})
