test_that("perform_classification returns NULL when phenotype_variable is NULL", {
  eigengene_data <- as.data.frame(matrix(rnorm(20), nrow = 5, ncol = 4))
  colnames(eigengene_data) <- paste0("ME", 1:4)
  rownames(eigengene_data) <- paste0("Sample", 1:5)
  metadata <- data.frame(Sample = paste0("Sample", 1:5),
                         Phenotype = rep(c("A", "B"), length.out = 5),
                         stringsAsFactors = FALSE)

  result <- perform_classification(eigengene_data, metadata, phenotype_variable = NULL)
  expect_null(result)
})

test_that("perform_classification works with binary phenotype", {
  set.seed(123)
  eigengene_data <- as.data.frame(matrix(rnorm(50), nrow = 10, ncol = 5))
  colnames(eigengene_data) <- paste0("ME", 1:5)
  rownames(eigengene_data) <- paste0("Sample", 1:10)

  metadata <- data.frame(
    Sample = paste0("Sample", 1:10),
    Phenotype = rep(c("A", "B"), each = 5),
    stringsAsFactors = FALSE
  )

  result <- perform_classification(eigengene_data, metadata, phenotype_variable = "Phenotype", significance_threshold = 0.05)

  expect_type(result, "list")
  expect_true("result" %in% names(result))
  expect_true("plots" %in% names(result))
  expect_s3_class(result$result, "data.frame")
  expect_true(all(c("Variable", "Class", "Result_t", "Result_pValue", "Adjusted_pValue") %in% colnames(result$result)))
  expect_true(all(sapply(result$plots, function(x) inherits(x, "ggplot"))))
})

test_that("perform_classification works with multiclass phenotype", {
  set.seed(321)
  eigengene_data <- as.data.frame(matrix(rnorm(45), nrow = 9, ncol = 5))
  colnames(eigengene_data) <- paste0("ME", 1:5)
  rownames(eigengene_data) <- paste0("Sample", 1:9)

  metadata <- data.frame(
    Sample = paste0("Sample", 1:9),
    Phenotype = rep(c("A", "B", "C"), each = 3),
    stringsAsFactors = FALSE
  )

  result <- perform_classification(eigengene_data, metadata, phenotype_variable = "Phenotype", significance_threshold = 0.1)

  expect_type(result, "list")
  expect_true("result" %in% names(result))
  expect_true("plots" %in% names(result))
  expect_s3_class(result$result, "data.frame")
  expect_true(all(c("Variable", "Class", "Result_t", "Result_pValue", "Adjusted_pValue") %in% colnames(result$result)))
  expect_true(all(sapply(result$plots, function(x) inherits(x, "ggplot"))))
})

