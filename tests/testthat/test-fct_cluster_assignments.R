test_that("cluster_assignments works without PhenoData", {
  cluster_df <- data.frame(
    feature = paste0("F", 1:3),
    cluster = paste0("cluster_", 1:3),
    col = RColorBrewer::brewer.pal(3, "Set1"),
    stringsAsFactors = FALSE
  )

  result <- cluster_assignments(cluster_df)

  expect_s3_class(result, "data.frame")
  expect_equal(colnames(result), c("feature", "cluster", "col", "feature_name"))
  expect_equal(result$feature_name, cluster_df$feature)
})

test_that("cluster_assignments works with PhenoData and no selected_columns", {
  cluster_df <- data.frame(
    feature = paste0("F", 1:3),
    cluster = paste0("cluster_", 1:3),
    col = RColorBrewer::brewer.pal(3, "Set1"),
    stringsAsFactors = FALSE
  )

  pheno_df <- data.frame(
    Feature_ID = paste0("F", 1:3),
    Symbol = c("GeneA", "GeneB", ""),
    stringsAsFactors = FALSE
  )

  result <- cluster_assignments(cluster_df, PhenoData = pheno_df)

  expect_s3_class(result, "data.frame")
  expect_equal(colnames(result), c("feature", "cluster", "col", "feature_name"))
  expect_equal(result$feature_name, c("GeneA", "GeneB", "F3"))
})

test_that("cluster_assignments works with PhenoData and selected_columns", {
  cluster_df <- data.frame(
    feature = paste0("F", 1:3),
    cluster = paste0("cluster_", 1:3),
    col = RColorBrewer::brewer.pal(3, "Set1"),
    stringsAsFactors = FALSE
  )

  pheno_df <- data.frame(
    Feature_ID = paste0("F", 1:3),
    Symbol = c("GeneA", "GeneB", "GeneC"),
    Description = paste("Description", 1:3),
    stringsAsFactors = FALSE
  )

  result <- cluster_assignments(cluster_df, PhenoData = pheno_df, selected_columns = "Description")

  expect_s3_class(result, "data.frame")
  expect_equal(colnames(result), c("feature", "cluster", "col", "feature_name", "Description"))
  expect_equal(result$Description, paste("Description", 1:3))
})
