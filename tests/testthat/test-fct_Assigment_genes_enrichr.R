test_that("Assigment_genes_enrichr returns expected columns", {
  # Simulated cluster assignments
  cluster_assignments <- data.frame(
    feature = c("F1", "F2", "F3", "F4"),
    cluster = c("cluster_1", "cluster_1", "cluster_2", "cluster_2"),
    col = c("#FF0000", "#FF0000", "#00FF00", "#00FF00"),
    feature_name = c("TP53", "BRCA1", "EGFR", "MYC"),
    stringsAsFactors = FALSE
  )

  # Temporarily set enrichR.live option to FALSE to skip actual web call
  options(enrichR.live = FALSE)

  result <- Assigment_genes_enrichr(
    cluster_assignments_ProtGenes = cluster_assignments,
    database = "GO_Biological_Process_2023"
  )

  # Check that returned object is a data.frame
  expect_s3_class(result, "data.frame")

  # Check that all expected new columns exist
  expected_cols <- c(
    "feature", "cluster", "col", "feature_name",
    "enriched_Term", "enriched_Overlap", "enriched_Genes",
    "enriched_P.value", "enriched_Adjusted.P.value"
  )
  expect_true(all(expected_cols %in% colnames(result)))

  # When enrichR.live = FALSE, all enriched columns should be NA
  enriched_cols <- setdiff(colnames(result), c("feature","cluster","col","feature_name"))
  expect_true(all(sapply(result[enriched_cols], function(x) all(is.na(x)))))
})
