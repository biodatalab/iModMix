test_that("annotation_matrices_list returns correct structure", {
  # Datos simulados
  Cor_Prot_Metab <- data.frame(
    Prot_module = c("blue", "green"),
    Metab_module = c("red", "yellow"),
    Correlation = c(0.9, 0.85),
    stringsAsFactors = FALSE
  )

  cluster_assignments_Prot <- data.frame(
    feature = c("P1", "P2"),
    col = c("blue", "green"),
    stringsAsFactors = FALSE
  )

  cluster_assignments_metab <- data.frame(
    feature = c("M1", "M2"),
    col = c("red", "yellow"),
    stringsAsFactors = FALSE
  )

  Prot_annotation <- data.frame(
    Feature_ID = c("P1", "P2"),
    Symbol = c("GeneA", "GeneB"),
    stringsAsFactors = FALSE
  )

  metab_annotation <- data.frame(
    Feature_ID = c("M1", "M2"),
    HMDB = c("HMDB001", "HMDB002"),
    stringsAsFactors = FALSE
  )

  # Correr función
  result <- annotation_matrices_list(
    Cor_Prot_Metab,
    cluster_assignments_Prot,
    cluster_assignments_metab,
    Prot_annotation,
    metab_annotation,
    top_n = 2
  )

  # Verificar estructura
  expect_type(result, "list")
  expect_true("annotation_matrices_list" %in% names(result))

  # Verificar que devuelve dos matrices (Prot + Metab por cada correlación)
  expect_equal(length(result$annotation_matrices_list), 4)

  # Verificar contenido esperado
  expect_true(any(result$annotation_matrices_list[[1]]$Feature_ID == "P1"))
  expect_true(any(result$annotation_matrices_list[[2]]$Feature_ID == "M1"))
})
