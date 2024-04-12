#' annotation_matrices_list
#'
#' @description Calculate correlation between the features of top correlated modules
#' @param Cor_Prot_Metab A feature matrix with the first principal component of each protein cluster
#' @param cluster_assignments_Prot data frame containing cluster_assignments and Enrichr terms
#' @param cluster_assignments_metab data frame containing cluster_assignments
#' @param Prot_annotation
#' @param Metabolites_annotation (data frame with de annotation names of the metabolites: Should have a column called HMDB)
#' @param Prot_t A feature matrix (e.g. gene expression) with samples in columns and features (e.g. genes) in rows Row names must be unique.
#' @param metab_t A feature matrix (e.g. gene expression) with samples in columns and features (e.g. metabolites) in rows Row names must be unique.
#' @return Important features
#' @export
#'
annotation_matrices_list <- function(Cor_Prot_Metab, cluster_assignments_Prot, cluster_assignments_metab, Prot_annotation = NULL, metab_annotation = NULL, top_n = 5) {
  # Select the top n correlations
  Top_correlations <- Cor_Prot_Metab[order(-abs(Cor_Prot_Metab$Correlation)), ][1:top_n, ]

  # Initialize lists to store cluster assignments, annotation matrices, and expression matrices
  annotation_matrices_list <- list()


  # Loop through each top correlation and extract variables from clusters
  for (i in 1:top_n) {
    Prot_module <- Top_correlations[i, "Prot_module"]
    Metab_module <- Top_correlations[i, "Metab_module"]

    # Extract variables for Prot and Metab
    cluster_top_Prot <- cluster_assignments_Prot[stringr::str_detect(cluster_assignments_Prot$col, Prot_module), ]
    cluster_top_Metab <- cluster_assignments_metab[stringr::str_detect(cluster_assignments_metab$col, Metab_module), ]

    # Extract variables for Prot and Metab from their respective annotation matrices
    cluster_variables_Prot <- cluster_top_Prot$feature
    cluster_variables_Metab <- cluster_top_Metab$feature

    # Check if Prot_annotation matrix is provided
    if (!is.null(Prot_annotation)) {
      cluster_annotation_matrix_Prot <- Prot_annotation[Prot_annotation$Feature_ID %in% cluster_variables_Prot, , drop = FALSE]
      annotation_matrices_list[[paste("Annotation_Matrix_Cluster_Prot_", i)]] <- cluster_annotation_matrix_Prot
    }

    # Check if metab_annotation matrix is provided
    if (!is.null(metab_annotation)) {
      cluster_annotation_matrix_Metab <- metab_annotation[metab_annotation$`row ID` %in% cluster_variables_Metab, , drop = FALSE]
      annotation_matrices_list[[paste("Annotation_Matrix_Cluster_Metab_", i)]] <- cluster_annotation_matrix_Metab
    }

    # Return the lists
    return(list(
      annotation_matrices_list = annotation_matrices_list
    ))

  }


}
