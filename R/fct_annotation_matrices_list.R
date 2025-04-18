#' annotation_matrices_list
#'
#' @description Calculate correlation between the features of top correlated modules.
#' @param Cor_Prot_Metab A data frame with the first principal component of each protein cluster and their correlations.
#' @param cluster_assignments_Prot A data frame containing cluster assignments and Enrichr terms for proteins.
#' @param cluster_assignments_metab A data frame containing cluster assignments for metabolites.
#' @param Prot_annotation A data frame with the annotation names of the proteins. Should have a column called Symbol.
#' @param metab_annotation A data frame with the annotation names of the metabolites. Should have a column called HMDB.
#' @param top_n The number of top correlations to select. Default is 5.
#' @return A list containing annotation matrices for the top correlated modules.
#' @export
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
