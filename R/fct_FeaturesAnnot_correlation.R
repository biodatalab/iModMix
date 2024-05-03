#' FeaturesAnnot_correlation
#'
#' @description Calculate correlation between the features of top correlated modules
#' @param Cor_Prot_Metab A feature matrix with the first principal component of each protein cluster
#' @param cluster_assignments_Prot data frame containing cluster_assignments and Enrichr terms
#' @param cluster_assignments_metab data frame containing cluster_assignments
#' @param Prot_annotation
#' @param Metabolites_annotation (data frame with de annotation names of the metabolites: Should have a column called KEGG)
#' @param Prot_t A feature matrix (e.g. gene expression) with samples in columns and features (e.g. genes) in rows Row names must be unique.
#' @param metab_t A feature matrix (e.g. gene expression) with samples in columns and features (e.g. metabolites) in rows Row names must be unique.
#' @return Important features
#' @export

FeaturesAnnot_correlation <- function(Cor_Prot_Metab, cluster_assignments_Prot, cluster_assignments_metab,
                                      #Prot_annotation = NULL, metab_annotation = NULL,
                                      Prot_t, metab_t, top_n = 5)  {
  # Select the top n correlations
  Top_correlations <- Cor_Prot_Metab[order(-abs(Cor_Prot_Metab$Correlation)), ][1:top_n, ]

  # Remove the "ME" prefix from Prot_module and Metab_module columns
  Top_correlations[c("Prot_module", "Metab_module")] <- lapply(Top_correlations[c("Prot_module", "Metab_module")], function(x) sub("^ME", "", x))


  # Print the Top_correlations
  #print(Top_correlations[,c("Prot_module", 'Metab_module', 'Correlation', 'Prot_count', 'Metab_count' )])

  # Initialize lists to store cluster assignments, annotation matrices, and expression matrices
  cluster_assignments_list <- list()
  annotation_matrices_list <- list()
  expression_matrices_list <- list()
  correlation_matrices_list <- list()
  Important_features_list <- list()
  correlation_List_list <- list()

  # Loop through each top correlation and extract variables from clusters
  for (i in 1:top_n) {
    Prot_module <- Top_correlations[i, "Prot_module"]
    print("Prot_module")
    print(Prot_module)
    Metab_module <- Top_correlations[i, "Metab_module"]



    # Extract variables for Prot and Metab
    # cluster_top_Prot <- cluster_assignments_Prot[stringr::str_detect(cluster_assignments_Prot$col, Prot_module), ]
    # cluster_top_Metab <- cluster_assignments_metab[stringr::str_detect(cluster_assignments_metab$col, Metab_module), ]
    cluster_top_Prot <- cluster_assignments_Prot[grepl(Prot_module, cluster_assignments_Prot$col), ]
    print("cluster_top_Prot")
    print(cluster_top_Prot)
    cluster_top_Metab <- cluster_assignments_metab[grepl(Metab_module, cluster_assignments_metab$col), ]


    # Store cluster assignments
    cluster_assignments_list[[paste("Cluster_assignments_Prot_", i)]] <- cluster_top_Prot
    cluster_assignments_list[[paste("Cluster_assignments_Metab_", i)]] <- cluster_top_Metab

    # Extract variables for Prot and Metab from their respective annotation matrices
    cluster_variables_Prot <- cluster_top_Prot$feature
    cluster_variables_Metab <- cluster_top_Metab$feature


    # # Filter annotation matrices based on Feature_ID values
    # cluster_annotation_matrix_Prot <- Prot_annotation[Prot_annotation$Feature_ID %in% cluster_variables_Prot, , drop = FALSE]
    # cluster_annotation_matrix_Metab <- metab_annotation[metab_annotation$`row ID` %in% cluster_variables_Metab, , drop = FALSE]
    #
    # # Check if Prot_annotation matrix is provided
    # if (!is.null(Prot_annotation)) {
    #   cluster_annotation_matrix_Prot <- Prot_annotation[Prot_annotation$Feature_ID %in% cluster_variables_Prot, , drop = FALSE]
    #   annotation_matrices_list[[paste("Annotation_Matrix_Cluster_Prot_", i)]] <- cluster_annotation_matrix_Prot
    # }
    #
    # # Check if metab_annotation matrix is provided
    # if (!is.null(metab_annotation)) {
    #   cluster_annotation_matrix_Metab <- metab_annotation[metab_annotation$`row ID` %in% cluster_variables_Metab, , drop = FALSE]
    #   annotation_matrices_list[[paste("Annotation_Matrix_Cluster_Metab_", i)]] <- cluster_annotation_matrix_Metab
    # }

    # Filter expression matrices based on column names
    cluster_expression_matrix_Prot <- Prot_t[, colnames(Prot_t) %in% cluster_variables_Prot, drop = FALSE]
    cluster_expression_matrix_Metab <- metab_t[, colnames(metab_t) %in% cluster_variables_Metab, drop = FALSE]
    # # Store cluster-specific expression matrices
    # expression_matrices_list[[paste("Expression_Matrix_Cluster_Prot_", i)]] <- cluster_expression_matrix_Prot
    # expression_matrices_list[[paste("Expression_Matrix_Cluster_Metab_", i)]] <- cluster_expression_matrix_Metab
    # Calculate and store the correlation matrix between Prot and Metab expression matrices
    correlation_matrix <- cor(cluster_expression_matrix_Prot, cluster_expression_matrix_Metab, method = 'spearman', use = "pairwise.complete.obs")

    correlation_matrix2 <- cor(cluster_expression_matrix_Prot, cluster_expression_matrix_Metab, method = 'spearman', use = "pairwise.complete.obs")

    #correlation_matrix
    # Replace row names with corresponding values from cluster_assignments_Prot$feature_name
    prot_annotation_match <- match(rownames(correlation_matrix), cluster_assignments_Prot$feature)
    non_na_symbols <- !is.na(cluster_assignments_Prot$feature_name)
    correlation_matrix_rows <- ifelse(!is.na(prot_annotation_match) & non_na_symbols[prot_annotation_match],
                                      cluster_assignments_Prot$feature_name[prot_annotation_match],
                                      rownames(correlation_matrix))
    rownames(correlation_matrix) <- correlation_matrix_rows


    # Replace column names with corresponding values from cluster_assignments_metab$feature_name
    metab_annotation_match <- match(colnames(correlation_matrix), cluster_assignments_metab$feature)
    non_na_KEGG <- !is.na(cluster_assignments_metab$feature_name)
    correlation_matrix_cols <- ifelse(!is.na(metab_annotation_match) & non_na_KEGG[metab_annotation_match],
                                      cluster_assignments_metab$feature_name[metab_annotation_match],
                                      colnames(correlation_matrix))
    colnames(correlation_matrix) <- correlation_matrix_cols

    #correlation_matrix2
    # Replace row names with corresponding values from cluster_assignments_Prot$feature_name
    prot_annotation_match <- match(rownames(correlation_matrix2), cluster_assignments_Prot$feature)
    non_na_symbols <- !is.na(cluster_assignments_Prot$feature_name)
    correlation_matrix_rows <- ifelse(!is.na(prot_annotation_match) & non_na_symbols[prot_annotation_match],
                                      cluster_assignments_Prot$feature_name[prot_annotation_match],
                                      rownames(correlation_matrix2))
    rownames(correlation_matrix2) <- correlation_matrix_rows

    # Replace column names in correlation_matrix2 with corresponding values from cluster_assignments_metab$feature_map
    metab_annotation_match2 <- match(colnames(correlation_matrix2), cluster_assignments_metab$feature)
    non_na_KEGG2 <- !is.na(cluster_assignments_metab$feature_map)
    correlation_matrix_cols2 <- ifelse(!is.na(metab_annotation_match2) & non_na_KEGG2[metab_annotation_match2],
                                       cluster_assignments_metab$feature_map[metab_annotation_match2],
                                       colnames(correlation_matrix2))
    colnames(correlation_matrix2) <- correlation_matrix_cols2


    # Extract row names omitting NA values and empty strings
    Important_features_Prot <- na.omit(rownames(correlation_matrix))
    Important_features_list[[paste("Important_features_Cluster_Prot_", i)]] <- Important_features_Prot[Important_features_Prot != ""]

    # Extract column names omitting NA values and empty strings
    Important_features_Metab <- na.omit(colnames(correlation_matrix2))
    Important_features_list[[paste("Important_features_Cluster_Metab_", i)]] <- Important_features_Metab[Important_features_Metab != ""]

    correlation_matrices_list[[paste("Correlation_Matrix_Cluster_", i)]] <- correlation_matrix

    # Convert the correlation matrix to a long format
    cor_Prot_metab_list <- reshape2::melt(correlation_matrix, id.var = "Correlation")
    colnames(cor_Prot_metab_list) <- c("Proteins/Genes", "Metabolites", "Correlation")
    cor_Prot_metab_list$Correlation <- round(cor_Prot_metab_list$Correlation, 2)

    correlation_List_list[[paste("Correlation_List_Cluster_", i)]] <- cor_Prot_metab_list
  }

  return(list(
    Top_correlations = Top_correlations,
    cluster_assignments = cluster_assignments_list,
    # annotation_matrices = annotation_matrices_list,
    # expression_matrices = expression_matrices_list,
    correlation_matrices = correlation_matrices_list,
    Important_features = Important_features_list,
    correlation_List = correlation_List_list
  ))
}
