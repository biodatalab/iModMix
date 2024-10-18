#' FeaturesAnnot_correlation
#'
#' @description Calculate correlation between the features of top correlated modules
#' @param Cor_Prot_Metab A feature matrix with the first principal component of each protein cluster
#' @param cluster_assignments_Prot data frame containing cluster_assignments and Enrichr terms
#' @param cluster_assignments_metab data frame containing cluster_assignments
#' @param Prot_annotation data frame with de annotation names of the proteins: Should have a column called Symbol
#' @param Metabolites_annotation (data frame with de annotation names of the metabolites: Should have a column called KEGG)
#' @param Prot_t A feature matrix (e.g. gene expression) with samples in columns and features (e.g. genes) in rows Row names must be unique.
#' @param metab_t A feature matrix (e.g. gene expression) with samples in columns and features (e.g. metabolites) in rows Row names must be unique.
#' @return Important features
#' @export

FeaturesAnnot_correlation <- function(Cor_Prot_Metab, cluster_assignments_Prot, cluster_assignments_metab,
                                      #Prot_annotation = NULL, metab_annotation = NULL,
                                      ExpressionProt_mat = ExpressionProt_mat, ExpressionMetab_mat = ExpressionMetab_mat, top_n = 5)  {
  # Select the top n correlations
  Top_correlations <- Cor_Prot_Metab[order(-abs(Cor_Prot_Metab$Correlation)), ][1:top_n, ]

  # Remove the "ME" prefix from Prot_module and Metab_module columns
  Top_correlations[c("Prot_module", "Metab_module")] <- lapply(Top_correlations[c("Prot_module", "Metab_module")], function(x) sub("^ME", "", x))

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
    Metab_module <- Top_correlations[i, "Metab_module"]

    # Extract variables for Prot and Metab
    cluster_top_Prot <- cluster_assignments_Prot[grepl(Prot_module, cluster_assignments_Prot$col), ]
    cluster_top_Metab <- cluster_assignments_metab[grepl(Metab_module, cluster_assignments_metab$col), ]

    # Store cluster assignments
    cluster_assignments_list[[paste("Cluster_assignments_Prot_", i)]] <- cluster_top_Prot
    cluster_assignments_list[[paste("Cluster_assignments_Metab_", i)]] <- cluster_top_Metab

    # Extract variables for Prot and Metab from their respective annotation matrices
    cluster_variables_Prot <- cluster_top_Prot$feature
    cluster_variables_Metab <- cluster_top_Metab$feature

    # Filter expression matrices based on column names
    ExpressionMetab_mat = ExpressionMetab_mat
    ExpressionMetab_mat$missing_count <- rowSums(is.na(ExpressionMetab_mat))
    featureMetab_mat <- subset(ExpressionMetab_mat, missing_count <= 0.1 * (ncol(ExpressionMetab_mat) - 2))
    featuresMetab <- featureMetab_mat$Feature_ID

    metab_t <- t(featureMetab_mat[, -c(1, ncol(featureMetab_mat))])
    colnames(metab_t) <- featuresMetab
    metab_t <- metab_t[, apply(metab_t, 2, function(x) length(unique(x)) > 1)]
    metab_t <- as.matrix(scale(metab_t))

    sd_values_metab <- apply(metab_t, 2, function(x) sd(x, na.rm = TRUE))
    filtered_indices <- which(sd_values_metab > quantile(sd_values_metab, 0.25))

    metab_t <- if (length(filtered_indices) > 20000) {
      metab_t[, order(sd_values_metab[filtered_indices], decreasing = TRUE)[1:20000]]
    } else {
      metab_t[, filtered_indices]
    }

    ExpressionProt_mat = ExpressionProt_mat
    ExpressionProt_mat$missing_count <- rowSums(is.na(ExpressionProt_mat))
    featureProt_mat <- subset(ExpressionProt_mat, missing_count <= 0.1 * (ncol(ExpressionProt_mat) - 2))
    featuresProt <- featureProt_mat$Feature_ID

    Prot_t <- t(featureProt_mat[, -c(1, ncol(featureProt_mat))])
    colnames(Prot_t) <- featuresProt
    Prot_t <- Prot_t[, apply(Prot_t, 2, function(x) length(unique(x)) > 1)]
    Prot_t <- as.matrix(scale(Prot_t))

    sd_values_Prot <- apply(Prot_t, 2, function(x) sd(x, na.rm = TRUE))
    filtered_indices <- which(sd_values_Prot > quantile(sd_values_Prot, 0.25))

    Prot_t <- if (length(filtered_indices) > 20000) {
      Prot_t[, order(sd_values_Prot[filtered_indices], decreasing = TRUE)[1:20000]]
    } else {
      Prot_t[, filtered_indices]
    }

    cluster_expression_matrix_Prot <- Prot_t[, colnames(Prot_t) %in% cluster_variables_Prot, drop = FALSE]
    cluster_expression_matrix_Metab <- metab_t[, colnames(metab_t) %in% cluster_variables_Metab, drop = FALSE]

    # Calculate and store the correlation matrix between Prot and Metab expression matrices
    correlation_matrix <- cor(cluster_expression_matrix_Prot, cluster_expression_matrix_Metab, method = 'spearman', use = "pairwise.complete.obs")

    #correlation_matrix2 <- cor(cluster_expression_matrix_Prot, cluster_expression_matrix_Metab, method = 'spearman', use = "pairwise.complete.obs")
    correlation_matrix2 <- correlation_matrix

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

    # Store cluster-specific expression matrices
    colnames(cluster_expression_matrix_Prot) <- correlation_matrix_rows
    colnames(cluster_expression_matrix_Metab) <- correlation_matrix_cols
    expression_matrices_list[[paste("Expression_Matrix_Cluster_Prot_", i)]] <- cluster_expression_matrix_Prot
    expression_matrices_list[[paste("Expression_Matrix_Cluster_Metab_", i)]] <- cluster_expression_matrix_Metab

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
    Important_features_Metab <- na.omit(colnames(correlation_matrix))
    #Important_features_Metab <- na.omit(colnames(correlation_matrix2))
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
    expression_matrices = expression_matrices_list,
    correlation_matrices = correlation_matrices_list,
    Important_features = Important_features_list,
    correlation_List = correlation_List_list
  ))
}
