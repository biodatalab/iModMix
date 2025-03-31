utils::globalVariables(c("missing_count"))

#' FeaturesAnnot_correlation
#'
#' @description Calculate correlation between the features of top correlated modules.
#' @param Cor_Datai_Dataj A data frame with the first principal component of each protein cluster and their correlations.
#' @param cluster_assignments_D2 A data frame containing cluster assignments and Enrichr terms for proteins.
#' @param cluster_assignments_D1 A data frame containing cluster assignments for metabolites.
#' @param ExpressionD2 A feature matrix (e.g., protein expression) with samples in rows and features (e.g., proteins) in columns. Row names must be unique.
#' @param ExpressionD1 A feature matrix (e.g., metabolite expression) with samples in rows and features (e.g., metabolites) in columns. Row names must be unique.
#' @param top_n The number of top correlations to select.
#' @return A list containing important features and correlation matrices.
#' @export
FeaturesAnnot_correlation <- function(Cor_Datai_Dataj, cluster_assignments_D1, cluster_assignments_D2,
                                      ExpressionD1 = ExpressionD1, ExpressionD2 = ExpressionD2, top_n = 5)  {
  # Select the top n correlations
  Top_correlations <- Cor_Datai_Dataj[order(-abs(Cor_Datai_Dataj$value)), ][1:top_n, ]

  # Remove the "ME" prefix from Prot_module and Metab_module columns
  Top_correlations[c("from", "to")] <- lapply(Top_correlations[c("from", "to")], function(x) sub("^D[123]", "", x))

  # Initialize lists to store cluster assignments, annotation matrices, and expression matrices
  cluster_assignments_list <- list()
  expression_matrices_list <- list()
  correlation_matrices_list <- list()
  Important_features_list <- list()
  correlation_List_list <- list()

  # Loop through each top correlation and extract variables from clusters
  for (i in 1:top_n) {
    Metab_module <- Top_correlations[i, "from"]
    Prot_module <- Top_correlations[i, "to"]

    # Extract variables for Prot and Metab
    cluster_top_Metab <- cluster_assignments_D1[grepl(Metab_module, cluster_assignments_D1$col), ]
    cluster_top_Prot <- cluster_assignments_D2[grepl(Prot_module, cluster_assignments_D2$col), ]

    # Store cluster assignments
    cluster_assignments_list[[paste("cluster_assignments_D1_", i)]] <- cluster_top_Metab
    cluster_assignments_list[[paste("cluster_assignments_D2_", i)]] <- cluster_top_Prot

    # Extract variables for Prot and Metab from their respective annotation matrices
    cluster_variables_Metab <- cluster_top_Metab$feature
    cluster_variables_Prot <- cluster_top_Prot$feature

    # Filter expression matrices based on column names
    ExpressionD1 = ExpressionD1
    ExpressionD1$missing_count <- rowSums(is.na(ExpressionD1))
    featureMetab_mat <- subset(ExpressionD1, missing_count <= 0.1 * (ncol(ExpressionD1) - 2))
    featuresMetab <- featureMetab_mat$Feature_ID

    metab_t <- t(featureMetab_mat[, -c(1, ncol(featureMetab_mat))])
    colnames(metab_t) <- featuresMetab
    metab_t <- metab_t[, apply(metab_t, 2, function(x) length(unique(x)) > 1)]

    sd_values_metab <- apply(metab_t, 2, function(x) sd(x, na.rm = TRUE))
    filtered_indices <- which(sd_values_metab > quantile(sd_values_metab, 0.25))

    metab_t <- if (length(sd_values_metab) > 20000) {
      if (length(filtered_indices) > 20000) {
        metab_t[, order(sd_values[filtered_indices], decreasing = TRUE)[1:20000]]
      } else {
        metab_t[, filtered_indices]
      }
    } else {
      metab_t[, ]
    }

    metab_t = impute::impute.knn(metab_t, k = min(10, nrow(metab_t)))
    metab_t= metab_t$data

    metab_t <- as.matrix(scale(metab_t))

    ExpressionD2 = ExpressionD2
    ExpressionD2$missing_count <- rowSums(is.na(ExpressionD2))
    featureProt_mat <- subset(ExpressionD2, missing_count <= 0.1 * (ncol(ExpressionD2) - 2))
    featuresProt <- featureProt_mat$Feature_ID

    Prot_t <- t(featureProt_mat[, -c(1, ncol(featureProt_mat))])
    colnames(Prot_t) <- featuresProt
    Prot_t <- Prot_t[, apply(Prot_t, 2, function(x) length(unique(x)) > 1)]

    sd_values_Prot <- apply(Prot_t, 2, function(x) sd(x, na.rm = TRUE))
    filtered_indices <- which(sd_values_Prot > quantile(sd_values_Prot, 0.25))

    Prot_t <- if (length(sd_values_Prot) > 20000) {
      if (length(filtered_indices) > 20000) {
        Prot_t[, order(sd_values[filtered_indices], decreasing = TRUE)[1:20000]]
      } else {
        Prot_t[, filtered_indices]
      }
    } else {
      Prot_t[, ]
    }

    Prot_t = impute::impute.knn(Prot_t, k = min(10, nrow(Prot_t)))
    Prot_t= Prot_t$data

    Prot_t <- as.matrix(scale(Prot_t))

    cluster_expression_matrix_Metab <- metab_t[, colnames(metab_t) %in% cluster_variables_Metab, drop = FALSE]
    cluster_expression_matrix_Prot <- Prot_t[, colnames(Prot_t) %in% cluster_variables_Prot, drop = FALSE]

    # Calculate and store the correlation matrix between Prot and Metab expression matrices
    correlation_matrix <- cor(cluster_expression_matrix_Metab, cluster_expression_matrix_Prot, method = 'spearman', use = "pairwise.complete.obs")

    #correlation_matrix2 <- correlation_matrix

    #correlation_matrix

    # # Replace column names with corresponding values from cluster_assignments_D1$feature_name
    # metab_annotation_match <- match(rownames(correlation_matrix), cluster_assignments_D1$feature)
    # non_na_KEGG <- !is.na(cluster_assignments_D1$feature_name)
    # correlation_matrix_rows <- ifelse(!is.na(metab_annotation_match) & non_na_KEGG[metab_annotation_match],
    #                                   cluster_assignments_D1$feature_name[metab_annotation_match],
    #                                   rownames(correlation_matrix))
    # rownames(correlation_matrix) <- correlation_matrix_rows
    #
    # # Replace row names with corresponding values from cluster_assignments_D2$feature_name
    # prot_annotation_match <- match(colnames(correlation_matrix), cluster_assignments_D2$feature)
    # non_na_symbols <- !is.na(cluster_assignments_D2$feature_name)
    # correlation_matrix_cols <- ifelse(!is.na(prot_annotation_match) & non_na_symbols[prot_annotation_match],
    #                                   cluster_assignments_D2$feature_name[prot_annotation_match],
    #                                   colnames(correlation_matrix))
    # colnames(correlation_matrix) <- correlation_matrix_cols
    #
    # #correlation_matrix2
    # # Replace row names with corresponding values from cluster_assignments_D2$feature_name
    # prot_annotation_match <- match(colnames(correlation_matrix2), cluster_assignments_D2$feature)
    # non_na_symbols <- !is.na(cluster_assignments_D2$feature_name)
    # correlation_matrix_cols <- ifelse(!is.na(prot_annotation_match) & non_na_symbols[prot_annotation_match],
    #                                   cluster_assignments_D2$feature_name[prot_annotation_match],
    #                                   colnames(correlation_matrix2))
    # colnames(correlation_matrix2) <- correlation_matrix_cols
    #
    # # Store cluster-specific expression matrices
    # colnames(cluster_expression_matrix_Metab) <- correlation_matrix_rows
    # colnames(cluster_expression_matrix_Prot) <- correlation_matrix_cols
    expression_matrices_list[[paste("Expression_Matrix_Cluster_Metab_", i)]] <- cluster_expression_matrix_Metab
    expression_matrices_list[[paste("Expression_Matrix_Cluster_Prot_", i)]] <- cluster_expression_matrix_Prot


    # # Replace column names in correlation_matrix2 with corresponding values from cluster_assignments_D1$feature_map
    # metab_annotation_match2 <- match(rownames(correlation_matrix2), cluster_assignments_D1$feature)
    # non_na_KEGG2 <- !is.na(cluster_assignments_D1$feature_map)
    # correlation_matrix_rows2 <- ifelse(!is.na(metab_annotation_match2) & non_na_KEGG2[metab_annotation_match2],
    #                                    cluster_assignments_D1$feature_map[metab_annotation_match2],
    #                                    rownames(correlation_matrix2))
    # rownames(correlation_matrix2) <- correlation_matrix_rows2


    # Extract column names omitting NA values and empty strings
    Important_features_Metab <- na.omit(rownames(correlation_matrix))
    Important_features_list[[paste("Important_features_Cluster_Metab_", i)]] <- Important_features_Metab[Important_features_Metab != ""]

    # Extract row names omitting NA values and empty strings
    Important_features_Prot <- na.omit(colnames(correlation_matrix))
    Important_features_list[[paste("Important_features_Cluster_Prot_", i)]] <- Important_features_Prot[Important_features_Prot != ""]

    correlation_matrices_list[[paste("Correlation_Matrix_Cluster_", i)]] <- correlation_matrix

    # Convert the correlation matrix to a long format
    Cor_Datai_Dataj_list <- reshape2::melt(correlation_matrix, id.var = "Correlation")
    colnames(Cor_Datai_Dataj_list) <- c("Data1", "Data2", "Correlation")
    Cor_Datai_Dataj_list <-  Cor_Datai_Dataj_list[order(abs(Cor_Datai_Dataj_list$Correlation), decreasing = TRUE), ]
    Cor_Datai_Dataj_list$Correlation <- round(Cor_Datai_Dataj_list$Correlation, 4)

    correlation_List_list[[paste("Correlation_List_Cluster_", i)]] <- Cor_Datai_Dataj_list
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
