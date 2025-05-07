#' FeaturesAnnot_correlation
#'
#' @description Calculate correlation between the features of top correlated modules.
#' @param Cor_Datai_Dataj A data frame with the first principal component of each protein cluster and their correlations.
#' @param cluster_assignments_D2 A data frame containing cluster assignments and Enrichr terms for proteins.
#' @param cluster_assignments_D1 A data frame containing cluster assignments for metabolites.
#' @param load_data1 A prepossessed data matrix resulting from "load data" function from Data1.
#' @param load_data2 A prepossessed data matrix resulting from "load data" function from Data2.
#' @param top_n The number of top correlations to select.
#' @return A list containing important features and correlation matrices.
#' @export
FeaturesAnnot_correlation <- function(Cor_Datai_Dataj, cluster_assignments_D1, cluster_assignments_D2,
                                      load_data1 = load_data1, load_data2 = load_data2, top_n = 5)  {
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

    cluster_expression_matrix_Metab <- load_data1[, colnames(load_data1) %in% cluster_variables_Metab, drop = FALSE]
    cluster_expression_matrix_Prot <- load_data2[, colnames(load_data2) %in% cluster_variables_Prot, drop = FALSE]

    # Calculate and store the correlation matrix between Prot and Metab expression matrices
    correlation_matrix <- cor(cluster_expression_matrix_Metab, cluster_expression_matrix_Prot, method = 'spearman', use = "pairwise.complete.obs")

    expression_matrices_list[[paste("Expression_Matrix_Cluster_Metab_", i)]] <- cluster_expression_matrix_Metab
    expression_matrices_list[[paste("Expression_Matrix_Cluster_Prot_", i)]] <- cluster_expression_matrix_Prot

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

  VarD1 <- as.data.frame(table(cluster_assignments_D1$col))
  VarD2 <- as.data.frame(table(cluster_assignments_D2$col))

  # Merge the counts with Top_correlations
  Top_correlations <- merge(Top_correlations, VarD1, by.x = "from", by.y = "Var1", all.x = TRUE)
  Top_correlations <- merge(Top_correlations, VarD2, by.x = "to", by.y = "Var1", all.x = TRUE)

  # Rename columns for clarity
  colnames(Top_correlations)[colnames(Top_correlations) == "Freq.x"] <- "# of var into the D1_module"
  colnames(Top_correlations)[colnames(Top_correlations) == "Freq.y"] <- "# of var into the D2_module"

  # Select and rename final columns
  Top_correlations <- Top_correlations[, c("from", "# of var into the D1_module", "to", "# of var into the D2_module", "value")]

  return(list(
    Top_correlations = Top_correlations,
    cluster_assignments = cluster_assignments_list,
    expression_matrices = expression_matrices_list,
    correlation_matrices = correlation_matrices_list,
    Important_features = Important_features_list,
    correlation_List = correlation_List_list
  ))
}
