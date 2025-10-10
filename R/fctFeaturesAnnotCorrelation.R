#' fctFeaturesAnnotCorrelation
#'
#' @description Calculate correlation between the features of top correlated modules.
#' @param corDataiDataj A data frame with the first principal component of each protein cluster and their correlations.
#' @param clusterAssignmentsD2 A data frame containing cluster assignments and Enrichr terms for proteins.
#' @param clusterAssignmentsD1 A data frame containing cluster assignments for metabolites.
#' @param loadData1 A prepossessed data matrix resulting from "load data" function from Data1.
#' @param loadData2 A prepossessed data matrix resulting from "load data" function from Data2.
#' @param topn The number of top correlations to select.
#' @return A list containing important features and correlation matrices.
#' @examples
#' # Simulated correlation data
#' corDataiDataj <- data.frame(
#'   from = c("blue", "green"),
#'   to = c("red", "yellow"),
#'   value = c(0.9, 0.85),
#'   stringsAsFactors = FALSE
#' )
#'
#' # Simulated cluster assignments
#' clusterAssignmentsD1 <- data.frame(
#'   feature = c("M1", "M2"),
#'   col = c("blue", "green"),
#'   stringsAsFactors = FALSE
#' )
#' clusterAssignmentsD2 <- data.frame(
#'   feature = c("P1", "P2"),
#'   col = c("red", "yellow"),
#'   stringsAsFactors = FALSE
#' )
#'
#' # Simulated expression matrices
#' loadData1 <- matrix(rnorm(20), nrow = 5, ncol = 4)
#' colnames(loadData1) <- c("M1", "M2", "X1", "X2")
#' rownames(loadData1) <- paste0("Sample", 1:5)
#'
#' loadData2 <- matrix(rnorm(20), nrow = 5, ncol = 4)
#' colnames(loadData2) <- c("P1", "P2", "Y1", "Y2")
#' rownames(loadData2) <- paste0("Sample", 1:5)
#'
#' # Run the function
#' result <- fctFeaturesAnnotCorrelation(
#'   corDataiDataj = corDataiDataj,
#'   clusterAssignmentsD1 = clusterAssignmentsD1,
#'   clusterAssignmentsD2 = clusterAssignmentsD2,
#'   loadData1 = loadData1,
#'   loadData2 = loadData2,
#'   topn = 2
#' )
#'
#' # View top correlations
#' result$Top_correlations
#'
#' @export
fctFeaturesAnnotCorrelation <- function(corDataiDataj, clusterAssignmentsD1, clusterAssignmentsD2,
                                      loadData1 = loadData1, loadData2 = loadData2, topn = 5)  {
  # Select the top n correlations
  Top_correlations <- corDataiDataj[order(-abs(corDataiDataj$value)), ][seq_len(topn), ]

  # Remove the "ME" prefix from Prot_module and Metab_module columns
  Top_correlations[c("from", "to")] <- lapply(Top_correlations[c("from", "to")], function(x) sub("^D[123]", "", x))

  # Initialize lists to store cluster assignments, annotation matrices, and expression matrices
  clusterAssignmentslist <- list()
  expression_matrices_list <- list()
  correlation_matrices_list <- list()
  Important_features_list <- list()
  correlation_List_list <- list()

  # Loop through each top correlation and extract variables from clusters
  for (i in seq_len(topn)) {
    Metab_module <- Top_correlations[i, "from"]
    Prot_module <- Top_correlations[i, "to"]

    # Extract variables for Prot and Metab
    cluster_top_Metab <- clusterAssignmentsD1[grepl(Metab_module, clusterAssignmentsD1$col), ]
    cluster_top_Prot <- clusterAssignmentsD2[grepl(Prot_module, clusterAssignmentsD2$col), ]

    # Store cluster assignments
    clusterAssignmentslist[[paste("clusterAssignmentsD1_", i)]] <- cluster_top_Metab
    clusterAssignmentslist[[paste("clusterAssignmentsD2_", i)]] <- cluster_top_Prot

    # Extract variables for Prot and Metab from their respective annotation matrices
    cluster_variables_Metab <- cluster_top_Metab$feature
    cluster_variables_Prot <- cluster_top_Prot$feature

    cluster_expression_matrix_Metab <- loadData1[, colnames(loadData1) %in% cluster_variables_Metab, drop = FALSE]
    cluster_expression_matrix_Prot <- loadData2[, colnames(loadData2) %in% cluster_variables_Prot, drop = FALSE]

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
    corDataiDataj_list <- reshape2::melt(correlation_matrix, id.var = "Correlation")
    colnames(corDataiDataj_list) <- c("Data1", "Data2", "Correlation")
    corDataiDataj_list <-  corDataiDataj_list[order(abs(corDataiDataj_list$Correlation), decreasing = TRUE), ]
    corDataiDataj_list$Correlation <- round(corDataiDataj_list$Correlation, 4)

    correlation_List_list[[paste("Correlation_List_Cluster_", i)]] <- corDataiDataj_list
  }

  VarD1 <- as.data.frame(table(clusterAssignmentsD1$col))
  VarD2 <- as.data.frame(table(clusterAssignmentsD2$col))

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
    cluster_assignments = clusterAssignmentslist,
    expression_matrices = expression_matrices_list,
    correlation_matrices = correlation_matrices_list,
    Important_features = Important_features_list,
    correlation_List = correlation_List_list
  ))
}
