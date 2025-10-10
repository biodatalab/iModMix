#' fctClusterAssignments
#'
#' @description Cluster assignments with annotation.
#' @param cluster A data frame containing cluster assignments.
#' @param phenoData A data frame with the annotation names of the Dataset. Should have a column called Feature_ID.
#' @param selectedColumns Columns selected for the user to merge in the cluster assignments table
#' @return A data frame containing cluster assignments and selected columns by user.
#' @examples
#' # Simulated cluster assignment data
#' cluster_df <- data.frame(
#'   feature = paste0("F", 1:5),
#'   cluster = paste0("cluster_", 1:5),
#'   col = RColorBrewer::brewer.pal(5, "Set1"),
#'   stringsAsFactors = FALSE
#' )
#'
#' # Simulated annotation data
#' pheno_df <- data.frame(
#'   Feature_ID = paste0("F", 1:5),
#'   Symbol = c("GeneA", "GeneB", "GeneC", "GeneD", "GeneE"),
#'   Description = paste("Description", 1:5),
#'   stringsAsFactors = FALSE
#' )
#'
#' # Run without selected columns
#' fctClusterAssignments(cluster_df, phenoData = pheno_df)
#'
#' # Run with selected columns
#' fctClusterAssignments(cluster_df, phenoData = pheno_df, selectedColumns = "Description")
#'
#' @export
fctClusterAssignments <- function(cluster, phenoData = NULL, selectedColumns = NULL) {
  if (is.null(phenoData)) {
    cluster_annot <- cluster
    cluster_annot$feature_name <- cluster_annot$feature
    cluster_annot <- cluster_annot[, c("feature", "cluster", "col", "feature_name")]
  } else {
    if (is.null(selectedColumns)) {
      cluster_annot <- merge(cluster, phenoData, by.x = "feature", by.y = "Feature_ID", all.x = TRUE)
      if ("Symbol" %in% colnames(cluster_annot)) {
        cluster_annot$feature_name <- ifelse(is.na(cluster_annot$Symbol) | cluster_annot$Symbol == "", cluster_annot$feature, cluster_annot$Symbol)
      } else {
        cluster_annot$feature_name <- cluster_annot$feature
      }
      cluster_annot <- cluster_annot[, c("feature", "cluster", "col", "feature_name")]
    } else {
      cluster_annot <- merge(cluster, phenoData, by.x = "feature", by.y = "Feature_ID", all.x = TRUE)
      if ("Symbol" %in% colnames(cluster_annot)) {
        cluster_annot$feature_name <- ifelse(is.na(cluster_annot$Symbol) | cluster_annot$Symbol == "", cluster_annot$feature, cluster_annot$Symbol)
      } else {
        cluster_annot$feature_name <- cluster_annot$feature
      }
      cluster_annot <- cluster_annot[, c("feature", "cluster", "col", "feature_name", selectedColumns)]
    }
  }
  return(cluster_annot)
}


