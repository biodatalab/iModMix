#' cluster_assignments
#'
#' @description Cluster assignments with annotation.
#' @param cluster A data frame containing cluster assignments.
#' @param PhenoData A data frame with the annotation names of the Dataset. Should have a column called Feature_ID.
#' @param selected_columns Columns selected for the user to merge in the cluster assignments table
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
#' cluster_assignments(cluster_df, PhenoData = pheno_df)
#'
#' # Run with selected columns
#' cluster_assignments(cluster_df, PhenoData = pheno_df, selected_columns = "Description")
#'
#' @export
cluster_assignments <- function(cluster, PhenoData = NULL, selected_columns = NULL) {
  if (is.null(PhenoData)) {
    cluster_annot <- cluster
    cluster_annot$feature_name <- cluster_annot$feature
    cluster_annot <- cluster_annot[, c("feature", "cluster", "col", "feature_name")]
  } else {
    if (is.null(selected_columns)) {
      cluster_annot <- merge(cluster, PhenoData, by.x = "feature", by.y = "Feature_ID", all.x = TRUE)
      if ("Symbol" %in% colnames(cluster_annot)) {
        cluster_annot$feature_name <- ifelse(is.na(cluster_annot$Symbol) | cluster_annot$Symbol == "", cluster_annot$feature, cluster_annot$Symbol)
      } else {
        cluster_annot$feature_name <- cluster_annot$feature
      }
      cluster_annot <- cluster_annot[, c("feature", "cluster", "col", "feature_name")]
    } else {
      cluster_annot <- merge(cluster, PhenoData, by.x = "feature", by.y = "Feature_ID", all.x = TRUE)
      if ("Symbol" %in% colnames(cluster_annot)) {
        cluster_annot$feature_name <- ifelse(is.na(cluster_annot$Symbol) | cluster_annot$Symbol == "", cluster_annot$feature, cluster_annot$Symbol)
      } else {
        cluster_annot$feature_name <- cluster_annot$feature
      }
      cluster_annot <- cluster_annot[, c("feature", "cluster", "col", "feature_name", selected_columns)]
    }
  }
  return(cluster_annot)
}


