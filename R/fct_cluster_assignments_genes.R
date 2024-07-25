#' cluster_assignments_genes
#'
#' @description cluster Assignments for proteins and genes
#' @param hclustTree (hierarchical clustering object)
#' @param Genes_Annotation (data frame with de symbol genes of the Proteins or RNAseq: Should have a column called Feature_ID
#' @return result_list data frame containing cluster_assignments and HMDB/Symbol
#'
#' @export
cluster_assignments_genes <- function(cluster_genes, Prot_annotation = NULL) {
  if (!is.null(Prot_annotation)) {
    # Create the new dataframe with the 'feature_name' column
    try_cluster_assigment_Prot <- merge(cluster_genes, Prot_annotation, by.x = "feature", by.y = "Feature_ID", all.x = TRUE)
    try_cluster_assigment_Prot$feature_name <- ifelse(is.na(try_cluster_assigment_Prot$Symbol) | try_cluster_assigment_Prot$Symbol == "", try_cluster_assigment_Prot$feature, try_cluster_assigment_Prot$Symbol)
    try_cluster_assigment_Prot$feature_map <- ifelse(is.na(try_cluster_assigment_Prot$Symbol) | try_cluster_assigment_Prot$Symbol == "", "", try_cluster_assigment_Prot$Symbol)
    try_cluster_assigment_Prot <- try_cluster_assigment_Prot[, c("feature", "cluster", "col", "feature_name", "feature_map")]
  } else {
    # If 'Protolites_annotation' is not present, keep the 'feature' name and leave 'feature_map' blank
    try_cluster_assigment_Prot <- cluster_genes
    try_cluster_assigment_Prot$feature_name <- try_cluster_assigment_Prot$feature
    try_cluster_assigment_Prot$feature_map <- ""
    try_cluster_assigment_Prot <- try_cluster_assigment_Prot[, c("feature", "cluster", "col", "feature_name", "feature_map")]
  }
  return(try_cluster_assigment_Prot)
}
