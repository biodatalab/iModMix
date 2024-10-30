#' cluster_assignments_genes
#'
#' @description Cluster assignments for proteins and genes.
#' @param cluster_genes A data frame containing cluster assignments for genes.
#' @param Prot_annotation A data frame with the annotation names of the proteins. Should have a column called Feature_ID.
#' @return A data frame containing cluster assignments and HMDB/Symbol.
#' @export
cluster_assignments_genes <- function(cluster_genes, Prot_annotation = NULL) {
  if (!is.null(Prot_annotation)) {
    try_cluster_assigment_Prot <- merge(cluster_genes, Prot_annotation, by.x = "feature", by.y = "Feature_ID", all.x = TRUE)
    try_cluster_assigment_Prot$feature_name <- ifelse(is.na(try_cluster_assigment_Prot$Symbol) | try_cluster_assigment_Prot$Symbol == "", try_cluster_assigment_Prot$feature, try_cluster_assigment_Prot$Symbol)
    try_cluster_assigment_Prot$feature_map <- ifelse(is.na(try_cluster_assigment_Prot$Symbol) | try_cluster_assigment_Prot$Symbol == "", "", try_cluster_assigment_Prot$Symbol)
    try_cluster_assigment_Prot <- try_cluster_assigment_Prot[, c("feature", "cluster", "col", "feature_name", "feature_map")]
  } else {
    try_cluster_assigment_Prot <- cluster_genes
    try_cluster_assigment_Prot$feature_name <- try_cluster_assigment_Prot$feature
    try_cluster_assigment_Prot$feature_map <- ""
    try_cluster_assigment_Prot <- try_cluster_assigment_Prot[, c("feature", "cluster", "col", "feature_name", "feature_map")]
  }
  return(try_cluster_assigment_Prot)
}
