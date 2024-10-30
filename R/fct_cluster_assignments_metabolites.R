#' cluster_assignments_metabolites
#'
#' @description cluster Assignments for metabolites
#' @param cluster_metabolites A data frame containing cluster assignments for metabolites
#' @param metab_annotation A data frame with the annotation names of the metabolites. Should have a column called Feature_ID.
#' @return A data frame containing cluster assignments and HMDB/Symbol.
#' @export
cluster_assignments_metabolites <- function(cluster_metabolites, metab_annotation = NULL) {
  if (is.null(metab_annotation)) {
    try_cluster_assigment_metab <- cluster_metabolites
    try_cluster_assigment_metab$feature_name <- try_cluster_assigment_metab$feature
    try_cluster_assigment_metab$feature_map <- ""
    try_cluster_assigment_metab <- try_cluster_assigment_metab[, c("feature", "cluster", "col", "feature_name", "feature_map")]
    }
  else {
    try_cluster_assigment_metab <- merge(cluster_metabolites, metab_annotation, by.x = "feature", by.y = "Feature_ID", all.x = TRUE)
    try_cluster_assigment_metab$feature_name <- ifelse(is.na(try_cluster_assigment_metab$KEGG) | try_cluster_assigment_metab$KEGG == "", try_cluster_assigment_metab$feature, try_cluster_assigment_metab$KEGG)
    try_cluster_assigment_metab$feature_map <- ifelse(is.na(try_cluster_assigment_metab$KEGG) | try_cluster_assigment_metab$KEGG == "", "", try_cluster_assigment_metab$KEGG)
    try_cluster_assigment_metab$feature_Mapped <- ifelse(is.na(try_cluster_assigment_metab$KEGG) | try_cluster_assigment_metab$KEGG == "", "", try_cluster_assigment_metab$KEGG)
    try_cluster_assigment_metab <- try_cluster_assigment_metab[, c("feature", "cluster", "col", "feature_name", "feature_map", "Metabolite")]
  }
  return(try_cluster_assigment_metab)
}
