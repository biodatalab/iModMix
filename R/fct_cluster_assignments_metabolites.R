#' cluster_assignments_metabolites
#'
#' @description cluster Assignments for metabolites
#' @param hclustTree (hierarchical clustering object)
#' @param Metabolites_annotation (data frame with de annotation names of the metabolites: Should have a column called HMDB)
#' @return result_list data frame containing cluster_assignments and HMDB/Symbol
#'
#' @export
cluster_assignments_metabolites <- function(cluster_metabolites, metab_annotation = NULL) {
  if (is.null(metab_annotation)) {
    # If 'Metabolites_annotation' is not present, keep the 'feature' name and leave 'feature_map' blank
    try_cluster_assigment_metab <- cluster_metabolites
    try_cluster_assigment_metab$feature_name <- try_cluster_assigment_metab$feature
    try_cluster_assigment_metab$feature_map <- ""
    try_cluster_assigment_metab <- try_cluster_assigment_metab[, c("feature", "cluster", "col", "feature_name", "feature_map")]
    }
  else {
    # Create the new dataframe with the 'feature_name' column
    try_cluster_assigment_metab <- merge(cluster_metabolites, metab_annotation, by.x = "feature", by.y = "Feature_ID", all.x = TRUE)
    try_cluster_assigment_metab$feature_name <- ifelse(is.na(try_cluster_assigment_metab$KEGG) | try_cluster_assigment_metab$KEGG == "", try_cluster_assigment_metab$feature, try_cluster_assigment_metab$KEGG)
    try_cluster_assigment_metab$feature_map <- ifelse(is.na(try_cluster_assigment_metab$KEGG) | try_cluster_assigment_metab$KEGG == "", "", try_cluster_assigment_metab$KEGG)
    try_cluster_assigment_metab$feature_Mapped <- ifelse(is.na(try_cluster_assigment_metab$KEGG) | try_cluster_assigment_metab$KEGG == "", "", try_cluster_assigment_metab$KEGG)
    try_cluster_assigment_metab <- try_cluster_assigment_metab[, c("feature", "cluster", "col", "feature_name", "feature_map", "Metabolite")]

  }
  return(try_cluster_assigment_metab)
}
