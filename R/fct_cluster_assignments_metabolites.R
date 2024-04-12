#' cluster_assignments_metabolites
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @export

#' #' 4a for metabolites
# cluster_assignments_metabolites <- function(cluster_metabolites, metab_annotation = NULL) {
#   if (!is.null(metab_annotation)) {
#     # Create the new dataframe with the 'feature_name' column
#     try_cluster_assigment_metab <- cluster_metabolites %>%
#       left_join(metab_annotation, by = c("feature" = "Feature_ID")) %>%
#       dplyr::mutate(feature_name = ifelse(is.na(KEGG) | KEGG == "", feature, KEGG),
#                     feature_map = ifelse(is.na(KEGG) | KEGG == "", "", KEGG)) %>%
#       dplyr::select(feature, cluster, col, feature_name, feature_map)
#   } else {
#     # If 'Metabolites_annotation' is not present, keep the 'feature' name and leave 'feature_map' blank
#     try_cluster_assigment_metab <- cluster_metabolites %>%
#       dplyr::mutate(feature_name = feature, feature_map = "") %>%
#       dplyr::select(feature, cluster, col, feature_name, feature_map)
#   }
#   return(try_cluster_assigment_metab)
# }


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
    try_cluster_assigment_metab <- try_cluster_assigment_metab[, c("feature", "cluster", "col", "feature_name", "feature_map")]
  }
  return(try_cluster_assigment_metab)
}
