#' hierarchical_cluster
#'
#' @description Perform hierarchical clustering of a partial correlation matrix. By default uses the topological overlap measure (TOM)
#' @param parcor_mat A partial correlation matrix from glassoFast$wi: Estimated inverse covariance matrix
#' @param TOM TRUE: topological overlap measure; FALSE: partial correlation matrix; default is TRUE.
#' @param min_module_size Smallest modules to be generated; default is 10
#' @return result_list List containing TOM_diss (TOM dissimilarity; NA if argument TOM = FALSE), hclustTree (hierarchical clustering object), dynamicMods (index of module membership for features), and mod_count (number of modules).
#'
#' @export
#'
hierarchical_cluster = function(parcor_mat, tom = TRUE, min_module_size = 10) {
  # needed here? do any of our functions use multithreading?
  WGCNA::enableWGCNAThreads()
  if (tom == TRUE) {
    # TOMsimilarity() requires a positive matrix
    tom_sim = WGCNA::TOMsimilarity(abs(parcor_mat))
    # TOM dissimilarity
    tom_diss = 1 - tom_sim
    # hierarchical clustering with tom
    hclustTree = hclust(as.dist(tom_diss), method = "average")
    dynamicMods = dynamicTreeCut::cutreeDynamic(dendro = hclustTree,
                                                method = "hybrid", distM = tom_diss,
                                                deepSplit = 2, pamRespectsDendro = FALSE,
                                                minClusterSize = min_module_size,
                                                verbose = 0)
  } else if (tom == FALSE) {
    # hierarchical clustering with partial correlation matrix
    hclustTree = hclust(as.dist(parcor_mat), method = "average")
    dynamicMods = dynamicTreeCut::cutreeDynamic(dendro = hclustTree,
                                                method = "hybrid", distM = tom_diss,
                                                deepSplit = 2, pamRespectsDendro = FALSE,
                                                minClusterSize = min_module_size,
                                                verbose = 0)
  } else {
    stop("Error: invalid tom argument. Must be TRUE/FALSE.")
  }
  # starts counting assignments at 0 (0 is unassigned), so add 1 to all values to conform to R counting (starts at 1)
  dynamicMods = dynamicMods + 1
  mod_count = max(dynamicMods, na.rm = TRUE)
  #color_palette = colorRampPalette(RColorBrewer::brewer.pal("Set1", n = 9))(mod_count)
  color_palette = viridis::viridis(mod_count + 1)
  dynamicMods_colors = color_palette[dynamicMods]
  dynamicMods_labels = paste0("cluster_", stringr::str_pad(dynamicMods, 6, pad = "0"))
  cluster_assignments = data.frame(feature = colnames(parcor_mat),
                                   cluster = dynamicMods_labels,
                                   col = dynamicMods_colors,
                                   stringsAsFactors = FALSE)
  if (tom == TRUE) {
    result_list = list(tom_diss = tom_diss,
                       hclustTree = hclustTree,
                       dynamicMods_labels = dynamicMods_labels,
                       dynamicMods_colors = dynamicMods_colors,
                       dynamicMods_numeric = dynamicMods,
                       mod_count = mod_count,
                       cluster_assignments = cluster_assignments)
  } else if (tom == FALSE) {
    result_list = list(hclustTree = hclustTree,
                       dynamicMods_labels = dynamicMods_labels,
                       dynamicMods_colors = dynamicMods_colors,
                       dynamicMods_numeric = dynamicMods,
                       mod_count = mod_count,
                       cluster_assignments = cluster_assignments)
  }
  return(result_list)
}
