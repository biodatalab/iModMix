#' Eigengenes
#'
#' @description A Eigengenes function
#'
#' @return The return value matrix.
#'
#' @export

Eigengenes = function(feature_mat_t = feature_mat_t, cluster_assignments=cluster_assignments) {
  module_eigenmetab_List_Me = WGCNA::moduleEigengenes(expr = feature_mat_t , colors = cluster_assignments)
  module_eigenmetab_Me = module_eigenmetab_List_Me$eigengenes
  return(list(module_eigenmetab_List_Me = module_eigenmetab_List_Me, module_eigenmetab_Me = module_eigenmetab_Me))
  }
