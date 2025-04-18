utils::globalVariables(c("missing_count"))

#' Eigengenes
#'
#' @description Calculates module eigengenes using the WGCNA package.
#' @param Expression_mat A feature matrix (e.g., gene expression) with samples in rows and features (e.g., genes) in columns. Row names must be unique.
#' @param cluster_assignments A vector of cluster assignments for each feature.
#' @return A list containing:
#' \item{module_eigenmetab_List_Me}{The full result from WGCNA::moduleEigengenes.}
#' \item{module_eigenmetab_Me}{The matrix of module eigengenes.}
#' \item{feature_mat_t}{The filtered and scaled feature matrix.}
#' @export
Eigengenes = function(load_data = load_data, cluster_assignments=cluster_assignments) {
  module_eigenmetab_List_Me = WGCNA::moduleEigengenes(expr = as.matrix(load_data) , colors = cluster_assignments)
  module_eigenmetab_Me = module_eigenmetab_List_Me$eigengenes
  return(list(module_eigenmetab_List_Me = module_eigenmetab_List_Me, module_eigenmetab_Me = module_eigenmetab_Me))
  }
