utils::globalVariables(c("missing_count"))

#' Eigengenes
#'
#' @description Calculates module eigengenes using the WGCNA package.
#' @param load_data A prepossessed data matrix resulting from "load data" function
#' @param cluster_assignments A vector of cluster assignments for each feature.
#' @return A list containing:
#' \item{module_eigenmetab_List_Me}{The full result from WGCNA::moduleEigengenes.}
#' \item{module_eigenmetab_Me}{The matrix of module eigengenes.}
#' \item{feature_mat_t}{The filtered and scaled feature matrix.}
#' @examples
#' # Simulate a small expression matrix
#' set.seed(123)
#' mat <- matrix(rnorm(100), nrow = 10, ncol = 10)
#' colnames(mat) <- paste0("Gene", 1:10)
#' rownames(mat) <- paste0("Sample", 1:10)
#'
#' # Simulate cluster assignments
#' clusters <- rep(1:2, each = 5)
#'
#' # Run Eigengenes function
#' result <- Eigengenes(load_data = mat, cluster_assignments = clusters)
#'
#' # View eigengene matrix
#' head(result$module_eigenmetab_Me)
#'
#' @export
Eigengenes <- function(load_data = load_data, cluster_assignments = cluster_assignments) {
  module_eigenmetab_List_Me <- WGCNA::moduleEigengenes(expr = as.matrix(load_data) , colors = cluster_assignments)
  module_eigenmetab_Me <- as.matrix(module_eigenmetab_List_Me$eigengenes)
  return(list(module_eigenmetab_List_Me = module_eigenmetab_List_Me, module_eigenmetab_Me = module_eigenmetab_Me))
  }
