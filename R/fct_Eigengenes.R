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
Eigengenes = function(Expression_mat = Expression_mat, cluster_assignments=cluster_assignments) {
  Expression_mat = Expression_mat
  Expression_mat$missing_count <- rowSums(is.na(Expression_mat))
  feature_mat <- subset(Expression_mat, missing_count <= 0.1 * (ncol(Expression_mat) - 2))
  features <- feature_mat$Feature_ID

  feature_mat_t <- t(feature_mat[, -c(1, ncol(feature_mat))])
  colnames(feature_mat_t) <- features
  feature_mat_t <- feature_mat_t[, apply(feature_mat_t, 2, function(x) length(unique(x)) > 1)]

  sd_values <- apply(feature_mat_t, 2, function(x) sd(x, na.rm = TRUE))
  filtered_indices <- which(sd_values > quantile(sd_values, 0.25))
  feature_mat_t <- as.matrix(scale(feature_mat_t))

  feature_mat_t <- if (length(filtered_indices) > 20000) {
    feature_mat_t[, order(sd_values[filtered_indices], decreasing = TRUE)[1:20000]]
  } else {
    feature_mat_t[, filtered_indices]
  }

  module_eigenmetab_List_Me = WGCNA::moduleEigengenes(expr = as.matrix(feature_mat_t) , colors = cluster_assignments)
  module_eigenmetab_Me = module_eigenmetab_List_Me$eigengenes
  return(list(module_eigenmetab_List_Me = module_eigenmetab_List_Me, module_eigenmetab_Me = module_eigenmetab_Me, feature_mat_t = feature_mat_t))
  }
