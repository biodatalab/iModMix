#' Eigengenes
#'
#' @description moduleEigengenes function by WGCNA
#' @return A feature matrix with the first principal component of each metabolomic/protein cluster
#'
#' @export

Eigengenes = function(Expression_mat = Expression_mat, cluster_assignments=cluster_assignments) {
  Expression_mat = Expression_mat
  Expression_mat$missing_count <- rowSums(is.na(Expression_mat))
  feature_mat <- subset(Expression_mat, missing_count <= 0.1 * (ncol(Expression_mat) - 2))
  features <- feature_mat$Feature_ID

  feature_mat_t <- t(feature_mat[, -c(1, ncol(feature_mat))])
  colnames(feature_mat_t) <- features
  feature_mat_t <- feature_mat_t[, apply(feature_mat_t, 2, function(x) length(unique(x)) > 1)]
  feature_mat_t <- as.matrix(scale(feature_mat_t))

  sd_values <- apply(feature_mat_t, 2, function(x) sd(x, na.rm = TRUE))
  feature_mat_t3SD <- feature_mat_t[, sd_values > quantile(sd_values, 0.25)]

  module_eigenmetab_List_Me = WGCNA::moduleEigengenes(expr = feature_mat_t , colors = cluster_assignments)
  module_eigenmetab_Me = module_eigenmetab_List_Me$eigengenes
  return(list(module_eigenmetab_List_Me = module_eigenmetab_List_Me, module_eigenmetab_Me = module_eigenmetab_Me, feature_mat_t = feature_mat_t))
  }
