#' partial_cors
#'
#' @description Calculates partial correlations based on graphical lasso (https://www.rdocumentation.org/packages/glassoFast/versions/1.0/topics/glassoFast).
#' @param feature_mat_t A feature matrix (e.g. gene expression) with samples in rows and features (e.g. genes) in columns. Row names must be unique.
#' @return partial_cor_mat A partial correlation matrix with NA's in the diagonal.
#'
#' @export
partial_cors = function(Expression_mat) {
  Expression_mat = Expression_mat
  Expression_mat$missing_count <- rowSums(is.na(Expression_mat))
  feature_mat <- subset(Expression_mat, missing_count <= 0.1 * (ncol(Expression_mat) - 2))
  features <- feature_mat$Feature_ID

  feature_mat_t <- t(feature_mat[, -c(1, ncol(feature_mat))])
  colnames(feature_mat_t) <- features
  feature_mat_t <- feature_mat_t[, apply(feature_mat_t, 2, function(x) length(unique(x)) > 1)]
  feature_mat_t <- as.matrix(scale(feature_mat_t))

  sd_values <- apply(feature_mat_t, 2, function(x) sd(x, na.rm = TRUE))
  filtered_indices <- which(sd_values > quantile(sd_values, 0.25))

  feature_mat_t <- if (length(filtered_indices) > 20000) {
    feature_mat_t[, order(sd_values[filtered_indices], decreasing = TRUE)[1:20000]]
  } else {
    feature_mat_t[, filtered_indices]
  }

  # generate covariance matrix
  cov_mat = cov(feature_mat_t, use = "pairwise.complete.obs")
  # calculate partial correlations
  glassoFast_result = glassoFast::glassoFast(cov_mat, .25, thr = 1e-04,
                                             maxIt = 10000, start = "cold",
                                             trace = FALSE)
  # extract just partial correlation result
  partial_cor_mat = glassoFast_result$wi
  # set diagonal values to 0; hierarchical clustering will give an error with NA diagonal values
  diag(partial_cor_mat) = NA
  # set row/colnames to match feature matrix
  rownames(partial_cor_mat) = colnames(feature_mat_t)
  colnames(partial_cor_mat) = colnames(feature_mat_t)
  # return the resulting partial correlations
  return(partial_cor_mat)
  #return(list(partial_cor_mat=partial_cor_mat))
}
