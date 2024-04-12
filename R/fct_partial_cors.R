#' partial_cors
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @export
partial_cors = function(feature_mat_t) {
  # generate covariance matrix
  #feature_mat = as.matrix(feature_mat[,-1])
  feature_mat_t = as.matrix(feature_mat_t)
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
  #return(partial_cor_mat)
  return(list(partial_cor_mat=partial_cor_mat))
}
