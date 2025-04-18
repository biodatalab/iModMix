#' partial_cors
#'
#' @description Calculates partial correlations based on graphical lasso (https://www.rdocumentation.org/packages/glassoFast/versions/1.0/topics/glassoFast).
#' @param Expression_mat A feature matrix (e.g. gene expression) with samples in columns and features (e.g. genes) in rows. Row names must be unique.
#' @return partial_cor_mat A partial correlation matrix with NA's in the diagonal.
#'
#' @export
partial_cors = function(load_data, rho = .25) {
  # generate covariance matrix
  cov_mat = cov(as.matrix(load_data), use = "pairwise.complete.obs")
  # calculate partial correlations
  glassoFast_result = glassoFast::glassoFast(cov_mat, rho = rho, thr = 1e-04,
                                             maxIt = 10000, start = "cold",
                                             trace = FALSE)
  # extract just partial correlation result
  partial_cor_mat = glassoFast_result$wi
  # set diagonal values to 0; hierarchical clustering will give an error with NA diagonal values
  diag(partial_cor_mat) = NA
  # set row/colnames to match feature matrix
  rownames(partial_cor_mat) = colnames(load_data)
  colnames(partial_cor_mat) = colnames(load_data)
  # return the resulting partial correlations
  return(partial_cor_mat)
}
