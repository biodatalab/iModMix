#' partial_cors
#'
#' @description Calculates partial correlations based on graphical lasso (https://www.rdocumentation.org/packages/glassoFast/versions/1.0/topics/glassoFast).
#' @param load_data A prepossessed data matrix resulting from "load data" function.
#' @param rho The regularization parameter for lasso. (a non-negative value or a p by p matrix of regularization parameters).
#' @return partial_cor_mat A partial correlation matrix with NA's in the diagonal.
#' @examples
#' # Simulate a small expression matrix
#' set.seed(123)
#' mat <- matrix(rnorm(100), nrow = 10, ncol = 10)
#' colnames(mat) <- paste0("Gene", 1:10)
#' rownames(mat) <- paste0("Sample", 1:10)
#'
#' # Run partial correlation
#' partial_cor_matrix <- partial_cors(mat, rho = 0.25)
#'
#' # View a portion of the result
#' partial_cor_matrix[1:5, 1:5]
#'
#' @export
partial_cors <- function(load_data, rho = .25) {
  # generate covariance matrix
  cov_mat <- cov(as.matrix(load_data), use = "pairwise.complete.obs")
  # calculate partial correlations
  glassoFast_result <- glassoFast::glassoFast(cov_mat, rho = rho, thr = 1e-04,
                                             maxIt = 10000, start = "cold",
                                             trace = FALSE)
  # extract just partial correlation result
  partial_cor_mat <- glassoFast_result$wi
  # set diagonal values to 0; hierarchical clustering will give an error with NA diagonal values
  diag(partial_cor_mat) <- NA
  # set row/colnames to match feature matrix
  rownames(partial_cor_mat) <- colnames(load_data)
  colnames(partial_cor_mat) <- colnames(load_data)
  # return the resulting partial correlations
  return(partial_cor_mat)
}
