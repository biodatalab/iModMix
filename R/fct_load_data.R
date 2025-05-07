#' load_data
#'
#' @description A function to load and preprocess the expression data.
#' @param Expression_mat A feature matrix (e.g., gene expression) with samples in rows and features (e.g., genes) in columns. Row names must be unique.
#' @return A prepossessed data matrix.
#'
#' @export
load_data <- function(Expression_mat = Expression_mat) {
  data <- Expression_mat

  # Calculate the number of missing values in each row
  data$missing_count <- rowSums(is.na(data))

  # Filter rows with missing values in more than 10% of the columns
  feature_mat <- subset(data, missing_count <= 0.1 * (ncol(data) - 2))

  # Extract feature IDs and transpose the feature matrix
  features <- feature_mat$Feature_ID
  feature_mat_t <- t(feature_mat[, -c(1, ncol(feature_mat))])
  colnames(feature_mat_t) <- features

  # Remove columns with only one unique value
  feature_mat_t <- feature_mat_t[, apply(feature_mat_t, 2, function(x) length(unique(x)) > 1)]

  # Calculate standard deviation for each column
  sd_values <- apply(feature_mat_t, 2, function(x) sd(x, na.rm = TRUE))

  # Filter columns with standard deviation above the 25th percentile
  filtered_indices <- which(sd_values > quantile(sd_values, 0.25))

  # Limit the number of columns to 20000 if necessary
  feature_mat_t <- if (length(sd_values) > 20000) {
    if (length(filtered_indices) > 20000) {
      feature_mat_t[, order(sd_values[filtered_indices], decreasing = TRUE)[1:20000]]
    } else {
      feature_mat_t[, filtered_indices]
    }
  } else {
    feature_mat_t[, ]
  }

  # Impute missing values using k-nearest neighbors
  feature_mat_t_imp <- impute::impute.knn(feature_mat_t, k = min(10, nrow(feature_mat_t)))
  feature_mat_t_imp_data <- feature_mat_t_imp$data

  # Scale the data
  feature_mat_t_imp_data <- as.matrix(scale(feature_mat_t_imp_data))

  return(feature_mat_t_imp_data)
}
