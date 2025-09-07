#' load_data
#'
#' @description A function to load and preprocess the expression data.
#' @param Expression_mat A feature matrix (e.g., gene expression) with samples in rows and features (e.g., genes) in columns. Row names must be unique.
#' @return A prepossessed data matrix.
#' @examples
#' # Simulated expression matrix with missing values
#' set.seed(123)
#' mat <- matrix(rnorm(1000), nrow = 10, ncol = 100)
#' colnames(mat) <- paste0("Gene", 1:100)
#' rownames(mat) <- paste0("Sample", 1:10)
#' mat[sample(length(mat), 50)] <- NA  # introduce some NAs
#'
#' # Add a Feature_ID column to mimic expected input
#' Expression_mat <- cbind(Feature_ID = paste0("F", 1:10), mat)
#' Expression_mat <- as.data.frame(Expression_mat)
#'
#' # Convert Feature_ID to character (if needed)
#' Expression_mat$Feature_ID <- as.character(Expression_mat$Feature_ID)
#'
#' # Run the function
#' processed_data <- load_data(Expression_mat)
#'
#' @export
load_data <- function(Expression_mat = Expression_mat) {
  data <- Expression_mat

  # Calculate the number of missing values in each row
  data$missing_count <- rowSums(is.na(data))

  # Filter rows with missing values in more than 10% of the columns
  feature_mat <- subset(data, missing_count <= 0.1 * (ncol(data) - 2))

  # Check if any rows remain after filtering
  if (nrow(feature_mat) == 0) {
    stop("No rows remain after filtering missing values")
  }

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
      feature_mat_t[, order(sd_values[filtered_indices], decreasing = TRUE)[seq_len(20000)]]
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
