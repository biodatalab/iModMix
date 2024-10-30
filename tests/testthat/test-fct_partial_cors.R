library(testthat)

# Create an example dataset
set.seed(123)
example_data <- data.frame(
  Feature_ID = paste0("Gene", 1:10),
  Sample1 = rnorm(10),
  Sample2 = rnorm(10),
  Sample3 = rnorm(10),
  Sample4 = rnorm(10)
)

# Add some rows with NA values to test filtering
example_data[1, 2] <- NA
example_data[2, 3] <- NA

# Define the partial_cors function here or ensure it is loaded in the environment

test_that("partial_cors returns a matrix with correct dimensions and NA diagonal", {
  result <- partial_cors(example_data)

  # Check that the result is a matrix
  expect_true(is.matrix(result))

  # Calculate the expected number of rows after filtering
  filtered_data <- example_data[rowSums(is.na(example_data[, -1])) <= 0.1 * (ncol(example_data) - 2), ]
  feature_mat_t <- t(filtered_data[, -1])
  feature_mat_t <- feature_mat_t[, apply(feature_mat_t, 2, function(x) length(unique(x)) > 1)]
  feature_mat_t <- as.matrix(scale(feature_mat_t))
  sd_values <- apply(feature_mat_t, 2, function(x) sd(x, na.rm = TRUE))
  filtered_indices <- which(sd_values > quantile(sd_values, 0.25))
  expected_rows <- length(filtered_indices)

  expect_equal(nrow(result), expected_rows)
  expect_equal(ncol(result), expected_rows)

  # Check that the diagonal contains NA
  expect_true(all(is.na(diag(result))))
})

test_that("partial_cors handles missing values correctly", {
  result <- partial_cors(example_data)

  # Check that rows with too many NAs have been removed
  expect_true(!"Gene1" %in% rownames(result))
  expect_true(!"Gene2" %in% rownames(result))
})

test_that("partial_cors scales the data correctly", {
  result <- partial_cors(example_data)

  # Check that the data has been scaled (mean 0 and standard deviation 1)
  scaled_data <- scale(t(example_data[, -c(1, ncol(example_data))]))
  expect_equal(colMeans(scaled_data, na.rm = TRUE), rep(0, ncol(scaled_data)), tolerance = 1e-8)
  expect_equal(apply(scaled_data, 2, sd, na.rm = TRUE), rep(1, ncol(scaled_data)), tolerance = 1e-8)
})

