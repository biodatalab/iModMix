#' perform_classification
#'
#' @description Performs classification using different methods such as Welch’s T-test, Random Forest, and K-Nearest Neighbors.
#' @param eigengene_data A data frame of eigengenes organized with patient IDs in rows and variables in columns.
#' @param metadata A data frame containing metadata, with a column call "Sample" that matches patient IDs in eigengene_data.
#' @param phenotype_variable The variable selected by the user in the Shiny app (response variable).
#' @param significance_threshold A numeric value to filter p-value. Default is 00.5.
#' @return A data frame with metrics such as AUC, Accuracy, and Error Rate for each binary classification.
#' @importFrom stats p.adjust
#' @examples
#' # Simulate eigengene data
#' set.seed(123)
#' eigengene_data <- as.data.frame(matrix(rnorm(100), nrow = 10, ncol = 10))
#' colnames(eigengene_data) <- paste0("ME", 1:10)
#' rownames(eigengene_data) <- paste0("Sample", 1:10)
#' # Simulate metadata with a binary phenotype
#' metadata <- data.frame(
#'   Sample = paste0("Sample", 1:10),
#'   Phenotype = rep(c("A", "B"), each = 5),
#'   stringsAsFactors = FALSE
#' )
#' # Run classification
#' result <- perform_classification(
#'   eigengene_data = eigengene_data,
#'   metadata = metadata,
#'   phenotype_variable = "Phenotype",
#'   significance_threshold = 0.05
#' )
#'
#' # View results
#' head(result$result)
#'
#' @export
perform_classification <- function(eigengene_data, metadata, phenotype_variable, significance_threshold = 0.05) {
  requireNamespace("ggplot2", quietly = TRUE)
  if (is.null(phenotype_variable)) {
    return(NULL)
  }
  metadata_subset <- metadata[, c("Sample", phenotype_variable), drop = FALSE]
  merged_data <- merge(eigengene_data, metadata_subset, by.x = "row.names", by.y = "Sample")
  predictors <- names(merged_data)[-c(1, ncol(merged_data))]
  response <- as.factor(merged_data[[phenotype_variable]])

  result_list <- list()
  plot_list <- list()

  if (length(levels(response)) == 2) {
    level_class <- unique(response)
    t_test_results <- purrr::map_df(predictors, function(x) {
      t_test_result <- t.test(merged_data[[x]] ~ response, var.equal = FALSE)
      data.frame(
        Variable = x,
        Class = paste(level_class[[1]], "vs", level_class[[2]]),
        Result_t = round(t_test_result$statistic, 4),
        Result_pValue = round(t_test_result$p.value, 4)
      )
    })
      t_test_results$Adjusted_pValue <- round(stats::p.adjust(t_test_results$Result_pValue, method = "BH"), 4)
      t_test_results <- t_test_results[t_test_results$Result_pValue <= significance_threshold, ]
      t_test_results <- t_test_results[order(t_test_results$Result_pValue), ]

    result_list[["Comparison"]] <- t_test_results

    boxplot_data <- tidyr::gather(merged_data, key = "Variable", value = "Expression", predictors)
    boxplot_data$Class <- response
    boxplot_data_filtered <- boxplot_data[boxplot_data$Variable %in% t_test_results$Variable, ]

    p <- ggplot2::ggplot(boxplot_data_filtered, ggplot2::aes(x = Variable, y = Expression, fill = Class)) +
      ggplot2::geom_boxplot() +
      ggplot2::labs(title = paste(level_class[[1]], "vs", level_class[[2]]))

    plot_list[["Comparison"]] <- p

  } else {
    for (class_label in levels(response)) {
      binary_response <- as.factor(ifelse(response == class_label, "Class", "Rest"))

      t_test_results <- purrr::map_df(predictors, function(x) {
        t_test_result <- t.test(merged_data[[x]] ~ binary_response, var.equal = FALSE)
        data.frame(
          Variable = x,
          Class = paste(class_label, "vs Rest"),
          Result_t = t_test_result$statistic,
          Result_pValue = t_test_result$p.value
        )
      })

      t_test_results$Adjusted_pValue <- stats::p.adjust(t_test_results$Result_pValue, method = "BH")
      t_test_results <- t_test_results[t_test_results$Result_pValue <= significance_threshold, ]
      t_test_results <- t_test_results[order(t_test_results$Result_pValue), ]

      result_list[[paste(class_label, "vs Rest")]] <- t_test_results

      boxplot_data <- tidyr::gather(merged_data, key = "Variable", value = "Expression", predictors)
      boxplot_data$Class <- binary_response
      boxplot_data_filtered <- boxplot_data[boxplot_data$Variable %in% t_test_results$Variable, ]

      p <- ggplot2::ggplot(boxplot_data_filtered, ggplot2::aes(x = Variable, y = Expression, fill = Class)) +
        ggplot2::geom_boxplot() +
        ggplot2::labs(title = paste(class_label, " vs Rest"))

      plot_list[[paste(class_label, "vs Rest")]] <- p
    }
  }
  result <- do.call(rbind, result_list)
  result <- result[order(result$Result_pValue), ]
  result$Result_t <- format(result$Result_t, scientific = TRUE)
  result$Result_pValue <- format(result$Result_pValue, scientific = TRUE)
  result$Adjusted_pValue <- format(result$Adjusted_pValue, scientific = TRUE)
  return(list(result = result, plots = plot_list))
}
