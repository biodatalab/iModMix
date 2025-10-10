#' fctiModMix2SE
#'
#' @description Converts expression data and corresponding sample metadata
#' into a Bioconductor-compliant SummarizedExperiment object.
#' @param dataExp A data frame with metabolites (features) in rows and samples in columns.
#' The first column should contain feature IDs (e.g., metabolite names).
#' @param metadata A data frame containing sample metadata.
#' Must include a column named "Sample" that matches the column names of \code{dataExp}
#' (excluding the first column).
#'
#' @return A \code{SummarizedExperiment} object.
#' @examples
#' dataExp <- data.frame(Feature_ID = paste0("M", 1:5),
#'                       S1 = rnorm(5), S2 = rnorm(5))
#' metadata <- data.frame(Sample = c("S1", "S2"),
#'                        Group = c("A", "B"))
#' se <- fctiModMix2SE(dataExp, metadata)
#' se
#' @export
fctiModMix2SE <- function(dataExp, metadata) {
  if (!requireNamespace("SummarizedExperiment", quietly = TRUE)) {
    stop("Package 'SummarizedExperiment' is required but not installed.")
  }
  
  # Check structure
  if (!"Feature_ID" %in% colnames(dataExp)) {
    stop("'dataExp' must contain a column named 'Feature_ID'.")
  }
  if (!"Sample" %in% colnames(metadata)) {
    stop("'metadata' must contain a column named 'Sample'.")
  }
  
  # Prepare assay matrix
  assay_data <- as.matrix(dataExp[, -1])
  rownames(assay_data) <- dataExp$Feature_ID
  
  # Match metadata order to samples
  metadata <- metadata[match(colnames(assay_data), metadata$Sample), ]
  
  # Create SummarizedExperiment
  SummarizedExperiment::SummarizedExperiment(
    assays = list(counts = assay_data),
    colData = metadata
  )
}
