#' load_data
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
load_data <- function(file_name) {
  demo_data_path <- "Example_data/FloresData_K_TK"
  file_path <- file.path(demo_data_path, file_name)
  read.csv(file_path)
}
