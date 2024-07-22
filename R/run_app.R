#' Run the Shiny Application
#'
#' @param ... arguments to pass to golem_opts.
#' See `?golem::get_golem_options` for more details.
#' @inheritParams shiny::shinyApp
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
run_app <- function(
  onStart = NULL,
  options = list(),
  enableBookmarking = NULL,
  uiPattern = "/",
  ...
) {
  demo_data_path <- "Example_data/FloresData_K_TK"
  file_path_metab_exp <- file.path(demo_data_path, "Metab_exp.csv")
  file_path_metab_annot <- file.path(demo_data_path, "Metab_annot.csv")
  file_path_prot_exp <- file.path(demo_data_path, "Prot_exp.csv")
  file_path_prot_annot <- file.path(demo_data_path, "Prot_annot.csv")
  file_path_metadata <- file.path(demo_data_path, "Metadata.csv")

  precargados_metab_exp <<- read.csv(file_path_metab_exp)
  precargados_metab_annot <<- read.csv(file_path_metab_annot)
  precargados_prot_exp <<- read.csv(file_path_prot_exp)
  precargados_prot_annot <<- read.csv(file_path_prot_annot)
  precargados_metadata <<- read.csv(file_path_metadata)

  temp_dir <- tempdir()
  unzip(file.path(demo_data_path, "PartialCorMetabolites.csv.zip"), exdir = temp_dir)
  file_path_partial_cor_metab <- file.path(temp_dir, "PartialCorMetabolites.csv")
  precargados_partial_cor_metab <<- read.csv(file_path_partial_cor_metab, header = TRUE, row.names = 1, check.names = FALSE)
  precargados_partial_cor_metab <<- as.matrix(precargados_partial_cor_metab)

  unzip(file.path(demo_data_path, "PartialCorProt.csv.zip"), exdir = temp_dir)
  file_path_partial_cor_prot <- file.path(temp_dir, "PartialCorProt.csv")
  precargados_partial_cor_prot <<- read.csv(file_path_partial_cor_prot, header = TRUE, row.names = 1, check.names = FALSE)
  precargados_partial_cor_prot <<- as.matrix(precargados_partial_cor_prot)

  file_path_enrichment_mouse <- file.path(demo_data_path, "EnrichmentMouse.csv")
  precargados_enrichment_mouse <<- read.csv(file_path_enrichment_mouse, header = TRUE, row.names = 1, check.names = FALSE)


  # Definir funciones para cargar los datos
  load_metab_exp <<- function() {
    return(precargados_metab_exp)
  }
  load_metab_annot <<- function() {
    return(precargados_metab_annot)
  }
  load_prot_exp <<- function() {
    return(precargados_prot_exp)
  }
  load_prot_annot <<- function() {
    return(precargados_prot_annot)
  }
  load_metadata <<- function() {
    return(precargados_metadata)
  }
  load_partial_cor_metab <<- function() {
    return(precargados_partial_cor_metab)
  }
  load_partial_cor_prot <<- function() {
    return(precargados_partial_cor_prot)
  }
  load_enrichment_mouse <<- function() {
    return(precargados_enrichment_mouse)
  }

  with_golem_options(
    app = shinyApp(
      ui = app_ui,
      server = app_server,
      onStart = onStart,
      options = options,
      enableBookmarking = enableBookmarking,
      uiPattern = uiPattern
    ),
    golem_opts = list(...)
  )
}
