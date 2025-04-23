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
)
{
  demo_data_path <- paste0(here::here(),"/inst/Example_data/ccRCC4_Data")

  file_path_partial_cor_metab <- file.path(demo_data_path, "PartialCorMetabolites.rds")
  file_path_partial_cor_RNA <- file.path(demo_data_path, "PartialCorGenes.rds")
  file_path_enrichment <- file.path(demo_data_path, "Enrichment.rds")

  precargados_partial_cor_metab <<- readRDS(file_path_partial_cor_metab)
  precargados_partial_cor_RNA <<- readRDS(file_path_partial_cor_RNA)
  precargados_enrichment <<- readRDS(file_path_enrichment)


  load_partial_cor_metab <<- function() {
    return(precargados_partial_cor_metab)
  }
  load_partial_cor_RNA <<- function() {
    return(precargados_partial_cor_RNA)
  }
  load_enrichment <<- function() {
    return(precargados_enrichment)
  }

  demo_data_pathAll <- paste0(here::here(),"/inst/Example_data/FloresData_K_TK")

  file_path_partial_cor_metabAll <- file.path(demo_data_pathAll, "PartialCorMetabolites.rds")
  file_path_partial_cor_RNAAll <- file.path(demo_data_pathAll, "PartialCorProt.rds")
  file_path_enrichmentAll <- file.path(demo_data_pathAll, "EnrichmentMouse.rds")

  precargados_partial_cor_metabAll <<- readRDS(file_path_partial_cor_metabAll)
  precargados_partial_cor_RNAAll <<- readRDS(file_path_partial_cor_RNAAll)
  precargados_enrichmentAll <<- readRDS(file_path_enrichmentAll)

  load_partial_cor_metabAll <<- function() {
    return(precargados_partial_cor_metabAll)
  }
  load_partial_cor_RNAAll <<- function() {
    return(precargados_partial_cor_RNAAll)
  }
  load_enrichmentAll <<- function() {
    return(precargados_enrichmentAll)
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

