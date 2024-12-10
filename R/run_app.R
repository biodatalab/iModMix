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
  # ccRCC4_Data
  demo_data_path <- "inst/Example_data/ccRCC4_Data"
  file_path_metab_exp <- file.path(demo_data_path, "Metab_exp.rds")
  file_path_metab_annot <- file.path(demo_data_path, "Metab_annot.rds")
  file_path_RNA_exp <- file.path(demo_data_path, "RNA_exp.rds")
  file_path_RNA_annot <- file.path(demo_data_path, "RNA_annot.rds")
  file_path_metadata <- file.path(demo_data_path, "Metadata.rds")
  file_path_partial_cor_metab <- file.path(demo_data_path, "PartialCorMetabolites.rds")
  file_path_partial_cor_RNA <- file.path(demo_data_path, "PartialCorGenes.rds")
  file_path_enrichment <- file.path(demo_data_path, "Enrichment.rds")

  precargados_metab_exp <<- readRDS(file_path_metab_exp)
  precargados_metab_annot <<- readRDS(file_path_metab_annot)
  precargados_RNA_exp <<- readRDS(file_path_RNA_exp)
  precargados_RNA_annot <<- readRDS(file_path_RNA_annot)
  precargados_metadata <<- readRDS(file_path_metadata)
  precargados_partial_cor_metab <<- readRDS(file_path_partial_cor_metab)
  precargados_partial_cor_RNA <<- readRDS(file_path_partial_cor_RNA)
  precargados_enrichment <<- readRDS(file_path_enrichment)

  # FloresData_K_TK
  demo_data_pathAll <- "inst/Example_data/FloresData_K_TK"
  file_path_metab_expAll <- file.path(demo_data_pathAll, "Metab_exp.rds")
  file_path_metab_annotAll <- file.path(demo_data_pathAll, "Metab_annot.rds")
  file_path_RNA_expAll <- file.path(demo_data_pathAll, "Prot_exp.rds")
  file_path_RNA_annotAll <- file.path(demo_data_pathAll, "Prot_annot.rds")
  file_path_metadataAll <- file.path(demo_data_pathAll, "Metadata.rds")
  file_path_partial_cor_metabAll <- file.path(demo_data_pathAll, "PartialCorMetabolites.rds")
  file_path_partial_cor_RNAAll <- file.path(demo_data_pathAll, "PartialCorProt.rds")
  file_path_enrichmentAll <- file.path(demo_data_pathAll, "EnrichmentMouse.rds")

  precargados_metab_expAll <<- readRDS(file_path_metab_expAll)
  precargados_metab_annotAll <<- readRDS(file_path_metab_annotAll)
  precargados_RNA_expAll <<- readRDS(file_path_RNA_expAll)
  precargados_RNA_annotAll <<- readRDS(file_path_RNA_annotAll)
  precargados_metadataAll <<- readRDS(file_path_metadataAll)
  precargados_partial_cor_metabAll <<- readRDS(file_path_partial_cor_metabAll)
  precargados_partial_cor_RNAAll <<- readRDS(file_path_partial_cor_RNAAll)
  precargados_enrichmentAll <<- readRDS(file_path_enrichmentAll)

  # Definir funciones para cargar los datos
  load_metab_exp <<- function() {
    return(precargados_metab_exp)
  }
  load_metab_annot <<- function() {
    return(precargados_metab_annot)
  }
  load_RNA_exp <<- function() {
    return(precargados_RNA_exp)
  }
  load_RNA_annot <<- function() {
    return(precargados_RNA_annot)
  }
  load_metadata <<- function() {
    return(precargados_metadata)
  }
  load_partial_cor_metab <<- function() {
    return(precargados_partial_cor_metab)
  }
  load_partial_cor_RNA <<- function() {
    return(precargados_partial_cor_RNA)
  }
  load_enrichment <<- function() {
    return(precargados_enrichment)
  }

  load_metab_expAll <<- function() {
    return(precargados_metab_expAll)
  }
  load_metab_annotAll <<- function() {
    return(precargados_metab_annotAll)
  }
  load_RNA_expAll <<- function() {
    return(precargados_RNA_expAll)
  }
  load_RNA_annotAll <<- function() {
    return(precargados_RNA_annotAll)
  }
  load_metadataAll <<- function() {
    return(precargados_metadataAll)
  }
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

