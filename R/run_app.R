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
  demo_data_path <- "inst/Example_data/ccRCC4_Data"
  file_path_metab_exp <- file.path(demo_data_path, "Metab_exp.csv")
  file_path_metab_annot <- file.path(demo_data_path, "Metab_annot.csv")
  file_path_RNA_exp <- file.path(demo_data_path, "RNA_exp.csv")
  file_path_RNA_annot <- file.path(demo_data_path, "RNA_annot.csv")
  file_path_metadata <- file.path(demo_data_path, "Metadata.csv")

  precargados_metab_exp <<- read.csv(file_path_metab_exp)
  precargados_metab_annot <<- read.csv(file_path_metab_annot)
  precargados_RNA_exp <<- read.csv(file_path_RNA_exp)
  precargados_RNA_annot <<- read.csv(file_path_RNA_annot)
  precargados_metadata <<- read.csv(file_path_metadata)

  temp_dir <- tempdir()
  unzip(file.path(demo_data_path, "PartialCorMetabolites.csv.zip"), exdir = temp_dir)
  file_path_partial_cor_metab <- file.path(temp_dir, "PartialCorMetabolites.csv")
  precargados_partial_cor_metab <<- read.csv(file_path_partial_cor_metab, header = TRUE, row.names = 1, check.names = FALSE)
  precargados_partial_cor_metab <<- as.matrix(precargados_partial_cor_metab)

  unzip(file.path(demo_data_path, "PartialCorGenes.csv.zip"), exdir = temp_dir)
  file_path_partial_cor_RNA <- file.path(temp_dir, "PartialCorGenes.csv")
  precargados_partial_cor_RNA <<- read.csv(file_path_partial_cor_RNA, header = TRUE, row.names = 1, check.names = FALSE)
  precargados_partial_cor_RNA <<- as.matrix(precargados_partial_cor_RNA)

  file_path_enrichment <- file.path(demo_data_path, "Enrichment.csv")
  precargados_enrichment <<- read.csv(file_path_enrichment, header = TRUE, row.names = 1, check.names = FALSE)


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

  ### data Flores

  demo_data_pathAll <- "inst/Example_data/FloresData_K_TK"
  file_path_metab_expAll <- file.path(demo_data_pathAll, "Metab_exp.csv")
  file_path_metab_annotAll <- file.path(demo_data_pathAll, "Metab_annot.csv")
  file_path_RNA_expAll <- file.path(demo_data_pathAll, "Prot_exp.csv")
  file_path_RNA_annotAll <- file.path(demo_data_pathAll, "Prot_annot.csv")
  file_path_metadataAll <- file.path(demo_data_pathAll, "Metadata.csv")

  precargados_metab_expAll <<- read.csv(file_path_metab_expAll)
  precargados_metab_annotAll <<- read.csv(file_path_metab_annotAll)
  precargados_RNA_expAll <<- read.csv(file_path_RNA_expAll)
  precargados_RNA_annotAll <<- read.csv(file_path_RNA_annotAll)
  precargados_metadataAll <<- read.csv(file_path_metadataAll)

  temp_dirAll <- tempdir()
  unzip(file.path(demo_data_pathAll, "PartialCorMetabolites.csv.zip"), exdir = temp_dirAll)
  file_path_partial_cor_metabAll <- file.path(temp_dirAll, "PartialCorMetabolites.csv")
  precargados_partial_cor_metabAll <<- read.csv(file_path_partial_cor_metabAll, header = TRUE, row.names = 1, check.names = FALSE)
  precargados_partial_cor_metabAll <<- as.matrix(precargados_partial_cor_metabAll)

  unzip(file.path(demo_data_pathAll, "PartialCorProt.csv.zip"), exdir = temp_dirAll)
  file_path_partial_cor_RNAAll <- file.path(temp_dirAll, "PartialCorProt.csv")
  precargados_partial_cor_RNAAll <<- read.csv(file_path_partial_cor_RNAAll, header = TRUE, row.names = 1, check.names = FALSE)
  precargados_partial_cor_RNAAll <<- as.matrix(precargados_partial_cor_RNAAll)

  file_path_enrichmentAll <- file.path(demo_data_pathAll, "EnrichmentMouse.csv")
  precargados_enrichmentAll <<- read.csv(file_path_enrichmentAll, header = TRUE, row.names = 1, check.names = FALSE)


  # Definir funciones para cargar los datos
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
