#' Assigment_genes_enrichr
#'
#' @description Enrichr terms for genes (Proteins and transcriptomics)
#' @param cluster_assignments_ProtGenes (data frame containing cluster_assignments and HMDB/Symbol)
#' @param database A list with all the available databases from enrichr
#' @return result_list data frame containing cluster_assignments and Enrichr terms
#' @examples
#' if (interactive()) {
#'   # Simulated cluster assignments with gene symbols
#'   cluster_assignments <- data.frame(
#'     feature = c("F1", "F2", "F3", "F4"),
#'     cluster = c("cluster_1", "cluster_1", "cluster_2", "cluster_2"),
#'     col = c("#FF0000", "#FF0000", "#00FF00", "#00FF00"),
#'     feature_name = c("TP53", "BRCA1", "EGFR", "MYC"),
#'     stringsAsFactors = FALSE
#'   )
#'
#'   # Run enrichment analysis using Enrichr (requires internet connection)
#'   enriched_results <- Assigment_genes_enrichr(
#'     cluster_assignments_ProtGenes = cluster_assignments,
#'     database = "GO_Biological_Process_2023"
#'   )
#'
#'   # View results
#'   head(enriched_results)
#' }
#'
#' @export
Assigment_genes_enrichr <- function(cluster_assignments_ProtGenes,
                                    database = "GO_Biological_Process_2023" ) {
  websiteLive <- getOption("enrichR.live")

  # Attempt to establish connection
  tryCatch(
    expr = {
      if (websiteLive) {
        enrichR::listEnrichrSites()
        enrichR::setEnrichrSite("Enrichr") # Human genes
      }
      if (websiteLive) dbs <- enrichR::listEnrichrDbs()
    },
    error = function(e) {
      message("Error: Unable to connect to Enrichr")
      return(NULL)
    }
  )

  dbs <- database
  Genes_enrich <- cluster_assignments_ProtGenes

  # New columns
  Genes_enrich$enriched_Term <- NA
  Genes_enrich$enriched_Overlap <- NA
  Genes_enrich$enriched_Genes <- NA
  Genes_enrich$enriched_P.value <- NA
  Genes_enrich$enriched_Adjusted.P.value <- NA

  # apply enrichr
  grouped_lists <- stats::aggregate(feature_name ~ cluster, data = Genes_enrich, FUN = list)


  for (i in seq_along(grouped_lists$cluster)) {
    cluster_id <- grouped_lists$cluster[i]
    feature_list <- grouped_lists$feature_name[[i]]

    if (websiteLive && length(feature_list) > 0) {
      enriched_result <- enrichR::enrichr(feature_list, dbs)
      if (!is.null(enriched_result[[database]]) && nrow(enriched_result[[database]]) > 0) {
        term <- enriched_result[[database]][["Term"]][1]
        overlap <- enriched_result[[database]][["Overlap"]][1]
        Genes <- enriched_result[[database]][["Genes"]][1]
        P.value <- enriched_result[[database]][["P.value"]][1]
        Adjusted.P.value <- enriched_result[[database]][["Adjusted.P.value"]][1]
        # Split the 'Term' column into 'enriched_Term' and 'enriched_GO'
        term_parts <- strsplit(term, "\\(")[[1]]
        enriched_Term <- term_parts[1]
        enriched_GO <- sub("\\)$", "", term_parts[2])

        # Update the new columns in the dataframe
        Genes_enrich[Genes_enrich$cluster == cluster_id, "enriched_Term"] <- enriched_Term
        Genes_enrich[Genes_enrich$cluster == cluster_id, "enriched_GO"] <- enriched_GO
        Genes_enrich[Genes_enrich$cluster == cluster_id, "enriched_Overlap"] <- overlap
        Genes_enrich[Genes_enrich$cluster == cluster_id, "enriched_Genes"] <- Genes
        Genes_enrich[Genes_enrich$cluster == cluster_id, "enriched_P.value"] <- P.value
        Genes_enrich[Genes_enrich$cluster == cluster_id, "enriched_Adjusted.P.value"] <- Adjusted.P.value
      }
    }
  }

  return(Genes_enrich)
}
