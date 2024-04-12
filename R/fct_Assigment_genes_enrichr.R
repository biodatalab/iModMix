#' Assigment_genes_enrichr
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @export
#'

Assigment_genes_enrichr <- function(cluster_assignments_ProtGenes,
                                    #species = "Human",
                                    database = "GO_Biological_Process_2023" ) {
  websiteLive <- getOption("enrichR.live")
  if (websiteLive) {
    enrichR::listEnrichrSites()
    enrichR::setEnrichrSite("Enrichr") # Human genes
  }
  if (websiteLive) dbs <- enrichR::listEnrichrDbs()

  # if (species == "Human") {
  #   dbs <- c("GO_Molecular_Function_2023", "GO_Cellular_Component_2023", "GO_Biological_Process_2023", "HDSigDB_Human_2021")
  # } else if (species == "Mouse") {
  #   dbs <- c("Mouse_Gene_Atlas","WikiPathways_2019_Mouse","KEGG_2019_Mouse", "HDSigDB_Mouse_2021")
  # } else {
  #   stop("Invalid species. Please provide 'Human' or 'Mouse'.")
  # }

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

    if (websiteLive) {
      enriched_result <- enrichR::enrichr(feature_list, dbs)
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

  return(Genes_enrich)
}
