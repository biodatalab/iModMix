#' Modules_correlation
#'
#' @description Calculates correlations between omic modules.
#'
#' @param eigengenes_Prot A feature matrix with the first principal component of each protein cluster
#' @param eigengenes_metab A feature matrix with the first principal component of each metabolomic cluster
#' @param cluster_assignments_Prot data frame containing cluster_assignments and Enrichr terms
#' @param cluster_assignments_metab data frame containing cluster_assignments
#' @return  Return the Top_correlations between Prot and metab eigengenes, graph of red, correlation plot
#' @export
Modules_correlation <- function(eigengenes_Prot, eigengenes_metab, cluster_assignments_Prot, cluster_assignments_metab, threshold = 0.5) {
  # Calculate correlation
  cor_Prot_metab_WGCNA <- cor(eigengenes_Prot, eigengenes_metab, method = 'spearman', use = "pairwise.complete.obs")

  # Create a histogram of correlation
  hist(cor_Prot_metab_WGCNA, main = "Histogram of Correlation")

  # Convert the correlation matrix to a long format
  cor_Prot_metab_list <- reshape2::melt(cor_Prot_metab_WGCNA, id.var = "Correlation")
  colnames(cor_Prot_metab_list) <- c("Prot_module", "Metab_module", "Correlation")

  # Filter the correlation list
  Top_cor_Prot_metab <- subset(cor_Prot_metab_list, abs(Correlation) >= threshold)
  Top_cor_Prot_metab$Correlation <- round(Top_cor_Prot_metab$Correlation, 2)

  # Remove the "ME" prefix from Prot_module and Metab_module columns
  Top_cor_Prot_metab[c("Prot_module", "Metab_module")] <- lapply(Top_cor_Prot_metab[c("Prot_module", "Metab_module")], function(x) sub("^ME", "", x))

  Count_Prot <- table(cluster_assignments_Prot$col)
  Count_Metab <- table(cluster_assignments_metab$col)

  Top_cor_Prot_metab$Prot_count <- paste(Count_Prot[Top_cor_Prot_metab$Prot_module], "genes", sep = " ")
  Top_cor_Prot_metab$Metab_count <- paste(Count_Metab[Top_cor_Prot_metab$Metab_module], "metabolites", sep = " ")
  Top_cor_Prot_metab$Prot_Module_id <- paste("Module", as.numeric(factor(Top_cor_Prot_metab$Prot_module)), sep = " ")
  Top_cor_Prot_metab$Metab_Module_id <- paste("Module", as.numeric(factor(Top_cor_Prot_metab$Metab_module)), sep = " ")
  Top_cor_Prot_metab$Prot_label0 <- paste(Top_cor_Prot_metab$Prot_Module_id, Top_cor_Prot_metab$Prot_count, sep = "\n")
  Top_cor_Prot_metab$Metab_label <- paste(Top_cor_Prot_metab$Metab_Module_id, Top_cor_Prot_metab$Metab_count, sep = "\n")
  Top_cor_Prot_metab$Enriched_Term_Genes <- cluster_assignments_Prot$enriched_Term[match(Top_cor_Prot_metab$Prot_module, cluster_assignments_Prot$col)]
  Top_cor_Prot_metab$Prot_label <- paste(Top_cor_Prot_metab$Prot_label0, Top_cor_Prot_metab$Enriched_Term_Genes, sep = "\n")

  Top_cor_Prot_metab$Enriched_Overlap <- cluster_assignments_Prot$enriched_Overlap[match(Top_cor_Prot_metab$Prot_module, cluster_assignments_Prot$col)]
  Top_cor_Prot_metab$Enriched_Genes <- cluster_assignments_Prot$enriched_Genes[match(Top_cor_Prot_metab$Prot_module, cluster_assignments_Prot$col)]
  Top_cor_Prot_metab$Enriched_P.value <- cluster_assignments_Prot$enriched_P.value[match(Top_cor_Prot_metab$Prot_module, cluster_assignments_Prot$col)]
  Top_cor_Prot_metab$Enriched_Adjusted.P.value <- cluster_assignments_Prot$enriched_Adjusted.P.value[match(Top_cor_Prot_metab$Prot_module, cluster_assignments_Prot$col)]
  Top_cor_Prot_metab$Enriched_GO <- cluster_assignments_Prot$enriched_GO[match(Top_cor_Prot_metab$Prot_module, cluster_assignments_Prot$col)]

  # Print the filtered list
  #print(Top_cor_Prot_metab[,c('Metab_Module_id',   'Metab_count', "Prot_Module_id", 'Prot_count', 'Correlation', 'Enriched_Term_Genes',   "Enriched_Overlap" ,   "Enriched_Genes",  "Enriched_P.value",  "Enriched_Adjusted.P.value", "Enriched_GO")])

  # Create the network graph
  #filtered_cor_Prot_metab_list = Top_cor_Prot_metab[, c("Prot_label0", "Metab_label", "Correlation")]
  filtered_cor_Prot_metab_list = Top_cor_Prot_metab[, c("Prot_label", "Metab_label", "Correlation")]
  network <- igraph::graph_from_data_frame(filtered_cor_Prot_metab_list, directed = FALSE)
  igraph::E(network)$label <- filtered_cor_Prot_metab_list$Correlation

  # Conditions for node type and color
  condicion_tipo <- ifelse(grepl("Gene", igraph::V(network)$name), "lightgreen", "#E69F00")
  color_text <- ifelse(grepl("Gene", igraph::V(network)$name), "darkgreen", "orange")

  # Plot the network graph
  # plot(
  #   network,
  #   edge.label = igraph::E(network)$label,
  #   vertex.size = 2,
  #   vertex.color = condicion_tipo,
  #   vertex.label.color = color_text,
  #   edge.label.cex = 0.8,
  #   edge.label.color = "black",
  #   edge.width = 2,
  #   edge.color = "gray",
  #   main = "Modules Correlation"
  # )

  # Return the Top_cor_Prot_metab data frame
  Top_cor_Prot_metab = Top_cor_Prot_metab[,c("Metab_Module_id" ,  "Metab_module", "Metab_count",   "Prot_Module_id",  "Prot_module", "Prot_count",   "Correlation",  'Enriched_Term_Genes',   "Enriched_Overlap" ,   "Enriched_Genes",  "Enriched_P.value",  "Enriched_Adjusted.P.value", "Enriched_GO")]
  #Top_cor_Prot_metab = Top_cor_Prot_metab[,c("Metab_Module_id" ,  "Metab_module", "Metab_count",   "Prot_Module_id",  "Prot_module", "Prot_count",   "Correlation")]
  return(list(Top_cor_Prot_metab = Top_cor_Prot_metab,
              filtered_cor_Prot_metab_list = filtered_cor_Prot_metab_list,
              cor_Prot_metab_WGCNA = cor_Prot_metab_WGCNA))
  }


