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

  cor_Prot_metab_list1 <- cor_Prot_metab_list[order(abs(cor_Prot_metab_list$Correlation), decreasing = TRUE), ][1:5, ]
  cor_Prot_metab_list2 <- subset(cor_Prot_metab_list, abs(Correlation) >= threshold)
  Top_cor_Prot_metab <- if (nrow(cor_Prot_metab_list1) >= nrow(cor_Prot_metab_list2)) {
    cor_Prot_metab_list1
  } else {
    cor_Prot_metab_list2
  }

  #Top_cor_Prot_metab$Correlation <- round(Top_cor_Prot_metab$Correlation, 2)

  # Remove the "ME" prefix from Prot_module and Metab_module columns
  Top_cor_Prot_metab[c("Prot_module", "Metab_module")] <- lapply(Top_cor_Prot_metab[c("Prot_module", "Metab_module")], function(x) sub("^ME", "", x))

  # Edges to funcion visnetwork
  edges <- Top_cor_Prot_metab[1:min(nrow(Top_cor_Prot_metab), 30), ]
  edges$label <-as.character(edges$Correlation)
  #edges$length = (1-abs(edges$Correlation))*10000 #
  edges$dashes = ifelse(abs(edges$Correlation) < 0.50, TRUE, FALSE)
  edges$title <-as.character(edges$Correlation)
  edges$smooth = ifelse(abs(edges$Correlation) < 0.50, TRUE, FALSE)
  edges$shadow = TRUE
  edges <- subset(edges, select = -c(Correlation))
  #colnames(edges) <- c("Prot_module" = "from", "Metab_module" = "to", "label", "length", "dashes", "title", "smooth", "shadow")
  colnames(edges) <- c("Prot_module" = "from", "Metab_module" = "to", "label", "dashes", "title", "smooth", "shadow")

  Count_Prot <- table(cluster_assignments_Prot$col)
  Count_Metab <- table(cluster_assignments_metab$col)

  unique_from <- unique(edges$from)
  label_from <- paste0("Module", seq_along(unique_from))
  value_from <- Count_Prot[match(unique_from, names(Count_Prot))]
  shape_from <- "triangle"
  title_from0 = paste(value_from, "genes", sep = " ")
  color_from <- "darkgreen"

  Enriched_Term_net <- cluster_assignments_Prot$enriched_Term[match(unique_from, cluster_assignments_Prot$col)]
  Enriched_Term_net <- ifelse(nchar(Enriched_Term_net) > 15, paste0(substring(Enriched_Term_net, 1, 15), "..."), Enriched_Term_net)
  title_from <- paste(title_from0, Enriched_Term_net, sep = "\n")



  unique_to <- unique(edges$to)
  label_to <- paste0("Module", seq_along(unique_to))
  value_to <- Count_Metab[match(unique_to, names(Count_Metab))]
  shape_to <- "diamond"
  title_to = paste(value_to, "metabolites", sep = " ")
  color_to <- "orange"

  #nodes to function Visnetwork
  nodes <- data.frame(id = c(unique_from, unique_to),
                      #label = c(label_from, label_to),
                      label = c(title_from, title_to),
                      value = c(value_from, value_to),
                      shape = c(rep(shape_from, length(unique_from)), rep(shape_to, length(unique_to))),
                      #title = c(title_from, title_to),
                      title = c(label_from, label_to),
                      color = c(rep(color_from, length(unique_from)), rep(color_to, length(unique_to))),
                      shadow = TRUE)
  nodes <- nodes[,c("id", "label", "value", "shape", "title", "color", "shadow")]



  edges[["from"]] <- sub("^#", "", edges[["from"]])
  edges[["to"]] <- sub("^#", "", edges[["to"]])

  rownames(nodes) <- NULL
  nodes[["id"]] <- sub("^#", "", nodes[["id"]])



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

  # Create the network graph
  filtered_cor_Prot_metab_list = Top_cor_Prot_metab[, c("Prot_label", "Metab_label", "Correlation")]

  Top_cor_Prot_metab = Top_cor_Prot_metab[,c("Metab_Module_id" ,  "Metab_module", "Metab_count",   "Prot_Module_id",  "Prot_module", "Prot_count",   "Correlation",  'Enriched_Term_Genes',   "Enriched_Overlap" ,   "Enriched_Genes",  "Enriched_P.value",  "Enriched_Adjusted.P.value", "Enriched_GO")]

  return(list(Top_cor_Prot_metab = Top_cor_Prot_metab,
              filtered_cor_Prot_metab_list = filtered_cor_Prot_metab_list,
              cor_Prot_metab_WGCNA = cor_Prot_metab_WGCNA,
              nodes = nodes,
              edges = edges))
  }


