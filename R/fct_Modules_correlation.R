#' Modules_correlation
#'
#' @description Calculates correlations between omic modules.
#' @param eigengenes_list List with the first principal component of each cluster.
#' @param cluster_list list containing cluster_assignments matrix.
#' @param threshold A numeric value to filter correlations. Default is 0.5.
#' @return  Return the Top_correlations between Prot and metab eigengenes, graph of red, correlation plot.
#' @export
Modules_correlation <- function(eigengenes_list, cluster_list, threshold = 0.50) {
  n <- length(eigengenes_list)
  eigengenes <- eigengenes_list
  clusters <- cluster_list

  cor_list <- list()
  for (i in 1:(n-1)) {
    for (j in (i+1):n) {
      cor_matrix <- stats::cor(eigengenes[[i]], eigengenes[[j]], method = 'spearman', use = "pairwise.complete.obs")
      cor_df <- reshape2::melt(cor_matrix, varnames = c("from", "to"))
      cor_df$from <- sub("^ME", paste0("D", i), cor_df$from)
      cor_df$to <- sub("^ME", paste0("D", j), cor_df$to)
      cor_df <-  cor_df[order(abs(cor_df$value), decreasing = TRUE), ]
      cor_df$value <- round(cor_df$value, 4)
      cor_list[[paste0("cor_Data", i, "_Data", j)]] <- cor_df
    }
  }

  cor_listPlot <- list()
  for (i in 1:(n-1)) {
    for (j in (i+1):n) {
      cor_matrix <- stats::cor(eigengenes[[i]], eigengenes[[j]], method = 'spearman', use = "pairwise.complete.obs")
      cor_listPlot[[paste0("cor_Data", i, "_Data", j)]] <- cor_matrix
    }
  }

  cor_top_list <- lapply(cor_list, function(cor_df) {
    cor_top_list1 <- cor_df[1:3, ]
    cor_top_list2 <- subset(cor_df, abs(value) >= threshold)
    if (nrow(cor_top_list1) >= nrow(cor_top_list2)) {
      cor_top_list1
    } else {
      cor_top_list2
    }
  })

  cor_top <- do.call(rbind, cor_top_list)

  edges <- cor_top
  edges$label <- as.character(round(edges$value, 2))
  edges$dashes <- ifelse(abs(edges$value) < threshold, TRUE, FALSE)
  edges$title <- as.character(edges$value)
  edges$smooth <- ifelse(abs(edges$value) < threshold, TRUE, FALSE)
  edges$shadow <- TRUE
  edges <- subset(edges, select = -c(value))

  nodes <- data.frame()
  shapes <- c("diamond", "triangle", "dot")
  colors <- c("orange", "darkgreen", "darkblue")

  for (k in 1:length(cor_top_list)) {
    if (length(cor_top_list) == 1) {
      i <- 1
      j <- 2
    } else if (length(cor_top_list) == 3) {
      if (k == 1) {
        i <- 1
        j <- 2
      } else if (k == 2) {
        i <- 1
        j <- 3
      } else if (k == 3) {
        i <- 2
        j <- 3
      }
    }

    unique_from <- unique(as.character(cor_top_list[[k]]$from))
    title_from <- unique_from
    value_from <- table(clusters[[i]]$col)[match(sub(paste0("^D", i), "", unique_from), names(table(clusters[[i]]$col)))]
    label_from <- paste(value_from, "var", sep = " ")
    shape_from <- shapes[i]
    color_from <- colors[i]

    unique_to <- unique(as.character(cor_top_list[[k]]$to))
    title_to <- unique_to
    value_to <- table(clusters[[j]]$col)[match(sub(paste0("^D", j), "", unique_to), names(table(clusters[[j]]$col)))]
    label_to <- paste(value_to, "var", sep = " ")
    shape_to <- shapes[j]
    color_to <- colors[j]

    nodes <- rbind(nodes, data.frame(id = c(unique_from, unique_to),
                                     label = c(label_from, label_to),
                                     value = c(value_from, value_to),
                                     shape = c(rep(shape_from, length(unique_from)), rep(shape_to, length(unique_to))),
                                     title = c(title_from, title_to),
                                     color = c(rep(color_from, length(unique_from)), rep(color_to, length(unique_to))),
                                     shadow = TRUE))
  }
  nodes <- unique(nodes)

  return(list(Top_cor_Prot_metab = cor_top, #reemplazo Top_cor_Prot_metab y a filtered_cor_Prot_metab_list
              Cor_list = cor_list,
              Correlation_Plot = cor_listPlot, #reemplazo cor_Prot_metab_WGCNA. Debo hacer plot for each correlation
              nodes = nodes,
              edges = edges,
              n = n))
}


