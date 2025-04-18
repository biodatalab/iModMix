% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_Assigment_genes_enrichr.R
\name{Assigment_genes_enrichr}
\alias{Assigment_genes_enrichr}
\title{Assigment_genes_enrichr}
\usage{
Assigment_genes_enrichr(
  cluster_assignments_ProtGenes,
  database = "GO_Biological_Process_2023"
)
}
\arguments{
\item{cluster_assignments_ProtGenes}{(data frame containing cluster_assignments and HMDB/Symbol)}

\item{database}{A list with all the available databases from enrichr}
}
\value{
result_list data frame containing cluster_assignments and Enrichr terms
}
\description{
Enrichr terms for genes (Proteins and transcriptomics)
}
