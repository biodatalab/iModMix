#' module1 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#'

#mod_module1_ui <- function(id, input, output, session) {
mod_module1_ui <- function(id) {
  ns <- NS(id)
  library(plotly)
  library(visNetwork)
  library(shinyWidgets)
  library(shinyBS)

  tagList(

    sidebarPanel(
      width = 4,

      fileInput(
        ns("DataSet"),
        accept = c('text/csv',
                   'text/comma-separated-values',
                   'text/plain',
                   '.csv'),
        label = h5("Upload Metabolomics Abundance Data")
      ),

      fileInput(
        ns("DataSet3"),
        accept = c('text/csv',
                   'text/comma-separated-values',
                   'text/plain',
                   '.csv'),
        label = h5("Upload Metabolomics Annotation Data")
      ),

      div(style = "border-top: 1px solid #ccc; margin-top: 10px; margin-bottom: 10px;"),

      fileInput(
        ns("DataSet2"),
        accept = c('text/csv',
                   'text/comma-separated-values',
                   'text/plain',
                   '.csv'),
        label = h5("Upload Proteomics/Genomics Expression Data")
      ),

      fileInput(
        ns("DataSet4"),
        accept = c('text/csv',
                   'text/comma-separated-values',
                   'text/plain',
                   '.csv'),
        label = h5("Upload Proteomics/Genomics Annotation data")
      ),

      div(style = "border-top: 1px solid #ccc; margin-top: 10px; margin-bottom: 10px;"),

      fileInput(
        ns("metadata"),
        accept = c('text/csv',
                   'text/comma-separated-values',
                   'text/plain',
                   '.csv'),
        label = h5("Upload Metadata")
      ),

      div(style = "border-top: 1px solid #ccc; margin-top: 10px; margin-bottom: 10px;"),

      helpText(
        "Example data is available to help you get started with iModMix. You can use this data to run the application and explore its features."),

      actionButton(
        ns("runDemo"), "Run with example datasets", icon = icon("play")
      )
    ),

    mainPanel(
      tabsetPanel(
        type = "tabs",
        tabPanel("Metabolomics",
                 tabsetPanel(
                   type = "tabs",
                   tabPanel("Data Input",
                            h4("Metabolomics abundance matrix",
                               bsButton("surf-infoMAM", label = "", icon = icon("info", lib = "font-awesome"), style = "default", size = "extra-small")),
                            bsPopover(id = "surf-infoMAM", title = "More information",
                                      content = HTML(paste0("Table reflecting the uploaded file “Metabolomics Abundance Data”. Check if the number of samples and the number of metabolomic features are correct. The arrows to the right of each column title can be used for sorting data from increasing or decreasing values. The search bar can also be used to confirm the details of a metabolomic feature of interest. ")),
                                      placement = "right", trigger = "hover", options = list(container = "body") ),
                            DT::DTOutput(ns("infotable")),
                            DT::DTOutput(ns("table")),
                            h4("Principal component analysis for each phenotype",
                               bsButton("surf-infoMPCAPh", label = "", icon = icon("info", lib = "font-awesome"), style = "default", size = "extra-small")),
                            bsPopover(id = "surf-infoMPCAPh", title = "More information",
                                      content = HTML(paste0("Drop-down menu displays conditions or sample descriptions provided with uploaded “Metadata”. Graph below displays Principal Component Analysis (PCA) plots representing each of your phenotype descriptions.")),
                                      placement = "right", trigger = "hover", options = list(container = "body") ),
                            selectInput(ns("phenotypeSelectorPCA"),
                                        label = "Select the phenotype of interest",
                                        choices = NULL,
                                        selected = NULL),
                            plotOutput(ns("PCA1")),
                            downloadButton(ns("downloadPCA"),
                                           "Principal component analysis"),
                            h4("Metabolomics annotation data",
                               bsButton("surf-infoMAD", label = "", icon = icon("info", lib = "font-awesome"), style = "default", size = "extra-small")),
                            bsPopover(id = "surf-infoMAD", title = "More information",
                                      content = HTML(paste0("Table reflecting the uploaded file “Metabolomics Annotation Data”. Check if the total number of entries at bottom of table matches the total number of features in the “Metabolomics Abundance Data”. The arrows to the right of each column title can be used for sorting data from increasing or decreasing values. The search bar can also be used to confirm the details of a metabolomic feature of interest.")),
                                      placement = "right", trigger = "hover", options = list(container = "body")
                               ),
                            DT::DTOutput(ns("table3"))
                   ),
                   tabPanel("Module Assignments",
                            h4("Sparse partial correlations: Metabolites", bsButton("surfInfoMPC", label = "", icon = icon("info", lib = "font-awesome"), style = "default", size = "extra-small")),
                            bsPopover(id = "surfInfoMPC", title = "More information",
                              content = HTML("<p>Partial correlation is a method of analyzing the relationship between two variables when other variables are present. Graphical Lasso (Glasso) is used to estimated the partial correlation and captures only direct associations.</p> <p>Below is a preview of the sparse partial correlation of the first five metabolomic features. The full .csv file of sparse partial correlation calculations for all the metabolomic features can be downloaded at the bottom of the table.</p>"),
                              placement = "right", trigger = "hover", options = list(container = "body")),
                            verbatimTextOutput(ns("matrizTable")),
                            downloadButton(ns("downloadParCor"),
                                           "Partial correlation matrix"),
                            h4("Hierarchical clustering", bsButton("surfInfoMHC", label = "", icon = icon("info", lib = "font-awesome"), style = "default", size = "extra-small")),
                            bsPopover(id = "surfInfoMHC", title = "More information",
                                      content = HTML("<p>Hierarchical clustering is used to identify common neighbors between the metabolomic features. Calculations are determined using the topographical overlap matrix (TOM) and based on the sparse partial correlations. Hierarchical clustering is visualized as a dendrogram.</p> <p> Axes: The vertical axis (y-axis) represents the dissimilarity between genes or modules, while the horizontal axis (x-axis) shows the genes or modules. Branches: Each line in the dendrogram represents a gene or module. Genes that are closer in the hierarchy (i.e., joined at a lower height in the dendrogram) have more similar expression profiles.</p>"),
                                      placement = "right", trigger = "hover", options = list(container = "body")),
                            plotOutput(ns("hc_plot")),
                            downloadButton(ns("downloadhc_plot"),
                                           "Hierarchical clustering"),
                            h4("Cluster assignments",
                               bsButton("surf-infoMAC", label = "", icon = icon("info", lib = "font-awesome"), style = "default", size = "extra-small")),
                            bsPopover(id = "surf-infoMAC", title = "More information",
                                      content = HTML(paste0("<p> Hierarchical clustering generates multiple clusters (modules) to which each metabolomic feature is assigned. The table below details the following columns: </p> <ul> <li>  Feature: metabolite ID </li> <li> Cluster: The module where the metabolite is assigned </li> <li> Col:  the color used on the hierarchical clustering dendrogram </li> <li> If annotation data is available, it also shows the KEGG ID and the metabolite name </li></ul> <p>The arrows to the right of each column title can be used for sorting data from increasing or decreasing values. The search bar can also be used to find the details of a metabolomic feature of interest. The full .csv file of Cluster Assignments for all the metabolomic features can be downloaded at the bottom of the table. </p>")),
                                      placement = "right", trigger = "hover", options = list(container = "body")),
                            DT::DTOutput(ns("tableClusterAssig")),
                            downloadButton(ns("downloadClusterAssig"),
                                           "Cluster assigment table"),
                            h4("First principal component from each module (Eigenfeatures)",
                               bsButton("surf-infoMEF", label = "", icon = icon("info", lib = "font-awesome"), style = "default", size = "extra-small")),
                            bsPopover(id = "surf-infoMEF", title = "More information",
                                      content = HTML(paste0("<p>The first principal component (PC1) is calculated for each module, referred to as an eigenfeature. Eigenfeatures are useful for: </p> <ol> <li>  Relating the modules to the phenotypes. </li> <li> Obtaining the correlation between omics datasets (integration). </li> </ol> <p> The full .csv file of calculations for PC1 for metabolomics modules can be downloaded at the bottom of the table. </p> <p> The heatmap below shows eigenfeatures across samples. The vertical axis (y-axis) represents the eigenfeatures, and the horizontal axis (x-axis) displays the sample conditions. </p>")),
                                      placement = "right", trigger = "hover", options = list(container = "body")),
                            DT::DTOutput(ns("tableEigengene")),
                            downloadButton(ns("downloadtableEigengene"),
                                           "Eigenfeatures table"),
                            h4("Eigenfeatures heatmap"),
                            plotOutput(ns("heatmapEigenMetab")),
                            downloadButton(ns("downloadheatmapEigenMetab"),
                                           "Eigenfeatures heatmap")
                   ),
                   tabPanel("Phenotype",
                            h4("Phenotype data",
                               bsButton("surf-infoMPD", label = "", icon = icon("info", lib = "font-awesome"), style = "default", size = "extra-small")),
                            bsPopover(id = "surf-infoMPD", title = "More information",
                                      content = HTML(paste0("Table reflecting the uploaded file “Metadata”. Check if the number of samples and the number of entries listed at the bottom of the table are the same. The arrows to the right of each column title can be used for sorting data from increasing or decreasing values. The search bar can also be used to confirm the details of a sample.")),
                                      placement = "right", trigger = "hover", options = list(container = "body")),
                            helpText(
                              "Note: Upload the metadata data to be able to run phenotype analysis."),
                            DT::DTOutput(ns("table5")),
                            h4("Classification between phenotypes by eigenfeatures",
<<<<<<< HEAD
                               bsButton("surf-infoMCPE", label = "", icon = icon("info", lib = "font-awesome"), style = "default", size = "extra-small")),
                            bsPopover(id = "surf-infoMCPE", title = "More information",
                                      content = HTML("<p>Statistical analysis by Student's t-test compares phenotypes chosen from a drop-down menu. The eigenfeatures of each module, determined previously, are used as predictors. The user can also specify a significance threshold for the p-value, with the default set to 0.05. </p> <p> It returns a data frame with the following columns: </p> <ul> <li> <b> Variable</b>: Represents the ID of the module. </li> <li> <b>Class</b>: Lists the two levels of the phenotypes being compared. If there are more than two levels, it compares one level against the others. </li>  <li> <b>Result_t </b>: The t-statistic value. </li> <li> <b> Result_pValue </b>: The p-value for the test. </li> </ul> <p> Boxplots are automatically generated at the bottom for significant eigenfeatures, with dots marking outliers and a legend describing the compared phenotype. </p> "),
=======
                               bsButton("surf-infoMCPEf", label = "", icon = icon("info", lib = "font-awesome"), style = "default", size = "extra-small")),
                            bsPopover(id = "surf-infoMCPEf", title = "More information",
                                      content = HTML(paste0("<p>Statistical analysis by Student's t-test compares phenotypes chosen from a drop-down menu. The eigenfeatures of each module, determined previously, are used as predictors. The user can also specify a significance threshold for the p-value, with the default set to 0.05. </p> <p> It returns a data frame with the following columns: </p> <ul> <li> Variable: Represents the ID of the module. </li> <li> Class: Lists the two levels of the phenotypes being compared. If there are more than two levels, it compares one level against the others. </li>  <li> Result_t: The t-statistic value. </li> <li> Result_pValue: The p-value for the test. </li> </ul> <p> Boxplots are automatically generated at the bottom for significant eigenfeatures, with dots marking outliers and a legend describing the compared phenotype. </p> ")),
>>>>>>> 1ff8cd465d1e0c5ac844b4d3e8322e974f98a162
                                      placement = "right", trigger = "hover", options = list(container = "body")),
                            fluidRow(
                              column(6,
                            selectInput(ns("phenotypeSelector"),
                                        label = "Select the phenotype of interest",
                                        choices = NULL,
                                        selected = NULL)
                              ),
                            column(6,
                              numericInput(ns("pValueThreshold"),
                              label = "Select p-value Threshold",
                              min = 0,
                              max = 1,
                              step = 0.0001,
                              value = 0.05)
                            )
                            ),
                            DT::DTOutput(ns("classification_results")),
                            plotOutput(ns("classification_plot_1_all")),
                            column(6,
                                   downloadButton(ns("downloadClassification_results"),
                                                  "Classification table")
                            ),
                            column(6,
                                   downloadButton(ns("downloadClassification_plot_1_all"),
                                                  "Boxplot classification")
                            ),
                            h4("Module screening",
                               bsButton("surf-infoMMS", label = "", icon = icon("info", lib = "font-awesome"), style = "default", size = "extra-small")),
                            bsPopover(id = "surf-infoMMS", title = "More information",
                                      content = HTML(paste0("The drop-down menu displays all modules generated by iModMix. Users can view the features within a selected module. If annotation data is available, it also shows the KEGG ID and metabolite name. PCA loading and heatmap plots are generated to visualize the behavior of each specific module across the phenotype.")),
                                      placement = "right", trigger = "hover", options = list(container = "body")),
                            selectInput(ns("moduleSelector"),
                                        label = "Select the module of interest",
                                        choices = NULL,
                                        selected = NULL),
                            DT::DTOutput(ns("ModuleFeaturesAnnot")),
                            downloadButton(ns("downloadModuleFeaturesAnnot"),
                                           "Module Features"),
                            h4("PCA loading by module"),
                            plotOutput(ns("Loadings1")),
                            downloadButton(ns("downloadLoadings1"),
                                           "PCA_Loadings"),
                            h4("Heatmap by module"),
                            plotOutput(ns("heatmap1")),
                            downloadButton(ns("downloadHeatmap1"),
                                           "Heatmap")
                   )
                 )
        ),

        tabPanel("Proteomics/Genomics",
                 tabsetPanel(
                   type = "tabs",
                   tabPanel("Data Input",
                            h4("Proteomics/Genomics expression matrix",
                               bsButton("surf-info_PGEM", label = "", icon = icon("info", lib = "font-awesome"), style = "default", size = "extra-small")),
                            bsPopover(id = "surf-info_PGEM", title = "More information",
                                      content = HTML(paste0("Table reflecting uploaded file <i>Proteomics/Genomics Expression data</i>. Check if the number of samples and the number of protein/genes are correct. The arrows to the right of each column title can be used for sorting data from increasing or decreasing values. The search bar can also be used to confirm the details of a protein/gene of interest. ")),
                                      placement = "right", trigger = "hover", options = list(container = "body") ),
                            DT::DTOutput(ns("infotable2")),
                            DT::DTOutput(ns("table2")),
                            h4("Principal component analysis for each phenotype"),
                            selectInput(ns("phenotypeSelectorPCA2"),
                                        label = "Select the phenotype of interest",
                                        choices = NULL,
                                        selected = NULL),
                            plotOutput(ns("PCA2")),
                            downloadButton(ns("downloadPCA2"),
                                           "Principal component analysis"),
                            h4("Proteomics/Genomics annotation data",
                               bsButton("surf-info_PGAD", label = "", icon = icon("info", lib = "font-awesome"), style = "default", size = "extra-small")),
                            bsPopover(id = "surf-info_PGAD", title = "More information",
                                      content = HTML(paste0("Table reflecting the uploaded file <i>Proteomics/Genomics Annotation Data</i>. Check if the total number of entries at bottom of table matches the total number of features in the <i>Proteomics/Genomics Expression Data</i>. The arrows to the right of each column title can be used for sorting data from increasing or decreasing values. The search bar can also be used to confirm the details of a protein/gene of interest. ")),
                                      placement = "right", trigger = "hover", options = list(container = "body")


                               ),
                            DT::DTOutput(ns("table4"))
                   ),
                   tabPanel("Module Assignments",
                            h4("Sparse partial correlations: Proteins/Genes",
                               bsButton("surf-info", label = "", icon = icon("info", lib = "font-awesome"), style = "default", size = "extra-small")),
                            bsPopover(id = "surf-info", title = "More information",
                                      content = HTML(paste0("<p>Partial correlation is a method of analyzing the relationship between two variables when other variables are present. Graphical Lasso (Glasso) is used to estimated the partial correlation and capture only direct associations.  </p> <p> Preview of the sparse partial correlation of the first five proteins/genes. The full .csv file of sparse partial correlation calculations for all the proteins/genes can be downloaded at the bottom of the table. </p>")),
                                      placement = "right", trigger = "hover", options = list(container = "body")


                               ),
                            verbatimTextOutput(ns("matrizTable2")),
                            downloadButton(ns("downloadParCor2"),
                                           "Partial correlation"),
                            h4("Hierarchical clustering"),
                            plotOutput(ns("hc_plot2")),
                            downloadButton(ns("downloadhc_plot2"),
                                           "Hierarchical clustering"),
                            h4("Cluster assignments",
                               bsButton("surf-info_PGCA", label = "", icon = icon("info", lib = "font-awesome"), style = "default", size = "extra-small")),
                            bsPopover(id = "surf-info_PGCA", title = "More information",
                                      content = HTML(paste0("<p>Hierarchical clustering generates multiple clusters (modules) to which each proteins/genes are assigned. The table below details the following columns:  </p> <ul> <li> Feature: proteins/genes ID </li>  <li>  Cluster: The module where the protein/gene is assigned </li>  <li> Col:  the color used on the hierarchical clustering dendrogram </li> <li> Gene_symbol: If annotation data is available, it also shows the gene symbol. </li> </ul> <p> The arrows to the right of each column title can be used for sorting data from increasing or decreasing values. The search bar can also be used to find the details of a protein/gene of interest.</p> <p> The full .csv file of Cluster Assignments for all the proteins/genes features can be downloaded at the bottom of the table. </p>")),
                                      placement = "right", trigger = "hover", options = list(container = "body")



                               ),
                            DT::DTOutput(ns("tableClusterAssig4")),
                            downloadButton(ns("downloadClusterAssig2"),
                                           "Cluster assigment"),
                            h4("Cluster assignments enriched",
                               bsButton("surf-info_PGCAE", label = "", icon = icon("info", lib = "font-awesome"), style = "default", size = "extra-small")),
                            bsPopover(id = "surf-info_PGCAE", title = "More information",
                                      content = HTML(paste0("Drop-down menu displays available libraries for pathway analysis. Choose a library to automatically amend the Proteomics/Genomics cluster descriptions on table below. Under column enriched_Term the most highly correlated pathway is displayed and in the following columns, along with enriched_Genes, and p-values as determined by Enrichr. The search bar can also be used to find the details of a protein/gene or module of interest. The full .csv file of Cluster Enrichments for Proteomics/Genomics can be downloaded at the bottom of the table.")),
                                      placement = "right", trigger = "hover", options = list(container = "body")


                               ),
                            helpText(
                              "Note: Upload the Annotation data to be able to run enrichment analysis."),
                            selectInput(
                              ns("databaseSelector"),
                              label = "Select Library",
                              choices = NULL),
                            DT::DTOutput(ns("tableClusterAssig3")),
                            downloadButton(ns("downloadEnrichment"),
                                           "Enrichment analysis"),
                            h4("First principal component from each module (Eigenfeatures)",
                               bsButton("surf-info_PGPC1ef", label = "", icon = icon("info", lib = "font-awesome"), style = "default", size = "extra-small")),
                            bsPopover(id = "surf-info_PGPC1ef", title = "More information",
                                      content = HTML(paste0("<p> The first principal component (PC1) is calculated for each module, referred to as eigenfeatures. Eigenfeatures are useful for: </p> <ol> <li> Relating the modules to the phenotypes. </li> <li>  Obtaining the correlation between omics datasets (integration). </li> </ol> <p> The full .csv file of calculations for PC1 for proteomics/genomics modules can be downloaded at the bottom of the table. </p> <p> The heatmap below shows eigenfeatures across samples. The vertical axis (y-axis) represents the eigenfeatures, and the horizontal axis (x-axis) displays the sample conditions. </p>")),
                                      placement = "right", trigger = "hover", options = list(container = "body")


                               ),
                            DT::DTOutput(ns("tableEigengene2")),
                            downloadButton(ns("downloadtableEigengene2"),
                                           "Eigenfeatures table"),
                            h4("Eigenfeatures heatmap"),
                            plotOutput(ns("heatmapEigenProt")),
                            downloadButton(ns("downloadheatmapEigenProt"),
                                           "Eigenfeatures heatmap")
                   ),
                   tabPanel("Phenotype",
                            h4("Phenotype data",
                               bsButton("surf-info_PGPData", label = "", icon = icon("info", lib = "font-awesome"), style = "default", size = "extra-small")),
                            bsPopover(id = "surf-info_PGPData", title = "More information",
                                      content = HTML(paste0("<p> Statistical analysis by Student's t-test compares phenotypes chosen from a drop-down menu. The eigenfeatures of each module, determined previously, are used as predictors. The user can also specify a significance threshold for the p-value, with the default set to 0.05. </p>  <p> It returns a data frame with the following columns: </p> <ul> <li>  <b> Variable<b>: Represents the ID of the module. </li> <li> <b>Class</b>: Lists the two levels of the phenotypes being compared. If there are more than two levels, it compares one level against the others. </li> <li> <b> Result_t</b>: The t-statistic value. </li> </li> <b>Result_pValue</b>: The p-value for the test.</li> </ul> <p> Boxplots are automatically generated at the bottom for significant eigenfeatures, with dots marking outliers and a legend describing the compared phenotype. </p>")),
                                      placement = "right", trigger = "hover", options = list(container = "body")

                            ),
                            helpText(
                              "Note: Upload the metadata data to be able to run phenotype analysis."),
                            DT::DTOutput(ns("table6")),
                            h4("Classification between phenotypes by eigenfeatures",
                               bsButton("surf-info_PGCPef", label = "", icon = icon("info", lib = "font-awesome"), style = "default", size = "extra-small")),
                            bsPopover(id = "surf-info_PGCPef", title = "More information",
                                      content = HTML(paste0("<p>Statistical analysis by Student's t-test compares phenotypes chosen from a drop-down menu. The eigenfeatures of each module, determined previously, are used as predictors. The user can also specify a significance threshold for the p-value, with the default set to 0.05. </p> <p> It returns a data frame with the following columns: </p> <ul> <li> Variable: Represents the ID of the module. </li> <li> Class: Lists the two levels of the phenotypes being compared. If there are more than two levels, it compares one level against the others. </li> <li>Result_t: The t-statistic value. </li> <li> Result_pValue: The p-value for the test. </li> </uL> <p>Boxplots are automatically generated at the bottom for significant eigenfeatures, with dots marking outliers and a legend describing the compared phenotype.</p>")),
                                      placement = "right", trigger = "hover", options = list(container = "body")
                               ),
                            fluidRow(
                              column(6,
                                     selectInput(ns("phenotypeSelector2"),
                                                 label = "Select the phenotype of interest",
                                                 choices = NULL,
                                                 selected = NULL)
                              ),
                              column(6,
                                     numericInput(ns("pValueThreshold2"),
                                                 label = "Select p-value Threshold",
                                                 min = 0,
                                                 max = 1,
                                                 step = 0.0001,
                                                 value = 0.05)
                              )
                            ),
                            DT::DTOutput(ns("classification_results2")),
                            plotOutput(ns("classification_plot_2_all")),
                            fluidRow(
                              column(6,
                                     downloadButton(ns("downloadClassification_results2"),
                                                    "Classification table")
                              ),
                              column(6,
                                     downloadButton(ns("downloadClassification_plot_2_all"),
                                                    "Boxplot classification")
                              )
                            ),
                            h4("Module screening",
                               bsButton("surf-infoMS", label = "", icon = icon("info", lib = "font-awesome"), style = "default", size = "extra-small")),
                            bsPopover(id = "surf-infoMS", title = "More information",
                                      content = HTML(paste0("The drop-down menu displays all modules generated by iModMix. Users can view the features within a selected module. If annotation data is available, it also shows the Gene Symbol. PCA loading and heatmap plots are generated to visualize the behavior of each specific module across the phenotype.")),
                                      placement = "right", trigger = "hover", options = list(container = "body")

                            ),
                            selectInput(ns("moduleSelector2"),
                                        label = "Select the module of interest",
                                        choices = NULL,
                                        selected = NULL),
                            DT::DTOutput(ns("ModuleFeatures2Annot")),
                            downloadButton(ns("downloadModuleFeatures2Annot"),
                                           "Module Features"),
                            h4("PCA loading by module"),
                            plotOutput(ns("Loadings2")),
                            downloadButton(ns("downloadLoadings2"),
                                           "PCA_Loadings"),
                            h4("Heatmap by module"),
                            plotOutput(ns("heatmap2")),
                            downloadButton(ns("downloadHeatmap2"),
                                           "Heatmap")
                   )
                 )
                 ),

        tabPanel("Multi-omics Analysis",
                 tabsetPanel(
                   type = "tabs",
                   tabPanel("Modules correlation",
                            h4("Correlation: Metabolites and Proteins/Genes",
                               bsButton("surf-infoMC", label = "", icon = icon("info", lib = "font-awesome"), style = "default", size = "extra-small")),
                            bsPopover(id = "surf-infoMC", title = "More information",
                                      content = HTML(paste0("Histogram depicting the correlation between eigen-metabolites and eigen-genes using Spearman correlation")),
                                      placement = "right", trigger = "hover", options = list(container = "body")),
                            plotOutput(ns("Correlation_plot")),
                            numericInput(ns("pValueThreshold3"),
                                        label = "Select Correlation Threshold",
                                        min = 0,
                                        max = 1,
                                        step = 0.0001,
                                        value = 0.5),
                            DT::DTOutput(ns("tableCorrelation")),
                            downloadButton(ns("downloadOmicsCorrelation"),
                                           "Omics correlation"),
                            h4("Module Network of Metabolites and Proteins/Genes",
                               bsButton("surf-info_MMPG", label = "", icon = icon("info", lib = "font-awesome"), style = "default", size = "extra-small")),
                            bsPopover(id = "surf-info_MMPG", title = "More information",
                                      content = HTML(paste0("An interactive module network showing each proteins/gene module as a green triangle  and metabolite modules as a yellow diamond. Clicking directly on the triangle or diamond identifies the module number. Correlation coefficients are seen on arrows connecting modules. Modules can be fluidly switched into different order and moved on the screen. The network can be downloaded as an html for saving. ")),
                                      placement = "right", trigger = "hover", options = list(container = "body")),
                            #plotOutput(ns("Network_plot")),
                            visNetworkOutput(ns("network")),
                            downloadLink(ns("downloadNetwork"),
                                         "Network as .html")

                   ),
                   tabPanel("Important features",
                            h4("Top 5 Multi-omics modules correlations",
                               bsButton("surf-infoT5MM", label = "", icon = icon("info", lib = "font-awesome"), style = "default", size = "extra-small")),
                            bsPopover(id = "surf-infoT5MM", title = "More information",
                                      content = HTML(paste0("Table of the top 5 highly correlated modules, with the number of features within each module, the correlation between modules, and the enriched term for the proteins/genomics modules.   ")),
                                      placement = "right", trigger = "hover", options = list(container = "body")),
                            DT::DTOutput(ns("ImportantVariables")),
                            h4("Top module correlation details",
                               bsButton("surf-infoTMCD", label = "", icon = icon("info", lib = "font-awesome"), style = "default", size = "extra-small")),
                            bsPopover(id = "surf-infoTMCD", title = "More information",
                                      content = HTML(paste0("The drop-down menu displays the details for each of the top 5 highly correlated modules. Select one option to see the features within each module and the correlation between each feature (Corrplot and table). The arrows to the right of each column title can be used to sort data from increasing or decreasing values. Users can also use the search bar to find the details of a protein/gene or metabolites of interest. The user can download the full .csv file of Module correlations at the bottom of the table. ")),
                                      placement = "right", trigger = "hover", options = list(container = "body")),
                            selectInput(ns("visualization_list"),
                                        label = "Select Lists to Visualize",
                                        choices = c("Top_1" = 1,
                                                    "Top_2" = 2,
                                                    "Top_3" = 3,
                                                    "Top_4" = 4,
                                                    "Top_5" = 5)),
                            h4("List of Metabolites",
                               bsButton("surf-info_LofMet", label = "", icon = icon("info", lib = "font-awesome"), style = "default", size = "extra-small")),
                            bsPopover(id = "surf-info_LofMet", title = "More information",
                                      content = HTML(paste0("<p> Displays the list of metabolites within a metabolomic module that is highly correlated with a protein module. The metabolomic module ID is specified first, followed by the list of constituent metabolites. This information facilitates further pathway analysis and provides valuable insights into the relationships between metabolites and proteins. </p> <p> The full .csv file of the list of metabolites to perform pathway analysis further can be downloaded at the bottom of the table. </p>")),
                                      placement = "right", trigger = "hover", options = list(container = "body")

                            ),
                            verbatimTextOutput(ns("Important_features_2")),

                            h4("List of Proteins/Genes",
                               bsButton("surf-info_LofPG", label = "", icon = icon("info", lib = "font-awesome"), style = "default", size = "extra-small")),
                            bsPopover(id = "surf-info_LofPG", title = "More information",
                                      content = HTML(paste0("Displays the list of proteins/genes within a proteomic/genomic module that is highly correlated with a metabolomic module. The module ID is first specified, along with its enriched term, enriched genes, and p-value. This is followed by the list of constituent proteins/genes. This information enables further pathway analysis and provides valuable insights into the relationships between metabolites, proteins, and genes.")),
                                      placement = "right", trigger = "hover", options = list(container = "body")

                            ),
                            verbatimTextOutput(ns("Important_features_1")),

                            # h4("Modules correlation: Metabolites and Proteins/Genes"),
                            # plotOutput(ns("Correlation_plotImp")),

                            h4("Corrplot: Metabolites and Proteins/Genes"),
                            plotOutput(ns("CorplotImp")),

                            h4("Modules correlation: Metabolites and Proteins/Genes",
                               bsButton("surf-infoMCMPG", label = "", icon = icon("info", lib = "font-awesome"), style = "default", size = "extra-small")),
                            bsPopover(id = "surf-infoMCMPG", title = "More information",
                                      content = HTML(paste0("Displays the list of proteins/genes within a proteomic/genomic module that is highly correlated with a metabolomic module. The module ID is first specified, along with its enriched term, enriched genes, and p-value. This is followed by the list of constituent proteins/genes. This information enables further pathway analysis and provides valuable insights into the relationships between metabolites, proteins, and genes.")),
                                      placement = "right", trigger = "hover", options = list(container = "body")),
                            DT::DTOutput(ns("Correlation_mod")),
                            downloadButton(ns("downloadModCorrelation"),
                                           "Modules correlation"),

                            h4("Metabolites from top module",
                               bsButton("surf-infoMTopM", label = "", icon = icon("info", lib = "font-awesome"), style = "default", size = "extra-small")),
                            bsPopover(id = "surf-infoMTopM", title = "More information",
                                      content = HTML(paste0("<p> Displays the list of metabolites within a metabolomic module that is highly correlated with a protein module. The metabolomic module ID is specified first, followed by the list of constituent metabolites. This information facilitates further pathway analysis and provides valuable insights into the relationships between metabolites and proteins. </p> <p> The full .csv file of the list of metabolites to perform pathway analysis further can be downloaded at the bottom of the table. </p>")),
                                      placement = "right", trigger = "hover", options = list(container = "body")),
                            # DT::DTOutput(ns("cluster_assignments_2")),
                            DT::DTOutput(ns("cluster_assignments_summary2")),
                            DT::DTOutput(ns("cluster_assignments_features2")),
                            downloadButton(ns("downloadcluster_assignments_2"),
                                           "Metabolites_TopModule"),

                            h4("Proteins/Genes from top module"),
                            #DT::DTOutput(ns("cluster_assignments_1")),
                            DT::DTOutput(ns("cluster_assignments_summary")),
                            DT::DTOutput(ns("cluster_assignments_features")),
                            downloadButton(ns("downloadcluster_assignments_1"),
                                           "Proteins_Genes_TopModule")
                 )
          )
        )
      )
    )
  )
}

#' module1 Server Functions
#'
#' @noRd
mod_module1_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    filedata <- reactiveVal(NULL)
    filedata3 <- reactiveVal(NULL)
    filedata2 <- reactiveVal(NULL)
    filedata4 <- reactiveVal(NULL)
    metadata <- reactiveVal(NULL)
    demo_loaded <- reactiveVal(NULL)
    demo_loaded2 <- reactiveVal(NULL)
    enrich_loaded <- reactiveVal(NULL)
    classification_plot <- reactiveVal()
    classification_plot2 <- reactiveVal()

    observeEvent(input$DataSet, {
      req(input$DataSet)
      filedata_value <- read.csv(input$DataSet$datapath)
      filedata(filedata_value)
    })

    observeEvent(input$DataSet3, {
      req(input$DataSet3)
      filedata_value <- read.csv(input$DataSet3$datapath)
      filedata3(filedata_value)
    })

    observeEvent(input$DataSet2, {
      req(input$DataSet2)
      filedata_value <- read.csv(input$DataSet2$datapath)
      filedata2(filedata_value)
    })

    observeEvent(input$DataSet4, {
      req(input$DataSet4)
      filedata_value <- read.csv(input$DataSet4$datapath)
      filedata4(filedata_value)
    })

    observeEvent(input$metadata, {
      req(input$metadata)
      filedata_metadata <- read.csv(input$metadata$datapath)
      metadata(filedata_metadata)
    })

    observeEvent(input$runDemo, {
      withProgress(message = 'Loading example data...', value = 0, {
        incProgress(0, detail = 'Loading Metab_exp.csv')
        filedata(load_metab_exp())
        Sys.sleep(1)

        incProgress(10, detail = 'Loading Metab_annot.csv')
        filedata3(load_metab_annot())
        Sys.sleep(1)

        incProgress(20, detail = 'Loading Prot_exp.csv')
        filedata2(load_prot_exp())
        Sys.sleep(1)

        incProgress(30, detail = 'Loading Prot_annot.csv')
        filedata4(load_prot_annot())
        Sys.sleep(1)

        incProgress(50, detail = 'Loading Metadata.csv...')
        metadata(load_metadata())
        Sys.sleep(2)

        demo_loaded(TRUE)
        demo_loaded2(TRUE)
        enrich_loaded(TRUE)
        updateSelectInput(session, "databaseSelector", selected = "KEGG_2019_Mouse")
        updateSliderInput(session, "pValueThreshold3", value = 0.9172)

        incProgress(100, detail = 'Complete!')
      })
    })

    data_info <- reactive({
      req(filedata())
      Nobservations <- nrow(filedata())
      Ncells <- ncol(filedata())-1
      SummaryData <- as.data.frame(list(Number = c(Ncells, Nobservations)))
      rownames(SummaryData) <- c("Samples", "Features")
      list(SummaryData = SummaryData)
    })
    output$infotable <- DT::renderDataTable({
      df <- data_info()$SummaryData
      DT::datatable(df)
    })
    output$table <- DT::renderDataTable({
      df <- filedata()
      DT::datatable(df)
    })

    pca1 <- reactive({
      req(filedata())
      data = filedata()
      data$missing_count = rowSums(is.na(data))
      feature_mat = subset(data, missing_count <= 0.1 * (ncol(data)-2))
      features <- feature_mat[,1]
      feature_mat_t <- as.matrix(scale(t(feature_mat[,-c(1,ncol(feature_mat))])))
      colnames(feature_mat_t) <- features
      feature_mat_t_imp = impute::impute.knn(feature_mat_t, k = min(10, nrow(feature_mat_t)))
      feature_mat_t_imp_data= feature_mat_t_imp$data
      pca_res <- prcomp(feature_mat_t_imp_data)
      return(list(pca_res = pca_res))
    })

    output$PCA1 <- renderPlot({
      library(ggfortify)
      requireNamespace("ggplot2", quietly = TRUE)
      requireNamespace("ggfortify", quietly = TRUE)
      if(is.null(metadata())){
      ggplot2::autoplot(pca1()$pca_res)
       } else {
        req(metadata())
        ggplot2::autoplot(pca1()$pca_res, data = metadata(), colour = input$phenotypeSelectorPCA)
      }
    })

    # Render the download handler
    output$downloadPCA <- downloadHandler(
      filename = function() {
        "PCA_Metabolites.png"
      },
      content = function(file) {
        if (is.null(metadata())) {
          p <- ggplot2::autoplot(pca1()$pca_res)
        } else {
          req(metadata())
          p <- ggplot2::autoplot(pca1()$pca_res, data = metadata(), colour = input$phenotypeSelectorPCA)
        }
        ggplot2::ggsave(file, plot = p, device = "png")
      }
    )

    data_info2 <- reactive({
      req(filedata2())
      Nobservations <- nrow(filedata2())
      Ncells <- ncol(filedata2())-1
      SummaryData <- as.data.frame(list(Number = c(Ncells, Nobservations)))
      rownames(SummaryData) <- c("Samples", "Features")
      list(SummaryData = SummaryData)
    })
    output$infotable2 <- DT::renderDataTable({
      df <- data_info2()$SummaryData
      DT::datatable(df)
    })

    output$table2 <- DT::renderDataTable({
      df <- filedata2()
      DT::datatable(df)
    })

    pca2 <- reactive({
      req(filedata2())
      data = filedata2()
      data$missing_count = rowSums(is.na(data))
      feature_mat = subset(data, missing_count <= 0.1 * (ncol(data)-2))
      features <- feature_mat[,1]
      feature_mat_t <- as.matrix(scale(t(feature_mat[,-c(1,ncol(feature_mat))])))
      colnames(feature_mat_t) <- features
      feature_mat_t_imp = impute::impute.knn(feature_mat_t, k = min(10, nrow(feature_mat_t)))
      feature_mat_t_imp_data= feature_mat_t_imp$data
      pca_res <- prcomp(feature_mat_t_imp_data)
      return(list(pca_res = pca_res))
    })

    output$PCA2 <- renderPlot({
      library(ggfortify)
      requireNamespace("ggplot2", quietly = TRUE)
      requireNamespace("ggfortify", quietly = TRUE)
      if(is.null(metadata())){
        ggplot2::autoplot(pca2()$pca_res)
      } else {
        ggplot2::autoplot(pca2()$pca_res, data = metadata(), colour = input$phenotypeSelectorPCA2)
      }
    })

    # Render the download handler
    output$downloadPCA2 <- downloadHandler(
      filename = function() {
        "PCA_Proteins/Genes.png"
      },
      content = function(file) {
        if (is.null(metadata())) {
          p <- ggplot2::autoplot(pca2()$pca_res)
        } else {
          req(metadata())
          p <- ggplot2::autoplot(pca2()$pca_res, data = metadata(), colour = input$phenotypeSelectorPCA2)
        }
        ggplot2::ggsave(file, plot = p, device = "png")
      }
    )

    output$table3 <- DT::renderDataTable({
      df <- filedata3()
      DT::datatable(df)
    })

    output$table4 <- DT::renderDataTable({
      df <- filedata4()
      DT::datatable(df)
    })

    output$table5 <- DT::renderDataTable({
      df <- metadata()
      DT::datatable(df)
    })

    pheno_variablesPCA <- reactive({
      names(metadata())[-which(names(metadata()) == "Sample")]
    })

    # For metabolites global PCA
    observe({
      updateSelectInput(session, "phenotypeSelectorPCA", choices = pheno_variablesPCA())
    })

    pheno_variables <- reactive({
      names(metadata())[-which(names(metadata()) == "Sample")]
    })

    observe({
      updateSelectInput(session, "phenotypeSelector", choices = pheno_variables())
    })

    pheno_variables2 <- reactive({
      names(metadata())[-which(names(metadata()) == "Sample")]
    })

    observe({
      updateSelectInput(session, "phenotypeSelector2", choices = pheno_variables2())
    })

    partial_cors1 <- reactive({
      withProgress(message = 'Calculating partial correlations (Metabolites)...', value = 0, {
        if (is.null(demo_loaded())) {
          req(filedata())
          Expression_mat <- filedata()
          Sys.sleep(1)  #
          par_cor1 <- partial_cors(Expression_mat = Expression_mat)
        } else {
          Sys.sleep(1)
          par_cor1 <- load_partial_cor_metab()
        }
        incProgress(100, detail = 'Complete!')
        list(par_cor1 = par_cor1)
      })
    })

    output$matrizTable <- renderPrint({
      partial_cors1()$par_cor1[1:5,1:5]
    })

    # Render the download handler
    output$downloadParCor <- downloadHandler(
      filename = function() {
        "PartialCorMetabolites.csv"
      },
      content = function(file) {
        write.csv(partial_cors1()$par_cor1, file, row.names = TRUE)
      }
    )

    hierarchical_cluster1 <- reactive({
      par_cor2 = partial_cors1()$par_cor1
      hc2 = hierarchical_cluster(parcor_mat = par_cor2, tom = TRUE, min_module_size = 10)
      hclusterTree2 = hc2$hclustTree
      hcDynMods2 = hc2$dynamicMods_numeric
      hcCluster_assignments2 = hc2$cluster_assignments
      return(list(hclusterTree2 = hclusterTree2, hcDynMods2 = hcDynMods2, hcCluster_assignments2 = hcCluster_assignments2 ))
    })

    unique_variables <- reactive({
      unique(hierarchical_cluster1()$hcCluster_assignments2$col)
    })

    observe({
      updateSelectInput(session, "moduleSelector", choices = unique_variables())
    })

    unique_variables2 <- reactive({
      unique(hierarchical_cluster2()$hcCluster_assignments$col)
    })

    observe({
      updateSelectInput(session, "moduleSelector2", choices = unique_variables2())
    })

    output$hc_plot <- renderPlot({
      hcClu = hierarchical_cluster1()$hclusterTree2
      hcMod = hierarchical_cluster1()$hcDynMods2
      WGCNA::plotDendroAndColors(dendro = hcClu,
                                 colors = hcMod,
                                 dendroLabels = FALSE,
                                 hang = 0.03,
                                 addGuide = TRUE,
                                 guideHang = 0.05,
                                 groupLabels = "Modules",
                                 main = "Feature dendrogram and module assignments")
    })

    # Render the download handler
    output$downloadhc_plot <- downloadHandler(
      filename = function() {
        "Hierarchical_clusterMetabolites.png"
      },
      content = function(file) {
        png(file)
        hcClu = hierarchical_cluster1()$hclusterTree2
        hcMod = hierarchical_cluster1()$hcDynMods2
        WGCNA::plotDendroAndColors(dendro = hcClu,
                                   colors = hcMod,
                                   dendroLabels = FALSE,
                                   hang = 0.03,
                                   addGuide = TRUE,
                                   guideHang = 0.05,
                                   groupLabels = "Modules",
                                   main = "Feature dendrogram and module assignments")
        dev.off()
      }
    )

    cluster_assignments_metabolites1 <- reactive({
      cluster_metabolites = as.data.frame(hierarchical_cluster1()$hcCluster_assignments2)
      if (is.null(filedata3())) {
        cluster_assignments_metab <- cluster_assignments_metabolites(cluster_metabolites = cluster_metabolites, metab_annotation = NULL)
      } else {
        cluster_assignments_metab <- cluster_assignments_metabolites(cluster_metabolites = cluster_metabolites, metab_annotation = filedata3())
      }
      return(list(cluster_assignments_metab = cluster_assignments_metab))
    })

    output$tableClusterAssig <- DT::renderDataTable({
      df1 = cluster_assignments_metabolites1()$cluster_assignments_metab
      DT::datatable(df1)
    })

    # Render the download handler
    output$downloadClusterAssig <- downloadHandler(
      filename = function() {
        "ClusterAssigMetabolites.csv"
      },
      content = function(file) {
        write.csv(cluster_assignments_metabolites1()$cluster_assignments_metab, file, row.names = TRUE)
      }
    )

    Eigengene1 <- reactive({
      req(filedata())
      Expression_mat = filedata()
      Cluster_assignments = hierarchical_cluster1()$hcCluster_assignments2[,3]
      Eigengenes = Eigengenes(Expression_mat = Expression_mat, cluster_assignments = Cluster_assignments)$module_eigenmetab_Me
      return(list(Eigengenes = Eigengenes))
    })

    output$tableEigengene <- DT::renderDataTable({
      df2 = as.data.frame(Eigengene1()$Eigengenes)
      DT::datatable(df2)
    })

    # Render the download handler
    output$downloadtableEigengene <- downloadHandler(
      filename = function() {
        "EigenfeaturesMetabolites.csv"
      },
      content = function(file) {
        write.csv(Eigengene1()$Eigengenes, file, row.names = TRUE)
      }
    )

    output$heatmapEigenMetab <- renderPlot({
      metab_heatmap_plot = ComplexHeatmap::Heatmap(
        as.data.frame(t(Eigengene1()$Eigengenes)), cluster_columns = FALSE, cluster_rows = TRUE,
        row_title = "Eigenfeatures", column_title = "Samples", name = "Z-score",
        heatmap_legend_param = list(title_position = "topleft", legend_direction = "vertical"),
        show_row_names = TRUE, row_names_side = "left", row_names_gp = grid::gpar(fontsize = 8),
        show_column_names = TRUE
      )

      ComplexHeatmap::draw(metab_heatmap_plot, heatmap_legend_side = "right",
                           annotation_legend_side = "left", padding = ggplot2::unit(c(2, 3, 2, 40), "mm"))
    })

    output$downloadheatmapEigenMetab <- downloadHandler(
      filename = function() {
        "HeatmapEigenMetabolites.png"
      },
      content = function(file) {
        png(file)
        metab_heatmap_plot = ComplexHeatmap::Heatmap(
          as.data.frame(t(Eigengene1()$Eigengenes)), cluster_columns = FALSE, cluster_rows = TRUE,
          row_title = "Eigenfeatures", column_title = "Samples", name = "Z-score",
          heatmap_legend_param = list(title_position = "topleft", legend_direction = "vertical"),
          show_row_names = TRUE, row_names_side = "left", row_names_gp = grid::gpar(fontsize = 8),
          show_column_names = TRUE
        )
        ComplexHeatmap::draw(metab_heatmap_plot, heatmap_legend_side = "right",
                             annotation_legend_side = "left", padding = ggplot2::unit(c(2, 3, 2, 40), "mm"))
        dev.off()
      }
    )

    Classification_Metabolites <- reactive({
      eigengenes_metab = as.data.frame(Eigengene1()$Eigengenes)
      metadata <- as.data.frame(metadata())
      phenotype_variable = input$phenotypeSelector
      significance_threshold = input$pValueThreshold
      classification_metabolite <- perform_classification( eigengene_data = eigengenes_metab,
                                                           metadata = metadata,
                                                           phenotype_variable = phenotype_variable,
                                                           significance_threshold = significance_threshold)
      return(list(
        result = classification_metabolite$result,
        plots = classification_metabolite$plots))
    })


    output$classification_results <- DT::renderDataTable({
      Classification_Metabolites()$result
    })

    # Render the download handler
    output$downloadClassification_results <- downloadHandler(
      filename = function() {
        "ClassByEigenfeatures_Metabolites.csv"
      },
      content = function(file) {
        write.csv(Classification_Metabolites()$result, file, row.names = TRUE)
      }
    )

    output$classification_plot_1_all <- renderPlot({
      selected_variable <- input$phenotypeSelector
      levels_selected_variable <- unique(metadata()[[selected_variable]])
      if (length(levels_selected_variable) < 3) {
        class_names <- levels_selected_variable
        class_label <- paste(class_names, collapse = " vs ")
        plot <- Classification_Metabolites()$plots[[1]]
        plot <- plot +
          ggplot2::labs(title = class_label, fill = as.factor(levels_selected_variable),
               x = "Variables",
               y = "Class") +
          ggplot2::theme(
            axis.text.x = ggplot2::element_text(angle = 90, hjust = 1)
          )
        classification_plot(plot)
        return(plot)
      } else {
        # Print multiple boxplot charts, one for each level of the selected variable
        plots_list <- lapply(1:length(levels_selected_variable), function(i) {
          Classification_Metabolites()$plots[[i]]
        })
        plot <- cowplot::plot_grid(plotlist = plots_list)
        classification_plot(plot)  # Store the plot in the reactive variable
        return(plot)
      }
    })

    # Render the download handler
    output$downloadClassification_plot_1_all <- downloadHandler(
      filename = function() {
        "Boxplot_classMetabolites.png"
      },
      content = function(file) {
        ggplot2::ggsave(file, plot = classification_plot(), device = "png")
      }
    )

    loadings_metab <- reactive({
      req(filedata())
      data = filedata()
      selected_variable <- input$phenotypeSelector
      data$missing_count = rowSums(is.na(data))
      feature_mat = subset(data, missing_count <= 0.1 * (ncol(data)-2))
      features <- feature_mat[,1]
      feature_mat_t <- as.matrix(scale(t(feature_mat[,-c(1,ncol(feature_mat))])))
      colnames(feature_mat_t) <- features
      feature_mat_t_imp = impute::impute.knn(feature_mat_t, k = min(10, nrow(feature_mat_t)))
      feature_mat_t_imp_data= feature_mat_t_imp$data
      cluster_Metab <- subset(hierarchical_cluster1()$hcCluster_assignments2, col == input$moduleSelector)
      #cluster_Metab <- subset(cluster_assignments_metabolites1()$cluster_assignments_metab, cluster == "cluster_000011")
      cluster_variables_Metab <- cluster_Metab$feature
      cluster_variables_MetabKEGG <- cluster_variables_Metab
      cluster_expression_matrix_Metab <- feature_mat_t_imp_data[, colnames(feature_mat_t_imp_data) %in% cluster_variables_Metab, drop = FALSE]
      combined_data <- merge(metadata()[,c("Sample", selected_variable)], cluster_expression_matrix_Metab, by.x = "Sample", by.y = "row.names", all.x = TRUE)
      heatmap_data_sub_order <- combined_data[order(combined_data[[selected_variable]]), ]
      data_heat= t(as.matrix(heatmap_data_sub_order[ , 3:ncol(heatmap_data_sub_order)]))
      pca_res <- prcomp(cluster_expression_matrix_Metab)
      return(list(pca_res = pca_res, data_heat= data_heat, heatmap_data_sub_order = heatmap_data_sub_order, cluster_variables_MetabKEGG = cluster_variables_MetabKEGG))
    })

    output$ModuleFeaturesAnnot <- DT::renderDataTable({
      req(loadings_metab())
      df2 = as.data.frame(loadings_metab()$cluster_variables_MetabKEGG)
      names(df2) = "Feature_ID"
      if (!is.null(filedata3())) {
        AnnoMeta = as.data.frame(filedata3())
        df2 <- merge(df2, AnnoMeta[, c("Feature_ID", "KEGG", "Metabolite")], by = "Feature_ID", all.x = TRUE)
      }
      DT::datatable(df2, rownames = FALSE)
    })

    # Render the download handler
    output$downloadModuleFeaturesAnnot <- downloadHandler(
      filename = function() {
        "FeaturesOnMetabolomicsModule.csv"
      },
      content = function(file) {
        df2 = as.data.frame(loadings_metab()$cluster_variables_MetabKEGG)
        names(df2) = "Feature_ID"
        if (!is.null(filedata3())) {
          AnnoMeta = as.data.frame(filedata3())
          df2 <- merge(df2, AnnoMeta[, c("Feature_ID", "KEGG", "Metabolite")], by = "Feature_ID", all.x = TRUE)
        }
        write.csv(df2, file, row.names = FALSE)
      }
    )

    output$Loadings1 <- renderPlot({
      library(ggfortify)
      requireNamespace("ggplot2", quietly = TRUE)
      requireNamespace("ggfortify", quietly = TRUE)
        ggplot2::autoplot(loadings_metab()$pca_res, data = metadata(), colour = input$phenotypeSelector, loadings = TRUE)
    })

    # Render the download handler
    output$downloadLoadings1 <- downloadHandler(
      filename = function() {
        "Loadings_Metabolites.png"
      },
      content = function(file) {
        p <- ggplot2::autoplot(loadings_metab()$pca_res, data = metadata(), colour = input$phenotypeSelector, loadings = TRUE)
        ggplot2::ggsave(file, plot = p, device = "png")
        }
    )

    output$heatmap1 <- renderPlot({
      selected_variable <- input$phenotypeSelector
      levels_selected_variable <- unique(metadata()[[selected_variable]])
      if (length(levels_selected_variable) == 2) {
        # Usar una paleta diferente para dos niveles
        col_palette <- c("Level1" = "#1B9E77", "Level2" = "#D95F02")
      } else {
        col_palette <- RColorBrewer::brewer.pal(length(levels_selected_variable), "Set1")
      }
      # Column annotation
      column_anno = ComplexHeatmap::HeatmapAnnotation(
        selected_variable = as.factor(loadings_metab()$heatmap_data_sub_order[[selected_variable]]),
        col = list(selected_variable = setNames(col_palette, levels_selected_variable)),
        annotation_legend_param = list(selected_variable = list(title_position = "topleft", legend_direction = "vertical"))
      )
      metab_heatmap_plot = ComplexHeatmap::Heatmap(
        loadings_metab()$data_heat, cluster_columns = FALSE, cluster_rows = TRUE,
        row_title = "Metabolite Abundance", column_title = "Tissues", name = "Z-score",
        heatmap_legend_param = list(title_position = "topleft", legend_direction = "vertical"),
        show_row_names = TRUE, row_names_side = "left", row_names_gp = grid::gpar(fontsize = 8),
        #show_row_names = FALSE,
        show_column_names = FALSE,  top_annotation = column_anno
        )
      ComplexHeatmap::draw(metab_heatmap_plot, heatmap_legend_side = "right",
                           annotation_legend_side = "left", padding = ggplot2::unit(c(2, 3, 2, 40), "mm"))
    })

    output$downloadHeatmap1 <- downloadHandler(
      filename = function() {
        "HeatmapModMetabolomics.png"
      },
      content = function(file) {
        png(file, width = 800, height = 600)  # Adjust dimensions as needed
        selected_variable <- input$phenotypeSelector
        levels_selected_variable <- unique(metadata()[[selected_variable]])

        if (length(levels_selected_variable) == 2) {
          col_palette <- c("Level1" = "#1B9E77", "Level2" = "#D95F02")
        } else {
          col_palette <- RColorBrewer::brewer.pal(length(levels_selected_variable), "Set1")
        }

        column_anno = ComplexHeatmap::HeatmapAnnotation(
          selected_variable = as.factor(loadings_metab()$heatmap_data_sub_order[[selected_variable]]),
          col = list(selected_variable = setNames(col_palette, levels_selected_variable)),
          annotation_legend_param = list(selected_variable = list(title_position = "topleft", legend_direction = "vertical"))
        )

        metab_heatmap_plot = ComplexHeatmap::Heatmap(
          loadings_metab()$data_heat, cluster_columns = FALSE, cluster_rows = TRUE,
          row_title = "Metabolite Abundance", column_title = "Tissues", name = "Z-score",
          heatmap_legend_param = list(title_position = "topleft", legend_direction = "vertical"),
          show_row_names = TRUE, row_names_side = "left", row_names_gp = grid::gpar(fontsize = 8),
          show_column_names = FALSE, top_annotation = column_anno
        )

        ComplexHeatmap::draw(metab_heatmap_plot, heatmap_legend_side = "right",
                             annotation_legend_side = "left", padding = ggplot2::unit(c(2, 3, 2, 40), "mm"))
        dev.off()
      }
    )


    partial_cors2 <- reactive({
      withProgress(message = 'Calculating partial correlations (Proteins/Genes)...', value = 0, {
        if (is.null(demo_loaded2())) {
          req(filedata2())
          Expression_mat <- filedata2()
          Sys.sleep(1)  #
          par_cor <- partial_cors(Expression_mat = Expression_mat)
        } else {
          Sys.sleep(1)
          par_cor <- load_partial_cor_prot()
        }
        incProgress(100, detail = 'Complete!')
        list(par_cor = par_cor)
      })
    })

    output$matrizTable2 <- renderPrint({
      partial_cors2()$par_cor[1:5,1:5]
    })

    # Render the download handler
    output$downloadParCor2 <- downloadHandler(
      filename = function() {
        "PartialCorProteins/Genes.csv"
      },
      content = function(file) {
        write.csv(partial_cors2()$par_cor, file, row.names = TRUE)
      }
    )

    hierarchical_cluster2 <- reactive({
      par_cor = partial_cors2()$par_cor
      hc = hierarchical_cluster(parcor_mat = par_cor, tom = TRUE, min_module_size = 10)
      hclusterTree = hc$hclustTree
      hcDynMods = hc$dynamicMods_numeric
      hcCluster_assignments = hc$cluster_assignments
      return(list(hclusterTree = hclusterTree,
                  hcDynMods = hcDynMods,
                  hcCluster_assignments = hcCluster_assignments ))
    })

    output$hc_plot2 <- renderPlot({
      hcClu = hierarchical_cluster2()$hclusterTree
      hcMod = hierarchical_cluster2()$hcDynMods
      WGCNA::plotDendroAndColors(dendro = hcClu,
                                 colors = hcMod,
                                 dendroLabels = FALSE,
                                 hang = 0.03,
                                 addGuide = TRUE,
                                 guideHang = 0.05,
                                 groupLabels = "Modules",
                                 main = "Feature dendrogram and module assignments")
    })

    # Render the download handler
    output$downloadhc_plot2 <- downloadHandler(
      filename = function() {
        "Hierarchical_clusterProtein/Genes.png"
      },
      content = function(file) {
        png(file)
        hcClu = hierarchical_cluster2()$hclusterTree
        hcMod = hierarchical_cluster2()$hcDynMods
        WGCNA::plotDendroAndColors(dendro = hcClu,
                                   colors = hcMod,
                                   dendroLabels = FALSE,
                                   hang = 0.03,
                                   addGuide = TRUE,
                                   guideHang = 0.05,
                                   groupLabels = "Modules",
                                   main = "Feature dendrogram and module assignments")
        dev.off()
      }
    )

    cluster_assignments_genes1 <- reactive({
      cluster_genes = as.data.frame(hierarchical_cluster2()$hcCluster_assignments)
      if (is.null(filedata4())) {
        cluster_assignments_Prot <- cluster_assignments_genes(cluster_genes = cluster_genes, Prot_annotation = NULL)
      } else {
        cluster_assignments_Prot <- cluster_assignments_genes(cluster_genes = cluster_genes, Prot_annotation = filedata4())
      }
      return(list(cluster_assignments_Prot = cluster_assignments_Prot))
    })

    output$tableClusterAssig4 <- DT::renderDataTable({
      df1 = cluster_assignments_genes1()$cluster_assignments_Prot
      DT::datatable(df1)
    })

    # Render the download handler
    output$downloadClusterAssig2 <- downloadHandler(
      filename = function() {
        "ClusterAssigGenes/Proteins.csv"
      },
      content = function(file) {
        write.csv(cluster_assignments_genes1()$cluster_assignments_Prot, file, row.names = TRUE)
      }
    )

    databaseSelectorList <- reactive({
      gene_set_library = readxl::read_excel("example_data/Gene_set_Library.xlsx", col_names = FALSE)
      choices <- gene_set_library[[1]]
      data.frame(choices = choices)
    })

    observe({
      updateSelectInput(session, "databaseSelector", choices = databaseSelectorList()$choices)
    })

    # curl::has_internet()
    assign("has_internet_via_proxy", TRUE, environment(curl::has_internet))
    library(enrichR)
    enrichR::listEnrichrSites()

    Genes_Prot_enrich <- reactive({
      withProgress(message = 'Performing enrichment analysis...', value = 0, {
        if (is.null(enrich_loaded())) {
          req(input$databaseSelector)
          selected_database <- input$databaseSelector
          cluster_assignments_ProtGenes <- cluster_assignments_genes1()$cluster_assignments_Prot
          cluster_assignments_Prot_enrich <- Assigment_genes_enrichr(cluster_assignments_ProtGenes = cluster_assignments_ProtGenes,
                                                                     database = selected_database)
          Sys.sleep(1)
        } else {
          Sys.sleep(1)
          cluster_assignments_Prot_enrich <- load_enrichment_mouse()
        }
        incProgress(100, detail = 'Complete!')
        list(cluster_assignments_Prot_enrich = cluster_assignments_Prot_enrich)
      })
    })


    output$tableClusterAssig3 <- DT::renderDataTable({
      df3 = Genes_Prot_enrich()$cluster_assignments_Prot_enrich
      DT::datatable(df3)
    })

    # Render the download handler
    output$downloadEnrichment <- downloadHandler(
      filename = function() {
        "EnrichmentbyModules.csv"
      },
      content = function(file) {
        write.csv(Genes_Prot_enrich()$cluster_assignments_Prot_enrich, file, row.names = TRUE)
      }
    )

    Eigengene2 <- reactive({
      req(filedata2())
      Expression_mat = filedata2()
      Cluster_assignments = hierarchical_cluster2()$hcCluster_assignments[,3]
      Eigengenes = Eigengenes(Expression_mat = Expression_mat,
                              cluster_assignments = Cluster_assignments)$module_eigenmetab_Me
      return(list(Eigengenes = Eigengenes))
    })

    output$tableEigengene2 <- DT::renderDataTable({
      df3 = as.data.frame(Eigengene2()$Eigengenes)
      DT::datatable(df3)
    })

    # Render the download handler
    output$downloadtableEigengene2 <- downloadHandler(
      filename = function() {
        "EigenfeaturesProteins_Genes.csv"
      },
      content = function(file) {
        write.csv(Eigengene2()$Eigengenes, file, row.names = TRUE)
      }
    )

    output$heatmapEigenProt <- renderPlot({
      metab_heatmap_plot = ComplexHeatmap::Heatmap(
        as.data.frame(t(Eigengene2()$Eigengenes)), cluster_columns = FALSE, cluster_rows = TRUE,
        row_title = "Eigenfeatures", column_title = "Samples", name = "Z-score",
        heatmap_legend_param = list(title_position = "topleft", legend_direction = "vertical"),
        show_row_names = TRUE, row_names_side = "left", row_names_gp = grid::gpar(fontsize = 8),
        show_column_names = TRUE
      )
      ComplexHeatmap::draw(metab_heatmap_plot, heatmap_legend_side = "right",
                           annotation_legend_side = "left", padding = ggplot2::unit(c(2, 3, 2, 40), "mm"))
    })

    output$downloadheatmapEigenProt <- downloadHandler(
      filename = function() {
        "HeatmapEigenProt.png"
      },
      content = function(file) {
        png(file)
        metab_heatmap_plot = ComplexHeatmap::Heatmap(
          as.data.frame(t(Eigengene2()$Eigengenes)), cluster_columns = FALSE, cluster_rows = TRUE,
          row_title = "Eigenfeatures", column_title = "Samples", name = "Z-score",
          heatmap_legend_param = list(title_position = "topleft", legend_direction = "vertical"),
          show_row_names = TRUE, row_names_side = "left", row_names_gp = grid::gpar(fontsize = 8),
          show_column_names = TRUE
        )
        ComplexHeatmap::draw(metab_heatmap_plot, heatmap_legend_side = "right",
                             annotation_legend_side = "left", padding = ggplot2::unit(c(2, 3, 2, 40), "mm"))
        dev.off()
      }
    )

    output$table6 <- DT::renderDataTable({
      df <- metadata()
      DT::datatable(df)
    })

    pheno_variablesPCA2 <- reactive({
      names(metadata())[-which(names(metadata()) == "Sample")]
    })

    # For metabolites global PCA
    observe({
      updateSelectInput(session, "phenotypeSelectorPCA2", choices = pheno_variablesPCA2())
    })

    Classification_Proteins <- reactive({
      eigengenes_prot = as.data.frame(Eigengene2()$Eigengenes)
      metadata <- as.data.frame(metadata())
      phenotype_variable = input$phenotypeSelector2
      significance_threshold = input$pValueThreshold2
      classification_proteins <- perform_classification( eigengene_data = eigengenes_prot,
                                                           metadata = metadata,
                                                           phenotype_variable = phenotype_variable,
                                                         significance_threshold = significance_threshold)
      return(list(
        result = classification_proteins$result,
        plots = classification_proteins$plots))
    })

    output$classification_results2 <- DT::renderDataTable({
      Classification_Proteins()$result
    })

    # Render the download handler
    output$downloadClassification_results2 <- downloadHandler(
      filename = function() {
        "ClassByEigenfeatures_Proteins/Genes.csv"
      },
      content = function(file) {
        write.csv(Classification_Proteins()$result, file, row.names = TRUE)
      }
    )

    output$classification_plot_2_all <- renderPlot({
      selected_variable <- input$phenotypeSelector2
      levels_selected_variable <- unique(metadata()[[selected_variable]])
      if (length(levels_selected_variable) < 3) {
        class_names <- levels_selected_variable
        class_label <- paste(class_names, collapse = " vs ")
        plot <- Classification_Proteins()$plots[[1]]
        plot <- plot +
          ggplot2::labs(title = class_label, fill = as.factor(levels_selected_variable),
                        x = "Variables",
                        y = "Class") +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 90, hjust = 1)
      )
        classification_plot2(plot)
        return(plot)
      } else {
        # Print multiple boxplot charts, one for each level of the selected variable
        plots_list <- lapply(1:length(levels_selected_variable), function(i) {
          Classification_Proteins()$plots[[i]]
        })
        plot <- cowplot::plot_grid(plotlist = plots_list)
        classification_plot2(plot)  # Store the plot in the reactive variable
        return(plot)
      }
    })

    output$downloadClassification_plot_2_all <- downloadHandler(
      filename = function() {
        "Boxplot_classificationProteins/Genes.png"
      },
      content = function(file) {
        # Save the plot stored in the reactive variable
        ggplot2::ggsave(file, plot = classification_plot2(), device = "png")
      }
    )

    loadings_Prot <- reactive({
      req(filedata2())
      data = filedata2()
      selected_variable <- input$phenotypeSelector2
      data$missing_count = rowSums(is.na(data))
      feature_mat = subset(data, missing_count <= 0.1 * (ncol(data)-2))
      features <- feature_mat[,1]
      feature_mat_t <- as.matrix(scale(t(feature_mat[,-c(1,ncol(feature_mat))])))
      colnames(feature_mat_t) <- features
      feature_mat_t_imp = impute::impute.knn(feature_mat_t, k = min(10, nrow(feature_mat_t)))
      feature_mat_t_imp_data= feature_mat_t_imp$data
      cluster_Prot <- subset(hierarchical_cluster2()$hcCluster_assignments, col == input$moduleSelector2)
      #cluster_Metab <- subset(cluster_assignments_metabolites1()$cluster_assignments_metab, cluster == "cluster_000011")
      cluster_variables_Prot <- cluster_Prot$feature
      cluster_variables_ProtSymbol <- cluster_variables_Prot
      cluster_expression_matrix_Prot <- feature_mat_t_imp_data[, colnames(feature_mat_t_imp_data) %in% cluster_variables_Prot, drop = FALSE]
      combined_data <- merge(metadata()[,c("Sample", selected_variable)], cluster_expression_matrix_Prot, by.x = "Sample", by.y = "row.names", all.x = TRUE)
      heatmap_data_sub_order <- combined_data[order(combined_data[[selected_variable]]), ]
      data_heat= t(as.matrix(heatmap_data_sub_order[ , 3:ncol(heatmap_data_sub_order)]))
      pca_res <- prcomp(cluster_expression_matrix_Prot)
      return(list(pca_res = pca_res, data_heat= data_heat, heatmap_data_sub_order = heatmap_data_sub_order, cluster_variables_ProtSymbol = cluster_variables_ProtSymbol))
    })

    output$ModuleFeatures2Annot <- DT::renderDataTable({
      req(loadings_Prot())
      df2 = as.data.frame(loadings_Prot()$cluster_variables_ProtSymbol)
      names(df2) = "Feature_ID"
      if (!is.null(filedata4())) {
        AnnoProt = as.data.frame(filedata4())
        df2 <- merge(df2, AnnoProt[, c("Feature_ID", "Symbol")], by = "Feature_ID", all.x = TRUE)
      }
      DT::datatable(df2, rownames = FALSE)
    })

    # Render the download handler
    output$downloadModuleFeatures2Annot <- downloadHandler(
      filename = function() {
        "FeaturesOnProteomicsModule.csv"
      },
      content = function(file) {
        df2 = as.data.frame(loadings_Prot()$cluster_variables_ProtSymbol)
        names(df2) = "Feature_ID"
        if (!is.null(filedata4())) {
          AnnoProt = as.data.frame(filedata4())
          df2 <- merge(df2, AnnoProt[, c("Feature_ID", "Symbol")], by = "Feature_ID", all.x = TRUE)
          }
        write.csv(df2, file, row.names = FALSE)
      }
    )

    output$Loadings2 <- renderPlot({
      library(ggfortify)
      requireNamespace("ggplot2", quietly = TRUE)
      requireNamespace("ggfortify", quietly = TRUE)
      ggplot2::autoplot(loadings_Prot()$pca_res, data = metadata(), colour = input$phenotypeSelector2, loadings = TRUE)
    })

    # Render the download handler
    output$downloadLoadings2 <- downloadHandler(
      filename = function() {
        "Loadings_Proteomics_Genes.png"
      },
      content = function(file) {
        p <- ggplot2::autoplot(loadings_Prot()$pca_res, data = metadata(), colour = input$phenotypeSelector2, loadings = TRUE)
        ggplot2::ggsave(file, plot = p, device = "png")
      }
    )

    output$heatmap2 <- renderPlot({
      selected_variable <- input$phenotypeSelector2
      levels_selected_variable <- unique(metadata()[[selected_variable]])

      if (length(levels_selected_variable) == 2) {
        col_palette <- c("Level1" = "#1B9E77", "Level2" = "#D95F02")
      } else {
        col_palette <- RColorBrewer::brewer.pal(length(levels_selected_variable), "Set1")
      }

      # Column annotation
      column_anno = ComplexHeatmap::HeatmapAnnotation(
        selected_variable = as.factor(loadings_Prot()$heatmap_data_sub_order[[selected_variable]]),
        col = list(selected_variable = setNames(col_palette, levels_selected_variable)),
        annotation_legend_param = list(selected_variable = list(title_position = "topleft", legend_direction = "vertical"))
      )

      Prot_heatmap_plot = ComplexHeatmap::Heatmap(
        loadings_Prot()$data_heat, cluster_columns = FALSE, cluster_rows = TRUE,
        row_title = "Genes Abundance", column_title = "Tissues", name = "Z-score",
        heatmap_legend_param = list(title_position = "topleft", legend_direction = "vertical"),
        show_row_names = TRUE, row_names_side = "left", row_names_gp = grid::gpar(fontsize = 8),
        show_column_names = FALSE,  top_annotation = column_anno
      )

      ComplexHeatmap::draw(Prot_heatmap_plot, heatmap_legend_side = "right",
                           annotation_legend_side = "left", padding = ggplot2::unit(c(2, 3, 2, 40), "mm"))
    })

    output$downloadHeatmap2 <- downloadHandler(
      filename = function() {
        "HeatmapModProt_Genes.png"
      },
      content = function(file) {
        png(file, width = 800, height = 600)  # Adjust dimensions as needed
        selected_variable <- input$phenotypeSelector2
        levels_selected_variable <- unique(metadata()[[selected_variable]])

        if (length(levels_selected_variable) == 2) {
          col_palette <- c("Level1" = "#1B9E77", "Level2" = "#D95F02")
        } else {
          col_palette <- RColorBrewer::brewer.pal(length(levels_selected_variable), "Set1")
        }

        column_anno = ComplexHeatmap::HeatmapAnnotation(
          selected_variable = as.factor(loadings_Prot()$heatmap_data_sub_order[[selected_variable]]),
          col = list(selected_variable = setNames(col_palette, levels_selected_variable)),
          annotation_legend_param = list(selected_variable = list(title_position = "topleft", legend_direction = "vertical"))
        )

        Prot_heatmap_plot = ComplexHeatmap::Heatmap(
          loadings_Prot()$data_heat, cluster_columns = FALSE, cluster_rows = TRUE,
          row_title = "Genes Abundance", column_title = "Tissues", name = "Z-score",
          heatmap_legend_param = list(title_position = "topleft", legend_direction = "vertical"),
          show_row_names = TRUE, row_names_side = "left", row_names_gp = grid::gpar(fontsize = 8),
          show_column_names = FALSE, top_annotation = column_anno
        )

        ComplexHeatmap::draw(Prot_heatmap_plot, heatmap_legend_side = "right",
                             annotation_legend_side = "left", padding = ggplot2::unit(c(2, 3, 2, 40), "mm"))
        dev.off()
      }
    )

    Cor_Prot_Metab1 <- reactive({
      threshold <- input$pValueThreshold3
      eigengenes_Prot <- Eigengene2()$Eigengenes
      eigengenes_metab <- Eigengene1()$Eigengenes

      cluster_assignments_metab <- hierarchical_cluster1()$hcCluster_assignments2
      Count_Metab <- table(cluster_assignments_metab$col)

      cluster_assignments_ProtGenes <- hierarchical_cluster2()$hcCluster_assignments
      Count_Prot <- table(cluster_assignments_ProtGenes$col)

      if (is.null(filedata4())) {
        # Si filedata4 es NULL, calcular la correlación usando cor() directamente
        cor_Prot_metab_WGCNA <- cor(eigengenes_Prot, eigengenes_metab, method = 'spearman', use = "pairwise.complete.obs")
        cor_Prot_metab_list <- reshape2::melt(cor_Prot_metab_WGCNA, varnames = c("Prot_module", "Metab_module"))
        colnames(cor_Prot_metab_list) <- c("Prot_module", "Metab_module", "Correlation")
        #Top_cor_Prot_metab <- subset(cor_Prot_metab_list, abs(Correlation) >= threshold)
        cor_Prot_metab_list1 <- cor_Prot_metab_list[order(abs(cor_Prot_metab_list$Correlation), decreasing = TRUE), ][1:5, ]
        cor_Prot_metab_list2 <- subset(cor_Prot_metab_list, abs(Correlation) >= threshold)

        Top_cor_Prot_metab <- if (nrow(cor_Prot_metab_list1) >= nrow(cor_Prot_metab_list2)) {
          cor_Prot_metab_list1
        } else {
          cor_Prot_metab_list2
        }

        Top_cor_Prot_metab$Correlation <- round(Top_cor_Prot_metab$Correlation, 2)
        filtered_cor_Prot_metab_list <- as.data.frame(Top_cor_Prot_metab)

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

        unique_from <- unique(edges$from)
        label_from <- paste0("Module", seq_along(unique_from))
        value_from <- Count_Prot[match(unique_from, names(Count_Prot))]
        shape_from <- "triangle"
        title_from0 = paste(value_from, "genes", sep = " ")
        color_from <- "darkgreen"

        unique_to <- unique(edges$to)
        label_to <- paste0("Module", seq_along(unique_to))
        value_to <- Count_Metab[match(unique_to, names(Count_Metab))]
        shape_to <- "diamond"
        title_to = paste(value_to, "metabolites", sep = " ")
        color_to <- "orange"

        #nodes to function Visnetwork
        nodes <- data.frame(id = c(unique_from, unique_to),
                            #label = c(label_from, label_to),
                            label = c(title_from0, title_to),
                            value = c(value_from, value_to),
                            shape = c(rep(shape_from, length(unique_from)), rep(shape_to, length(unique_to))),
                            #title = c(title_from0, title_to),
                            title = c(label_from, label_to),
                            color = c(rep(color_from, length(unique_from)), rep(color_to, length(unique_to))),
                            shadow = TRUE)
        nodes <- nodes[,c("id", "label", "value", "shape", "title", "color", "shadow")]

        edges[["from"]] <- sub("^#", "", edges[["from"]])
        edges[["to"]] <- sub("^#", "", edges[["to"]])

        rownames(nodes) <- NULL
        nodes[["id"]] <- sub("^#", "", nodes[["id"]])

      } else {
        cluster_assignments_metab <- cluster_assignments_metabolites1()$cluster_assignments_metab
        cluster_assignments_Prot_enrich <- Genes_Prot_enrich()$cluster_assignments_Prot_enrich

        Cor_Prot_Metab <- Modules_correlation(eigengenes_Prot, eigengenes_metab,
                                              cluster_assignments_Prot_enrich,
                                              cluster_assignments_metab, threshold = threshold)

        Top_cor_Prot_metab <- Cor_Prot_Metab$Top_cor_Prot_metab
        filtered_cor_Prot_metab_list <- Cor_Prot_Metab$filtered_cor_Prot_metab_list
        cor_Prot_metab_WGCNA <- Cor_Prot_Metab$cor_Prot_metab_WGCNA
        edges <- Cor_Prot_Metab$edges
        nodes <- Cor_Prot_Metab$nodes
      }

      list(Top_cor_Prot_metab = Top_cor_Prot_metab,
           filtered_cor_Prot_metab_list = filtered_cor_Prot_metab_list,
           cor_Prot_metab_WGCNA = cor_Prot_metab_WGCNA,
           edges = edges,
           nodes = nodes)
    })

    output$tableCorrelation <- DT::renderDataTable({
      df4 = as.data.frame(Cor_Prot_Metab1()$Top_cor_Prot_metab)
      DT::datatable(df4)
    })

    # Render the download handler
    output$downloadOmicsCorrelation <- downloadHandler(
      filename = function() {
        "OmicsCorrelation.csv"
      },
      content = function(file) {
        write.csv(Cor_Prot_Metab1()$Top_cor_Prot_metab, file, row.names = TRUE)
      }
    )

    # Create a histogram of correlation
    output$Correlation_plot <- renderPlot({
      cor_Prot_metab_WGCNA = Cor_Prot_Metab1()$cor_Prot_metab_WGCNA
      hist(cor_Prot_metab_WGCNA, main = "Histogram of Correlation")
    })

    output$Network_plot <- renderPlot({
      filtered_cor_Prot_metab_list = as.data.frame(Cor_Prot_Metab1()$filtered_cor_Prot_metab_list)
      # Create the network graph
      network <- igraph::graph_from_data_frame(filtered_cor_Prot_metab_list, directed = FALSE)
      igraph::E(network)$label <- filtered_cor_Prot_metab_list$Correlation
      # Conditions for node type and color
      condicion_tipo <- ifelse(grepl("genes", igraph::V(network)$name), "lightgreen", "#E69F00")
      color_text <- ifelse(grepl("genes", igraph::V(network)$name), "darkgreen", "orange")
      legend_labels <- c("Genes", "Metabolites")
      plot(
        network,
        edge.label = igraph::E(network)$label,
        vertex.size = 2,
        vertex.color = condicion_tipo,
        vertex.label.color = color_text,
        edge.label.cex = 0.8,
        edge.label.color = "black",
        edge.width = 2,
        edge.color = "gray",
        main = "Modules network"
      )
      legend("topright", legend = legend_labels, pch = 21, pt.bg = c("lightgreen", "orange"),
             pt.cex = 1.5, cex = 0.8, col = c("darkgreen", "black"), bty = "n")
     })

    mynetwork <- reactive({
      dfnodes <- as.data.frame(Cor_Prot_Metab1()$nodes)
      dfedges <- as.data.frame(Cor_Prot_Metab1()$edges)

      visNetwork::visNetwork(
        nodes = dfnodes,
        edges = dfedges,
        #main = "Protein-Metabolite Network",
        width = "100%",
        height = "800px"
      ) %>%
        visLegend(
          useGroups = FALSE,
          addNodes = data.frame(
            label = c("Genes Modules", "Metabolites Modules"),
            shape = c("triangle", "diamond"),
            color = c("darkgreen", "orange")),
          addEdges = data.frame(
            label = "Correlation",
            shape = "line",
            length = 200,
            color = "darkgreen")
        ) %>%
        visInteraction(navigationButtons = TRUE)
    })
    output$network <- renderVisNetwork({
      mynetwork()
    })

    output$downloadNetwork <- downloadHandler(
      filename = function() {
        paste('network-', Sys.Date(), '.html', sep='')
      },
      content = function(con) {
        mynetwork() %>% visSave(con)
      }
    )


    ImpVar_Prot_Metab1 <- reactive({
      Cor_Prot_Metab = as.data.frame(Cor_Prot_Metab1()$Top_cor_Prot_metab)
      cluster_assignments_metab = cluster_assignments_metabolites1()$cluster_assignments_metab
      req(filedata())
      ExpressionMetab_mat = filedata()
      req(filedata2())
      ExpressionProt_mat = filedata2()
      if (is.null(filedata4())) {
        cluster_assignments_Prot = as.data.frame(cluster_assignments_genes1()$cluster_assignments_Prot)
        str(cluster_assignments_Prot)
      } else{
        cluster_assignments_Prot = Genes_Prot_enrich()$cluster_assignments_Prot_enrich
      }
      ImpVar_Prot_Metab <- FeaturesAnnot_correlation(Cor_Prot_Metab = Cor_Prot_Metab,
                                                     cluster_assignments_Prot = cluster_assignments_Prot,
                                                     cluster_assignments_metab = cluster_assignments_metab,
                                                     ExpressionProt_mat = ExpressionProt_mat,
                                                     ExpressionMetab_mat = ExpressionMetab_mat,
                                                     top_n = 5) #$correlation_matrices_list
      return(list(
        Top_correlations = ImpVar_Prot_Metab$Top_correlations,
        cluster_assignments = ImpVar_Prot_Metab$cluster_assignments,
        correlation_matrices = ImpVar_Prot_Metab$correlation_matrices,
        Important_features = ImpVar_Prot_Metab$Important_features,
        correlation_List = ImpVar_Prot_Metab$correlation_List
      ))
    })

    Important_Features <- reactive({
      custom_palette <-colorRampPalette(c(RColorBrewer::brewer.pal(11, "RdYlBu")[11], "white", RColorBrewer::brewer.pal(11, "RdYlBu")[1]))(n = 100)

      if(input$visualization_list == 1){
        df1_1 = as.data.frame(ImpVar_Prot_Metab1()$cluster_assignments[[1]])
        df1_2 = as.data.frame(ImpVar_Prot_Metab1()$cluster_assignments[[2]])
        df4   = ImpVar_Prot_Metab1()$correlation_List[[1]]
        df4_1 = hist(ImpVar_Prot_Metab1()$correlation_matrices[[1]], main = "Top 1 Modules Correlation")
        df4_2 = corrplot::corrplot(ImpVar_Prot_Metab1()$correlation_matrices[[1]], type = "upper",  tl.col = "black", col = custom_palette)
        df5_1 = ImpVar_Prot_Metab1()$Important_features[[1]]
        df5_2 = ImpVar_Prot_Metab1()$Important_features[[2]]
      }

      if(input$visualization_list == 2){
        df1_1 = as.data.frame(ImpVar_Prot_Metab1()$cluster_assignments[[3]])
        df1_2 = as.data.frame(ImpVar_Prot_Metab1()$cluster_assignments[[4]])
        df4   = ImpVar_Prot_Metab1()$correlation_List[[2]]
        df4_1 = hist(ImpVar_Prot_Metab1()$correlation_matrices[[2]], main = "Top 2 Modules Correlation")
        df4_2 = corrplot::corrplot(ImpVar_Prot_Metab1()$correlation_matrices[[2]], type = "upper",  tl.col = "black", col = custom_palette)
        df5_1 = ImpVar_Prot_Metab1()$Important_features[[3]]
        df5_2 = ImpVar_Prot_Metab1()$Important_features[[4]]
      }

      if(input$visualization_list == 3){
        df1_1 = as.data.frame(ImpVar_Prot_Metab1()$cluster_assignments[[5]])
        df1_2 = as.data.frame(ImpVar_Prot_Metab1()$cluster_assignments[[6]])
        df4   = ImpVar_Prot_Metab1()$correlation_List[[3]]
        df4_1 = hist(ImpVar_Prot_Metab1()$correlation_matrices[[3]], main = "Top 3 Modules Correlation")
        df4_2 = corrplot::corrplot(ImpVar_Prot_Metab1()$correlation_matrices[[3]], type = "upper",  tl.col = "black", col = custom_palette)
        df5_1 = ImpVar_Prot_Metab1()$Important_features[[5]]
        df5_2 = ImpVar_Prot_Metab1()$Important_features[[6]]
      }

      if(input$visualization_list == 4){
        df1_1 = as.data.frame(ImpVar_Prot_Metab1()$cluster_assignments[[7]])
        df1_2 = as.data.frame(ImpVar_Prot_Metab1()$cluster_assignments[[8]])
        df4   = ImpVar_Prot_Metab1()$correlation_List[[4]]
        df4_1 = hist(ImpVar_Prot_Metab1()$correlation_matrices[[4]], main = "Top 2 Modules Correlation")
        df4_2 = corrplot::corrplot(ImpVar_Prot_Metab1()$correlation_matrices[[4]], type = "upper",  tl.col = "black", col = custom_palette)
        df5_1 = ImpVar_Prot_Metab1()$Important_features[[7]]
        df5_2 = ImpVar_Prot_Metab1()$Important_features[[8]]
      }

      if(input$visualization_list == 5){
        df1_1 = as.data.frame(ImpVar_Prot_Metab1()$cluster_assignments[[9]])
        df1_2 = as.data.frame(ImpVar_Prot_Metab1()$cluster_assignments[[10]])
        df4   = ImpVar_Prot_Metab1()$correlation_List[[5]]
        df4_1 = hist(ImpVar_Prot_Metab1()$correlation_matrices[[5]], main = "Top 3 Modules Correlation")
        df4_2 = corrplot::corrplot(ImpVar_Prot_Metab1()$correlation_matrices[[5]], type = "upper",  tl.col = "black", col = custom_palette)
        df5_1 = ImpVar_Prot_Metab1()$Important_features[[9]]
        df5_2 = ImpVar_Prot_Metab1()$Important_features[[10]]
      }

      return(list(df1_1 = df1_1, df1_2=df1_2, df5_1 = df5_1, df5_2=df5_2, df4=df4, df4_1=df4_1, df4_2=df4_2))
    })

    output$ImportantVariables <- DT::renderDataTable({
      df = as.data.frame(ImpVar_Prot_Metab1()$Top_correlations)
      DT::datatable(df)
    })

    output$Correlation_plotImp <- renderPlot({
      Important_Features()$df4_1
    })

    output$CorplotImp <- renderPlot({
      Important_Features()$df4_2
    })

    output$Correlation_mod <- DT::renderDataTable({
      df4 = data.frame(Important_Features()$df4)
      DT::datatable(df4)
    })
    # Render the download handler
    output$downloadModCorrelation <- downloadHandler(
      filename = function() {
        "ModulesCorrelation.csv"
      },
      content = function(file) {
        write.csv(Important_Features()$df4, file, row.names = TRUE)
      }
    )

    #Metabolites
    # output$cluster_assignments_2 <- DT::renderDataTable({
    #   df <- Important_Features()$df1_2
    # })

    output$cluster_assignments_features2 <- DT::renderDataTable({
      df <- Important_Features()$df1_2
      # Selecciona las columnas de interés
      df_features <- df %>%
        select(feature, feature_name, feature_map, Metabolite) %>%
        distinct()  # Elimina duplicados
      DT::datatable(df_features)
    })

    output$cluster_assignments_summary2 <- DT::renderDataTable({
      df <- Important_Features()$df1_2
      # Prepara la tabla con la información de las demás columnas
      cluster <- unique(df$cluster)
      col <- unique(df$col)
      SummaryData <- data.frame(cluster = cluster, col = col, stringsAsFactors = FALSE)
      DT::datatable(SummaryData)
    })

    output$downloadcluster_assignments_2 <- downloadHandler(
      filename = function() {
        "Metabolites_TopModule.csv"
      },
      content = function(file) {
        write.csv(Important_Features()$df1_2, file, row.names = TRUE)
      }
    )

    #Proteins
    # output$cluster_assignments_1 <- DT::renderDataTable({
    #   Important_Features()$df1_1
    # })

    output$cluster_assignments_features <- DT::renderDataTable({
      df <- Important_Features()$df1_1

      # Selecciona las columnas de interés
      df_features <- df %>%
        select(feature, feature_name) %>%
        distinct()  # Elimina duplicados

      DT::datatable(df_features)
    })

    output$cluster_assignments_summary <- DT::renderDataTable({
      df <- Important_Features()$df1_1

      # Prepara la tabla con la información de las demás columnas
      df_summary <- df %>%
        group_by(cluster, col) %>%
        summarise(
          enriched_Term = unique(enriched_Term),
          enriched_Overlap = unique(enriched_Overlap),
          enriched_Genes = unique(enriched_Genes),
          enriched_P.value = unique(enriched_P.value),
          enriched_Adjusted_P.value = unique(enriched_Adjusted.P.value),
          enriched_GO = unique(enriched_GO)
        ) %>%
        ungroup()

      DT::datatable(df_summary)
    })

    # Render the download handler
    output$downloadcluster_assignments_1 <- downloadHandler(
      filename = function() {
        "Proteins_Genes_TopModule.csv"
      },
      content = function(file) {
        write.csv(Important_Features()$df1_1, file, row.names = TRUE)
      }
    )

    output$Important_features_1 <- renderText({
      Important_Features()$df5_1
    })

    output$Important_features_2 <- renderText({
      Important_Features()$df5_2
    })

  })
}


## To be copied in the UI
# mod_module1_ui("module1_1")

## To be copied in the server
# mod_module1_server("module1_1")
