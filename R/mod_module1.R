utils::globalVariables(c("value", "feature", "Variable", "Expression", "Class"))

#' module1 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom grDevices colorRampPalette dev.off png
#' @importFrom graphics hist legend
#' @importFrom stats as.dist cor cov hclust na.omit prcomp quantile sd setNames t.test
#' @importFrom utils read.csv str unzip write.csv
#' @importFrom graphics par

options(shiny.maxRequestSize=100*1024^2)

#mod_module1_ui <- function(id, input, output, session) {
mod_module1_ui <- function(id) {
  ns <- NS(id)

  tagList(

    sidebarPanel(
      width = 4,

      fileInput(
        ns("Data1"),
        accept = c('text/csv',
                   'text/comma-separated-values',
                   'text/plain',
                   '.csv'),
        label = h5("Upload Expression Data 1")
      ),

      fileInput(
        ns("PhenoData1"),
        accept = c('text/csv',
                   'text/comma-separated-values',
                   'text/plain',
                   '.csv'),
        label = h5("Upload Annotation Data 1")
      ),

      div(style = "border-top: 1px solid #ccc; margin-top: 10px; margin-bottom: 10px;"),

      fileInput(
        ns("Data2"),
        accept = c('text/csv',
                   'text/comma-separated-values',
                   'text/plain',
                   '.csv'),
        label = h5("Upload Expression Data 2")
      ),

      fileInput(
        ns("PhenoData2"),
        accept = c('text/csv',
                   'text/comma-separated-values',
                   'text/plain',
                   '.csv'),
        label = h5("Upload Annotation Data 2")
      ),

      div(style = "border-top: 1px solid #ccc; margin-top: 10px; margin-bottom: 10px;"),

      fileInput(
        ns("Data3"),
        accept = c('text/csv',
                   'text/comma-separated-values',
                   'text/plain',
                   '.csv'),
        label = h5("Upload Expression Data 3")
      ),

      fileInput(
        ns("PhenoData3"),
        accept = c('text/csv',
                   'text/comma-separated-values',
                   'text/plain',
                   '.csv'),
        label = h5("Upload Annotation Data 3")
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

      label = h5("Upload Sample Metadata"),

      helpText(
        "Example data is available to help you get started with iModMix. You can use this data to run the application and explore its features."),

      actionButton(
        ns("runDemo"), "ccRCC datasets (id metabolites)", icon = icon("play")
      ),

      actionButton(
        ns("runDemoAll"), "LUAD datasets (id and unidentified metabolites)", icon = icon("play")
      )
    ),

    mainPanel(
      tabsetPanel(
        type = "tabs",
        tabPanel("Data 1",
                 tabsetPanel(
                   type = "tabs",
                   tabPanel("Data Input",
                            h4("Expression data 1",
                               shinyBS::bsButton("surfinfoMAM", label = "", icon = icon("info", lib = "font-awesome"), style = "default", size = "extra-small")),
                            shinyBS::bsPopover(id = "surfinfoMAM", title = "Information",
                                      content = HTML(paste0("Table reflecting the uploaded file expression Data 1. Check if the number of samples and the number of data_1 features are correct. The arrows to the right of each column title can be used for sorting data from increasing or decreasing values. The search bar can also be used to confirm the details of a feature of interest.")),
                                      placement = "right", trigger = "hover", options = list(container = "body")),
                            DT::DTOutput(ns("infotable")),
                            DT::DTOutput(ns("table")),
                            h4("Principal component analysis for each phenotype",
                               shinyBS::bsButton("surf-infoMPCAPh", label = "", icon = icon("info", lib = "font-awesome"), style = "default", size = "extra-small")),
                            shinyBS::bsPopover(id = "surf-infoMPCAPh", title = "More information",
                                      content = HTML(paste0("Drop-down menu displays conditions or sample descriptions provided with uploaded Metadata. Graph below displays Principal Component Analysis (PCA) plots representing each of your phenotype descriptions.")),
                                      placement = "right", trigger = "hover", options = list(container = "body") ),
                            selectInput(ns("phenotypeSelectorPCA"),
                                        label = "Select the phenotype of interest",
                                        choices = NULL,
                                        selected = NULL),
                            plotOutput(ns("PCA1")),
                            downloadButton(ns("downloadPCA"),
                                           "Principal component analysis"),
                            h4("Annotation data 1",
                               shinyBS::bsButton("surf-infoMAD", label = "", icon = icon("info", lib = "font-awesome"), style = "default", size = "extra-small")),
                            shinyBS::bsPopover(id = "surf-infoMAD", title = "More information",
                                      content = HTML(paste0("Table reflecting the uploaded file <i> Annotation Data 1</i>. Check if the total number of entries at bottom of table matches the total number of features in the \u201c Abundance Data 1\u201d. The arrows to the right of each column title can be used for sorting data from increasing or decreasing values. The search bar can also be used to confirm the details of a feature of interest.")),
                                      placement = "right", trigger = "hover", options = list(container = "body")
                               ),
                            DT::DTOutput(ns("tableAnnot1"))
                   ),
                   tabPanel("Module Assignments",
                            h4("Sparse partial correlations: data 1",
                               shinyBS::bsButton("surfInfoMPC", label = "", icon = icon("info", lib = "font-awesome"), style = "default", size = "extra-small")),
                            shinyBS::bsPopover(id = "surfInfoMPC", title = "More information",
                              content = HTML("<p>Partial correlation is a method of analyzing the relationship between two variables when other variables are present. Graphical Lasso (Glasso) is used to estimate the partial correlation and captures only direct associations.</p> <p>Below is a preview of the sparse partial correlation of the first five features. The full .csv file for the sparse partial correlation calculations for all the features can be downloaded at the bottom of the table.</p>"),
                              placement = "right", trigger = "hover", options = list(container = "body")),
                            verbatimTextOutput(ns("matrizTable")),
                            downloadButton(ns("downloadParCor"),
                                           "Partial correlation matrix"),
                            h4("Hierarchical clustering", shinyBS::bsButton("surfInfoMHC", label = "", icon = icon("info", lib = "font-awesome"), style = "default", size = "extra-small")),
                            shinyBS::bsPopover(id = "surfInfoMHC", title = "More information",
                                      content = HTML("<p>Hierarchical clustering is used to identify common neighbors between the features. Calculations are determined using the topographical overlap matrix (TOM) and based on the sparse partial correlations. Hierarchical clustering is visualized as a dendrogram.</p> <p> Axes: The vertical axis (y-axis) represents the dissimilarity between genes or modules, while the horizontal axis (x-axis) shows the genes or modules. Branches: Each line in the dendrogram represents a gene or module. Genes that are closer in the hierarchy (i.e., joined at a lower height in the dendrogram) have more similar expression profiles.</p>"),
                                      placement = "right", trigger = "hover", options = list(container = "body")),
                            plotOutput(ns("hc_plot")),
                            downloadButton(ns("downloadhc_plot"),
                                           "Hierarchical clustering Image (.png)"),
                            h4("Cluster assignments",
                               shinyBS::bsButton("surf-infoMAC", label = "", icon = icon("info", lib = "font-awesome"), style = "default", size = "extra-small")),
                            shinyBS::bsPopover(id = "surf-infoMAC", title = "More information",
                                      content = HTML(paste0("<p> Hierarchical clustering generates multiple modules (clusters) to which each feature is assigned. The table below details the following columns: </p> <ul> <li>  Feature: metabolite ID </li> <li> module_id: The module where the metabolite is assigned and the color used on the hierarchical clustering dendrogram </li> <li> If annotation data is available, it also shows the KEGG ID and the metabolite name </li></ul> <p>The arrows to the right of each column title can be used for sorting data from increasing or decreasing values. The search bar can also be used to find the details of a metabolomic feature of interest. The full .csv file of Cluster Assignments for all the metabolomic features can be downloaded at the bottom of the table. </p>")),
                                      placement = "right", trigger = "hover", options = list(container = "body")),
                            selectInput(
                              ns("Mapping1"),
                              label = "If you uploaded annotation data, select columns to view",
                              choices = NULL,
                              multiple = TRUE),
                            DT::DTOutput(ns("tableClusterAssig")),
                            downloadButton(ns("downloadClusterAssig"),
                                           "Cluster assigment table"),
                            h4("First principal component from each module (Eigenfeatures)",
                               shinyBS::bsButton("surf-infoMEF", label = "", icon = icon("info", lib = "font-awesome"), style = "default", size = "extra-small")),
                            shinyBS::bsPopover(id = "surf-infoMEF", title = "More information",
                                      content = HTML(paste0("<p>The first principal component (PC1) is calculated for each module, referred to as an eigenfeature. Eigenfeatures are useful for: </p> <ol> <li>  Relating the modules to the phenotypes. </li> <li> Obtaining the correlation between omics datasets (integration). </li> </ol> <p> The full .csv file of calculations for PC1 for metabolomics modules can be downloaded at the bottom of the table. </p> <p> The heatmap below shows eigenfeatures across samples. The vertical axis (y-axis) represents the eigenfeatures, and the horizontal axis (x-axis) displays the sample conditions. </p>")),
                                      placement = "right", trigger = "hover", options = list(container = "body")),
                            DT::DTOutput(ns("tableEigengene")),
                            downloadButton(ns("downloadtableEigengene"),
                                           "Eigenfeatures table"),
                            h4("Eigenfeatures heatmap"),
                            plotOutput(ns("heatmapEigenMetab")),
                            downloadButton(ns("downloadheatmapEigenMetab"),
                                           "Eigenfeatures heatmap Image (.png)"),
                            h4("Cluster assignments enriched",
                               shinyBS::bsButton("surf-info_MGCAE", label = "", icon = icon("info", lib = "font-awesome"), style = "default", size = "extra-small")),
                            shinyBS::bsPopover(id = "surf-info_MGCAE", title = "More information",
                                               content = HTML(paste0("Drop-down menu displays available libraries for pathway analysis. Choose a library to automatically amend the dataset cluster descriptions on table below. Under column enriched_Term the most highly correlated pathway is displayed and in the following columns, along with enriched_Genes, and p-values as determined by Enrichr. The search bar can also be used to find the details of a protein/gene or module of interest. The full .csv file of Cluster Enrichments for dataset can be downloaded at the bottom of the table.")),
                                               placement = "right", trigger = "hover", options = list(container = "body")),
                            helpText(
                              "Note: Upload the Annotation data to be able to run enrichment analysis."),
                            selectInput(
                              ns("databaseSelector1"),
                              label = "Select Library",
                              choices = NULL),
                            checkboxInput(ns("runEnrichment1"),  label = "Run Enrichment Analysis", value = FALSE),
                            DT::DTOutput(ns("tableClusterAssigAnnot1")),
                            downloadButton(ns("downloadEnrichment1"),
                                           "Enrichment analysis Data 1")
                   ),
                   tabPanel("Phenotype",
                            h4("Metadata",
                               shinyBS::bsButton("surf-info1PD", label = "", icon = icon("info", lib = "font-awesome"), style = "default", size = "extra-small")),
                            shinyBS::bsPopover(id = "surf-info1PD", title = "More information",
                                      content = HTML(paste0("Table reflecting the uploaded file <i>Metadata</i>. Check if the number of samples and the number of entries listed at the bottom of the table are the same. The arrows to the right of each column title can be used for sorting data from increasing or decreasing values. The search bar can also be used to confirm the details of a sample.")),
                                      placement = "right", trigger = "hover", options = list(container = "body")),
                            helpText(
                              "Note: Upload the metadata data to be able to run phenotype analysis."),
                            DT::DTOutput(ns("tableM1")),
                            h4("Classification between phenotypes by eigenfeatures",
                               shinyBS::bsButton("surf-infoMCPEf", label = "", icon = icon("info", lib = "font-awesome"), style = "default", size = "extra-small")),
                            shinyBS::bsPopover(id = "surf-infoMCPEf", title = "More information",
                                      content = HTML(paste0("<p>Statistical analysis by Students t-test compares phenotypes chosen from a drop-down menu. The eigenfeatures of each module, determined previously, are used as predictors. The user can also specify a significance threshold for the p-value, with the default set to 0.05. </p> <p> It returns a data frame with the following columns: </p> <ul> <li> Variable: Represents the ID of the module. </li> <li> Class: Lists the two levels of the phenotypes being compared. If there are more than two levels, it compares one level against the others. </li>  <li> Result_t: The t-statistic value. </li> <li> Result_pValue: The p-value for the test. </li> </ul> <p> Boxplots are automatically generated at the bottom for significant eigenfeatures, with dots marking outliers and a legend describing the compared phenotype. </p> ")),
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
                                                  "Boxplot classification Image (.png)")
                            ),
                            h4("Module screening",
                               shinyBS::bsButton("surf-infoMMS", label = "", icon = icon("info", lib = "font-awesome"), style = "default", size = "extra-small")),
                            shinyBS::bsPopover(id = "surf-infoMMS", title = "More information",
                                      content = HTML(paste0("The drop-down menu displays all modules generated by iModMix. Users can view the features within a selected module. If annotation data is available, it also shows the KEGG ID and metabolite name. PCA loading and heatmap plots are generated to visualize the behavior of each specific module across the phenotype.")),
                                      placement = "right", trigger = "hover", options = list(container = "body")),
                            fluidRow(
                              column(6,
                            selectInput(ns("moduleSelector"),
                                        label = "Select the module of interest",
                                        choices = NULL,
                                        selected = NULL)
                              ),
                            column(6,
                            selectInput(
                              ns("Screening1"),
                              label = "If you uploaded annotation data, select columns to view",
                              choices = NULL,
                              multiple = TRUE)
                            )
                            ),
                            DT::DTOutput(ns("ModuleFeaturesAnnot1")),
                            downloadButton(ns("downloadModuleFeaturesAnnot1"),
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

        tabPanel("Data 2",
                 tabsetPanel(
                   type = "tabs",
                   tabPanel("Data Input",
                            h4("Expression data 2",
                               shinyBS::bsButton("surf-info_2GEM", label = "", icon = icon("info", lib = "font-awesome"), style = "default", size = "extra-small")),
                            shinyBS::bsPopover(id = "surf-info_2GEM", title = "More information",
                                      content = HTML(paste0("Table reflecting uploaded file <i>data Expression data</i>. Check if the number of samples and the number of protein/genes are correct. The arrows to the right of each column title can be used for sorting data from increasing or decreasing values. The search bar can also be used to confirm the details of a protein/gene of interest. ")),
                                      placement = "right", trigger = "hover", options = list(container = "body") ),
                            DT::DTOutput(ns("infotable2")),
                            DT::DTOutput(ns("table2")),
                            h4("Principal component analysis for each phenotype",
                               shinyBS::bsButton("surf-info_PGPC", label = "", icon = icon("info", lib = "font-awesome"), style = "default", size = "extra-small")),
                            shinyBS::bsPopover(id = "surf-info_PGPC", title = "More information",
                                      content = HTML(paste0("Drop-down menu displays conditions or sample descriptions provided with uploaded <i>Metadata</i>. Graph below displays Principal Component Analysis (PCA) plots representing each of your phenotype descriptions. ")),
                                      placement = "right", trigger = "hover", options = list(container = "body")),
                            selectInput(ns("phenotypeSelectorPCA2"),
                                        label = "Select the phenotype of interest",
                                        choices = NULL,
                                        selected = NULL),
                            plotOutput(ns("PCA2")),
                            downloadButton(ns("downloadPCA2"),
                                           "Principal component analysis"),
                            h4("Annotation data 2",
                               shinyBS::bsButton("surf-info_PGAD", label = "", icon = icon("info", lib = "font-awesome"), style = "default", size = "extra-small")),
                            shinyBS::bsPopover(id = "surf-info_PGAD", title = "More information",
                                      content = HTML(paste0("Table reflecting the uploaded file <i>data Annotation Data</i>. Check if the total number of entries at bottom of table matches the total number of features in the <i>data Expression Data</i>. The arrows to the right of each column title can be used for sorting data from increasing or decreasing values. The search bar can also be used to confirm the details of a protein/gene of interest. ")),
                                      placement = "right", trigger = "hover", options = list(container = "body")),
                            DT::DTOutput(ns("tableAnnot2"))
                   ),
                   tabPanel("Module Assignments",
                            h4("Sparse partial correlations: data 2",
                               shinyBS::bsButton("surf-info_SPCforPG", label = "", icon = icon("info", lib = "font-awesome"), style = "default", size = "extra-small")),
                            shinyBS::bsPopover(id = "surf-info_SPCforPG", title = "More information",
                                      content = HTML(paste0("<p>Partial correlation is a method of analyzing the relationship between two variables when other variables are present. Graphical Lasso (Glasso) is used to estimated the partial correlation and captures only direct associations.  </p> <p> Preview of the sparse partial correlation of the first five proteins/genes. The full .csv file for the sparse partial correlation calculations for all the proteins/genes can be downloaded at the bottom of the table. </p>")),
                                      placement = "right", trigger = "hover", options = list(container = "body")
                               ),
                            verbatimTextOutput(ns("matrizTable2")),
                            downloadButton(ns("downloadParCor2"),
                                           "Partial correlation"),
                            h4("Hierarchical clustering",
                               shinyBS::bsButton("surf-info_PGHC", label = "", icon = icon("info", lib = "font-awesome"), style = "default", size = "extra-small")),
                            shinyBS::bsPopover(id = "surf-info_PGHC", title = "More information",
                                      content = HTML(paste0("<p>Hierarchical clustering is used to identify common neighbors between the proteins/genes. Calculations are determined using the topographical overlap matrix (TOM) and based on the sparse partial correlations. Hierarchical clustering is visualized as a dendrogram. </p> <p> Axes: The vertical axis (y-axis) represents the dissimilarity between metabolites or modules, while the horizontal axis (x-axis) shows the modules. </p> <p>Branches: Each line in the dendrogram represents a module. Modules that are closer in the hierarchy (i.e., joined at a lower height in the dendrogram) have more similar expression profiles. </p>")),
                                      placement = "right", trigger = "hover", options = list(container = "body")
                            ),
                            plotOutput(ns("hc_plot2")),
                            downloadButton(ns("downloadhc_plot2"),
                                           "Hierarchical clustering Image (.png)"),
                            h4("Cluster assignments",
                               shinyBS::bsButton("surf-info_PGCA", label = "", icon = icon("info", lib = "font-awesome"), style = "default", size = "extra-small")),
                            shinyBS::bsPopover(id = "surf-info_PGCA", title = "More information",
                                      content = HTML(paste0("<p>Hierarchical clustering generates multiple modules (clusters) to which each proteins/genes are assigned. The table below details the following columns:  </p> <ul> <li> Feature: proteins/genes ID </li>  <li>  module_id: The module where the protein/gene is assigned and the color used on the hierarchical clustering dendrogram </li> <li> Gene_symbol: If annotation data is available, it also shows the gene symbol. </li> </ul> <p> The arrows to the right of each column title can be used for sorting data from increasing or decreasing values. The search bar can also be used to find the details of a protein/gene of interest.</p> <p> The full .csv file of Cluster Assignments for all the proteins/genes features can be downloaded at the bottom of the table. </p>")),
                                      placement = "right", trigger = "hover", options = list(container = "body")),
                            selectInput(
                              ns("Mapping2"),
                              label = "If you uploaded annotation data, select columns to view",
                              choices = NULL,
                              multiple = TRUE
                            ),
                            DT::DTOutput(ns("tableClusterAssig2")),
                            downloadButton(ns("downloadClusterAssig2"),
                                           "Cluster assigment"),
                            h4("First principal component from each module (Eigenfeatures)",
                               shinyBS::bsButton("surf-info_PGPC1ef", label = "", icon = icon("info", lib = "font-awesome"), style = "default", size = "extra-small")),
                            shinyBS::bsPopover(id = "surf-info_PGPC1ef", title = "More information",
                                      content = HTML(paste0("<p> The first principal component (PC1) is calculated for each module, referred to as eigenfeatures. Eigenfeatures are useful for: </p> <ol> <li> Relating the modules to the phenotypes. </li> <li>  Obtaining the correlation between omics datasets (integration). </li> </ol> <p> The full .csv file of calculations for PC1 for data modules can be downloaded at the bottom of the table. </p> <p> The heatmap below shows eigenfeatures across samples. The vertical axis (y-axis) represents the eigenfeatures, and the horizontal axis (x-axis) displays the sample conditions. </p>")),
                                      placement = "right", trigger = "hover", options = list(container = "body") ),
                            DT::DTOutput(ns("tableEigengene2")),
                            downloadButton(ns("downloadtableEigengene2"),
                                           "Eigenfeatures table"),
                            h4("Eigenfeatures heatmap"),
                            plotOutput(ns("heatmapEigenProt")),
                            downloadButton(ns("downloadheatmapEigenProt"),
                                           "Eigenfeatures heatmap Image (.png)"),
                            h4("Cluster assignments enriched",
                               shinyBS::bsButton("surf-info_PGCAE", label = "", icon = icon("info", lib = "font-awesome"), style = "default", size = "extra-small")),
                            shinyBS::bsPopover(id = "surf-info_PGCAE", title = "More information",
                                               content = HTML(paste0("Drop-down menu displays available libraries for pathway analysis. Choose a library to automatically amend the data cluster descriptions on table below. Under column enriched_Term the most highly correlated pathway is displayed and in the following columns, along with enriched_Genes, and p-values as determined by Enrichr. The search bar can also be used to find the details of a protein/gene or module of interest. The full .csv file of Cluster Enrichments for data can be downloaded at the bottom of the table.")),
                                               placement = "right", trigger = "hover", options = list(container = "body")),
                            helpText(
                              "Note: Upload the Annotation data to be able to run enrichment analysis."),
                            selectInput(
                              ns("databaseSelector2"),
                              label = "Select Library",
                              choices = NULL),
                            checkboxInput(ns("runEnrichment2"),  label = "Run Enrichment Analysis", value = FALSE),
                            DT::DTOutput(ns("tableClusterAssigAnnot2")),
                            downloadButton(ns("downloadEnrichment2"),
                                           "Enrichment analysis Data 2")
                   ),
                   tabPanel("Phenotype",
                            h4("Metadata",
                               shinyBS::bsButton("surf-info_2GPData", label = "", icon = icon("info", lib = "font-awesome"), style = "default", size = "extra-small")),
                            shinyBS::bsPopover(id = "surf-info_2GPData", title = "More information",
                                      content = HTML(paste0("Table reflecting the uploaded file <i> Metadata</i>. Check if the number of samples and the number of entries listed at the bottom of the table are the same. The arrows to the right of each column title can be used for sorting data from increasing or decreasing values. The search bar can also be used to confirm the details of a sample.")),
                                      placement = "right", trigger = "hover", options = list(container = "body")),
                            DT::DTOutput(ns("tableM2")),
                            h4("Classification between phenotypes by eigenfeatures",
                               shinyBS::bsButton("surf-info_PGCPef", label = "", icon = icon("info", lib = "font-awesome"), style = "default", size = "extra-small")),
                            shinyBS::bsPopover(id = "surf-info_PGCPef", title = "More information",
                                      content = HTML(paste0("<p>Statistical analysis by Students t-test compares phenotypes chosen from a drop-down menu. The eigenfeatures of each module, determined previously, are used as predictors. The user can also specify a significance threshold for the p-value, with the default set to 0.05. </p> <p> It returns a data frame with the following columns: </p> <ul> <li> Variable: Represents the ID of the module. </li> <li> Class: Lists the two levels of the phenotypes being compared. If there are more than two levels, it compares one level against the others. </li>  <li> Result_t: The t-statistic value. </li> <li> Result_pValue: The p-value for the test. </li> </ul> <p> Boxplots are automatically generated at the bottom for significant eigenfeatures, with dots marking outliers and a legend describing the compared phenotype. </p> ")),
                                      placement = "right", trigger = "hover", options = list(container = "body")),
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
                                                    "Boxplot classification Image (.png)")
                              )
                            ),
                            h4("Module screening",
                               shinyBS::bsButton("surf-infoMS", label = "", icon = icon("info", lib = "font-awesome"), style = "default", size = "extra-small")),
                            shinyBS::bsPopover(id = "surf-infoMS", title = "More information",
                                      content = HTML(paste0("The drop-down menu displays all modules generated by iModMix. Users can view the features within a selected module. If annotation data is available, it also shows the Gene Symbol. PCA loading and heatmap plots are generated to visualize the behavior of each specific module across the phenotype.")),
                                      placement = "right", trigger = "hover", options = list(container = "body")),
                            fluidRow(
                              column(6,
                                     selectInput(ns("moduleSelector2"),
                                                 label = "Select the module of interest",
                                                 choices = NULL,
                                                 selected = NULL)
                              ),
                              column(6,
                              selectInput(
                                ns("Screening2"),
                                label = "If you uploaded annotation data, select columns to view",
                                choices = NULL,
                                multiple = TRUE)
                              )
                            ),
                            DT::DTOutput(ns("ModuleFeaturesAnnot2")),
                            downloadButton(ns("downloadModuleFeaturesAnnot2"),
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

         tabPanel("Data 3",
                 tabsetPanel(
                   type = "tabs",
                   tabPanel("Data Input",
                            h4("Expression data 3",
                               shinyBS::bsButton("surf-info_3GEM", label = "", icon = icon("info", lib = "font-awesome"), style = "default", size = "extra-small")),
                            shinyBS::bsPopover(id = "surf-info_3GEM", title = "More information",
                                      content = HTML(paste0("Table reflecting uploaded file <i>Genomics Expression data</i>. Check if the number of samples and the number of genes are correct. The arrows to the right of each column title can be used for sorting data from increasing or decreasing values. The search bar can also be used to confirm the details of a gene of interest.")),
                                      placement = "right", trigger = "hover", options = list(container = "body") ),
                            DT::DTOutput(ns("infotable3")),
                            DT::DTOutput(ns("table3")),
                            h4("Principal component analysis for each phenotype",
                               shinyBS::bsButton("surf-info_3GPC", label = "", icon = icon("info", lib = "font-awesome"), style = "default", size = "extra-small")),
                            shinyBS::bsPopover(id = "surf-info_3GPC", title = "More information",
                                      content = HTML(paste0("Drop-down menu displays conditions or sample descriptions provided with uploaded <i>Metadata</i>. Graph below displays Principal Component Analysis (PCA) plots representing each of your phenotype descriptions. ")),
                                      placement = "right", trigger = "hover", options = list(container = "body")),
                            selectInput(ns("phenotypeSelectorPCA3"),
                                        label = "Select the phenotype of interest",
                                        choices = NULL,
                                        selected = NULL),
                            plotOutput(ns("PCA3")),
                            downloadButton(ns("downloadPCA3"),
                                           "Principal component analysis"),
                            h4("Annotation data 3",
                               shinyBS::bsButton("surf-info_3GAD", label = "", icon = icon("info", lib = "font-awesome"), style = "default", size = "extra-small")),
                            shinyBS::bsPopover(id = "surf-info_3GAD", title = "More information",
                                      content = HTML(paste0("Table reflecting the uploaded file <i>Genomics Annotation Data</i>. Check if the total number of entries at bottom of table matches the total number of features in the <i>Genomics Expression Data</i>. The arrows to the right of each column title can be used for sorting data from increasing or decreasing values. The search bar can also be used to confirm the details of a protein/gene of interest. ")),
                                      placement = "right", trigger = "hover", options = list(container = "body")),
                            DT::DTOutput(ns("tableAnnot3"))
                   ),
                   tabPanel("Module Assignments",
                            h4("Sparse partial correlations: data 3",
                               shinyBS::bsButton("surf-info_SPCforPG3", label = "", icon = icon("info", lib = "font-awesome"), style = "default", size = "extra-small")),
                            shinyBS::bsPopover(id = "surf-info_SPCforPG3", title = "More information",
                                      content = HTML(paste0("<p>Partial correlation is a method of analyzing the relationship between two variables when other variables are present. Graphical Lasso (Glasso) is used to estimated the partial correlation and captures only direct associations.  </p> <p> Preview of the sparse partial correlation of the first five proteins/genes. The full .csv file for the sparse partial correlation calculations for all the proteins/genes can be downloaded at the bottom of the table. </p>")),
                                      placement = "right", trigger = "hover", options = list(container = "body")
                               ),
                            verbatimTextOutput(ns("matrizTable3")),
                            downloadButton(ns("downloadParCor3"),
                                           "Partial correlation"),
                            h4("Hierarchical clustering",
                               shinyBS::bsButton("surf-info_GHC3", label = "", icon = icon("info", lib = "font-awesome"), style = "default", size = "extra-small")),
                            shinyBS::bsPopover(id = "surf-info_GHC3", title = "More information",
                                      content = HTML(paste0("<p>Hierarchical clustering is used to identify common neighbors between the proteins/genes. Calculations are determined using the topographical overlap matrix (TOM) and based on the sparse partial correlations. Hierarchical clustering is visualized as a dendrogram. </p> <p> Axes: The vertical axis (y-axis) represents the dissimilarity between metabolites or modules, while the horizontal axis (x-axis) shows the modules. </p> <p>Branches: Each line in the dendrogram represents a module. Modules that are closer in the hierarchy (i.e., joined at a lower height in the dendrogram) have more similar expression profiles. </p>")),
                                      placement = "right", trigger = "hover", options = list(container = "body")
                            ),
                            plotOutput(ns("hc_plot3")),
                            downloadButton(ns("downloadhc_plot3"),
                                           "Hierarchical clustering Image (.png)"),
                            h4("Cluster assignments",
                               shinyBS::bsButton("surf-info_GCA3", label = "", icon = icon("info", lib = "font-awesome"), style = "default", size = "extra-small")),
                            shinyBS::bsPopover(id = "surf-info_GCA3", title = "More information",
                                      content = HTML(paste0("<p>Hierarchical clustering generates multiple modules (clusters) to which each proteins/genes are assigned. The table below details the following columns:  </p> <ul> <li> Feature: proteins/genes ID </li>  <li>  module_id: The module where the protein/gene is assigned and the color used on the hierarchical clustering dendrogram </li> <li> Gene_symbol: If annotation data is available, it also shows the gene symbol. </li> </ul> <p> The arrows to the right of each column title can be used for sorting data from increasing or decreasing values. The search bar can also be used to find the details of a protein/gene of interest.</p> <p> The full .csv file of Cluster Assignments for all the proteins/genes features can be downloaded at the bottom of the table. </p>")),
                                      placement = "right", trigger = "hover", options = list(container = "body")),
                            selectInput(
                              ns("Mapping3"),
                              label = "If you uploaded annotation data, select columns to view",
                              choices = NULL,
                              multiple = TRUE
                            ),
                            DT::DTOutput(ns("tableClusterAssig3")),
                            downloadButton(ns("downloadClusterAssig3"),
                                           "Cluster assigment"),
                            h4("First principal component from each module (Eigenfeatures)",
                               shinyBS::bsButton("surf-info_GPC1ef3", label = "", icon = icon("info", lib = "font-awesome"), style = "default", size = "extra-small")),
                            shinyBS::bsPopover(id = "surf-info_GPC1ef3", title = "More information",
                                      content = HTML(paste0("<p> The first principal component (PC1) is calculated for each module, referred to as eigenfeatures. Eigenfeatures are useful for: </p> <ol> <li> Relating the modules to the phenotypes. </li> <li>  Obtaining the correlation between omics datasets (integration). </li> </ol> <p> The full .csv file of calculations for PC1 for genomics modules can be downloaded at the bottom of the table. </p> <p> The heatmap below shows eigenfeatures across samples. The vertical axis (y-axis) represents the eigenfeatures, and the horizontal axis (x-axis) displays the sample conditions. </p>")),
                                      placement = "right", trigger = "hover", options = list(container = "body") ),
                            DT::DTOutput(ns("tableEigengene3")),
                            downloadButton(ns("downloadtableEigengene3"),
                                           "Eigenfeatures table"),
                            h4("Eigenfeatures heatmap"),
                            plotOutput(ns("heatmapEigenGene")),
                            downloadButton(ns("downloadheatmapEigenGene"),
                                           "Eigenfeatures heatmap Image (.png)"),
                            h4("Cluster assignments enriched",
                               shinyBS::bsButton("surf-info_GCAE3", label = "", icon = icon("info", lib = "font-awesome"), style = "default", size = "extra-small")),
                            shinyBS::bsPopover(id = "surf-info_GCAE3", title = "More information",
                                               content = HTML(paste0("Drop-down menu displays available libraries for pathway analysis. Choose a library to automatically amend the Genomics cluster descriptions on table below. Under column enriched_Term the most highly correlated pathway is displayed and in the following columns, along with enriched_Genes, and p-values as determined by Enrichr. The search bar can also be used to find the details of a protein/gene or module of interest. The full .csv file of Cluster Enrichments for Genomics can be downloaded at the bottom of the table.")),
                                               placement = "right", trigger = "hover", options = list(container = "body")),
                            helpText(
                              "Note: Upload the Annotation data to be able to run enrichment analysis."),
                            selectInput(
                              ns("databaseSelector3"),
                              label = "Select Library",
                              choices = NULL),
                            checkboxInput(ns("runEnrichment3"),  label = "Run Enrichment Analysis", value = FALSE),
                            DT::DTOutput(ns("tableClusterAssigAnnot3")),
                            downloadButton(ns("downloadEnrichment3"),
                                           "Enrichment analysis data 3")
                   ),
                   tabPanel("Phenotype",
                            h4("Metadata",
                               shinyBS::bsButton("surf-info_3GPData", label = "", icon = icon("info", lib = "font-awesome"), style = "default", size = "extra-small")),
                            shinyBS::bsPopover(id = "surf-info_3GPData", title = "More information",
                                      content = HTML(paste0("Table reflecting the uploaded file <i> Metadata</i>. Check if the number of samples and the number of entries listed at the bottom of the table are the same. The arrows to the right of each column title can be used for sorting data from increasing or decreasing values. The search bar can also be used to confirm the details of a sample.")),
                                      placement = "right", trigger = "hover", options = list(container = "body")),
                            DT::DTOutput(ns("tableM3")),
                            h4("Classification between phenotypes by eigenfeatures",
                               shinyBS::bsButton("surf-info_3GCPef", label = "", icon = icon("info", lib = "font-awesome"), style = "default", size = "extra-small")),
                            shinyBS::bsPopover(id = "surf-info_3GCPef", title = "More information",
                                      content = HTML(paste0("<p>Statistical analysis by Students t-test compares phenotypes chosen from a drop-down menu. The eigenfeatures of each module, determined previously, are used as predictors. The user can also specify a significance threshold for the p-value, with the default set to 0.05. </p> <p> It returns a data frame with the following columns: </p> <ul> <li> Variable: Represents the ID of the module. </li> <li> Class: Lists the two levels of the phenotypes being compared. If there are more than two levels, it compares one level against the others. </li>  <li> Result_t: The t-statistic value. </li> <li> Result_pValue: The p-value for the test. </li> </ul> <p> Boxplots are automatically generated at the bottom for significant eigenfeatures, with dots marking outliers and a legend describing the compared phenotype. </p> ")),
                                      placement = "right", trigger = "hover", options = list(container = "body")),
                            fluidRow(
                              column(6,
                                     selectInput(ns("phenotypeSelector3"),
                                                 label = "Select the phenotype of interest",
                                                 choices = NULL,
                                                 selected = NULL)
                              ),
                              column(6,
                                     numericInput(ns("pValueThreshold3"),
                                                 label = "Select p-value Threshold",
                                                 min = 0,
                                                 max = 1,
                                                 step = 0.0001,
                                                 value = 0.05)
                              )
                            ),
                            DT::DTOutput(ns("classification_results3")),
                            plotOutput(ns("classification_plot_3_all")),
                            fluidRow(
                              column(6,
                                     downloadButton(ns("downloadClassification_results3"),
                                                    "Classification table")
                              ),
                              column(6,
                                     downloadButton(ns("downloadClassification_plot_3_all"),
                                                    "Boxplot classification Image (.png)")
                              )
                            ),
                            h4("Module screening",
                               shinyBS::bsButton("surf-infoMS3", label = "", icon = icon("info", lib = "font-awesome"), style = "default", size = "extra-small")),
                            shinyBS::bsPopover(id = "surf-infoMS3", title = "More information",
                                      content = HTML(paste0("The drop-down menu displays all modules generated by iModMix. Users can view the features within a selected module. If annotation data is available, it also shows the Gene Symbol. PCA loading and heatmap plots are generated to visualize the behavior of each specific module across the phenotype.")),
                                      placement = "right", trigger = "hover", options = list(container = "body")),
                            fluidRow(
                              column(6,
                                     selectInput(ns("moduleSelector3"),
                                                 label = "Select the module of interest",
                                                 choices = NULL,
                                                 selected = NULL)
                              ),
                              column(6,
                              selectInput(
                                ns("Screening3"),
                                label = "If you uploaded annotation data, select columns to view",
                                choices = NULL,
                                multiple = TRUE)
                              )
                            ),
                            DT::DTOutput(ns("ModuleFeaturesAnnot3")),
                            downloadButton(ns("downloadModuleFeaturesAnnot3"),
                                           "Module Features"),
                            h4("PCA loading by module"),
                            plotOutput(ns("Loadings3")),
                            downloadButton(ns("downloadLoadings3"),
                                           "PCA_Loadings"),
                            h4("Heatmap by module"),
                            plotOutput(ns("heatmap3")),
                            downloadButton(ns("downloadHeatmap3"),
                                           "Heatmap")
                   )
                 )
                 ),

        tabPanel("Multi-omics Analysis",
                 tabsetPanel(
                   type = "tabs",
                   tabPanel("Modules Integration",
                            h4("Correlation between datasets",
                               shinyBS::bsButton("surf-infoMC", label = "", icon = icon("info", lib = "font-awesome"), style = "default", size = "extra-small")),
                            shinyBS::bsPopover(id = "surf-infoMC", title = "More information",
                                      content = HTML(paste0("Histogram depicting the correlation between datasets using Spearman correlation")),
                                      placement = "right", trigger = "hover", options = list(container = "body")),
                            plotOutput(ns("Correlation_plot12")),
                            numericInput(ns("pValueThresholdcor"),
                                        label = "Select correlation threshold",
                                        min = 0,
                                        max = 1,
                                        step = 0.0001,
                                        value = 0.5),
                            DT::DTOutput(ns("tableCorrelation")),
                            downloadButton(ns("downloadOmicsCorrelation"),
                                           "Omics correlation"),
                            h4("Module network between datasets",
                               shinyBS::bsButton("surf-info_MMPG", label = "", icon = icon("info", lib = "font-awesome"), style = "default", size = "extra-small")),
                            shinyBS::bsPopover(id = "surf-info_MMPG", title = "More information",
                                      content = HTML(paste0("An interactive module network showing each Data2 module as a green triangle  and Data1 modules as a yellow diamond. Clicking directly on the triangle or diamond identifies the module number. Correlation coefficients are seen on arrows connecting modules. Modules can be fluidly switched into different order and moved on the screen. The network can be downloaded as an html for saving. ")),
                                      placement = "right", trigger = "hover", options = list(container = "body")),
                            #plotOutput(ns("Network_plot")),
                            visNetwork::visNetworkOutput(ns("network")),
                            downloadLink(ns("downloadNetwork"),
                                         "Network as .html")),
                   tabPanel("Data 1 - Data 2",
                            h4("Top modules correlations",
                               shinyBS::bsButton("surf-infoT5MM", label = "", icon = icon("info", lib = "font-awesome"), style = "default", size = "extra-small")),
                            shinyBS::bsPopover(id = "surf-infoT5MM", title = "More information",
                                      content = HTML(paste0("Table of the top 5 highly correlated modules, with the number of features within each module, the correlation between modules, and the enriched term for the proteins modules.")),
                                      placement = "right", trigger = "hover", options = list(container = "body")),
                            numericInput(ns("TopModules_12"),
                                         label = "Select the number of correlated modules to display",
                                         min = 1,
                                         max = 50,
                                         step = 1,
                                         value = 5),
                            DT::DTOutput(ns("ImportantVariables_12")),
                            h4("Top module correlation details",
                               shinyBS::bsButton("surf-infoTMCD", label = "", icon = icon("info", lib = "font-awesome"), style = "default", size = "extra-small")),
                            shinyBS::bsPopover(id = "surf-infoTMCD", title = "More information",
                                      content = HTML(paste0("The drop-down menu displays the details for each of the top 5 highly correlated modules. Select one option to see the features within each module and the correlation between each feature (Corrplot and table). The arrows to the right of each column title can be used to sort data from increasing or decreasing values. Users can also use the search bar to find the details of a protein/gene or metabolites of interest. The user can download the full .csv file of Module correlations at the bottom of the table. ")),
                                      placement = "right", trigger = "hover", options = list(container = "body")),
                            selectInput(ns("visualization_list_12"),
                                        label = "Select the correlated modules to display",
                                        choices = c("Top_1" = 1,
                                                    "Top_2" = 2,
                                                    "Top_3" = 3,
                                                    "Top_4" = 4,
                                                    "Top_5" = 5)),
                            h4("Features from data 1",
                               shinyBS::bsButton("surf-info_LofMet", label = "", icon = icon("info", lib = "font-awesome"), style = "default", size = "extra-small")),
                            shinyBS::bsPopover(id = "surf-info_LofMet", title = "More information",
                                      content = HTML(paste0("<p> Displays the list of features within a Data 1 module that is highly correlated with a Data 2 module. The Data 1 module ID is specified first, followed by the list of constituent metabolites. This information facilitates further pathway analysis and provides valuable insights into the relationships between metabolites and proteins. </p> <p> The full .csv file of the list of metabolites to perform pathway analysis further can be downloaded at the bottom of the table. </p>")),
                                      placement = "right", trigger = "hover", options = list(container = "body")

                            ),
                            verbatimTextOutput(ns("Important_features_12_1")),

                            h4("Features from data 2",
                               shinyBS::bsButton("surf-info_LofPG", label = "", icon = icon("info", lib = "font-awesome"), style = "default", size = "extra-small")),
                            shinyBS::bsPopover(id = "surf-info_LofPG", title = "More information",
                                      content = HTML(paste0("Displays the list of features within a Data 2 module that is highly correlated with a Data 1 module. The module ID is first specified, along with its enriched term, enriched genes, and p-value. This is followed by the list of constituent proteins/genes. This information enables further pathway analysis and provides valuable insights into the relationships between metabolites, proteins, and genes.")),
                                      placement = "right", trigger = "hover", options = list(container = "body")

                            ),
                            verbatimTextOutput(ns("Important_features_12_2")),

                            h4("Corrplot: Data 1 and Data 2"),
                            plotOutput(ns("CorplotImp12")),

                            h4("Modules correlation: Data 1 and Data 2",
                               shinyBS::bsButton("surf-infoMCMPG", label = "", icon = icon("info", lib = "font-awesome"), style = "default", size = "extra-small")),
                            shinyBS::bsPopover(id = "surf-infoMCMPG", title = "More information",
                                      content = HTML(paste0("Displays the list of features within a Data 2 module that is highly correlated with a Data 1 module. The module ID is first specified, along with its enriched term, enriched genes, and p-value. This is followed by the list of constituent proteins/genes. This information enables further pathway analysis and provides valuable insights into the relationships between metabolites, proteins, and genes.")),
                                      placement = "right", trigger = "hover", options = list(container = "body")),
                            DT::DTOutput(ns("Correlation_mod12")),
                            downloadButton(ns("downloadModCorrelation12"),
                                           "Modules_1-2_correlation"),

                            h4("Data 1: Features from top module",
                               shinyBS::bsButton("surf-infoMTopM", label = "", icon = icon("info", lib = "font-awesome"), style = "default", size = "extra-small")),
                            shinyBS::bsPopover(id = "surf-infoMTopM", title = "More information",
                                      content = HTML(paste0("<p> Displays the list of features within a Data 1 module that is highly correlated with a Data 2 module. The Data 1 module ID is specified first, followed by the list of constituent metabolites. This information facilitates further pathway analysis and provides valuable insights into the relationships between metabolites and proteins. </p> <p> The full .csv file of the list of metabolites to perform pathway analysis further can be downloaded at the bottom of the table. </p>")),
                                      placement = "right", trigger = "hover", options = list(container = "body")),
                            DT::DTOutput(ns("cluster_assignments_summary12_1")),
                            selectInput(
                              ns("Screening12_1"),
                              label = "If you uploaded annotation data, select columns to view",
                              choices = NULL,
                              multiple = TRUE),
                            DT::DTOutput(ns("cluster_assignments_features12_1")),
                            downloadButton(ns("downloadcluster_assignments12_1"),
                                           "Data1_Features_TopModule12"),

                            h4("Classification between phenotypes by Data 1",
                               shinyBS::bsButton("surf-infoMCPM", label = "", icon = icon("info", lib = "font-awesome"), style = "default", size = "extra-small")),
                            shinyBS::bsPopover(id = "surf-infoMCPM", title = "More information",
                                      content = HTML(paste0("<p>Statistical analysis by Students t-test compares phenotypes chosen from a drop-down menu. The feactures of top correlated module, determined previously, are used as predictors. The user can also specify a significance threshold for the p-value, with the default set to 0.05. </p> <p> It returns a boxplot </p>  <li> Class: Lists the two levels of the phenotypes being compared. If there are more than two levels, it compares one level against the others. </li> </li> </ul> <p> Dots marking outliers and a legend describing the compared phenotype. </p> ")),
                                      placement = "right", trigger = "hover", options = list(container = "body")),
                            fluidRow(
                              column(6,
                                     selectInput(ns("phenotypeSelector_imp_12_1"),
                                                 label = "Select the phenotype of interest",
                                                 choices = NULL,
                                                 selected = NULL)
                              ),
                              column(6,
                                     numericInput(ns("pValueThreshold_imp_12_1"),
                                                  label = "Select p-value Threshold",
                                                  min = 0,
                                                  max = 1,
                                                  step = 0.001,
                                                  value = 0.05)
                              )
                            ),
                            DT::DTOutput(ns("classification_results_imp_12_1")),
                            plotOutput(ns("classification_plot_1_all_imp_12_1")),

                            h4("Data 2: Features from top module"),
                            DT::DTOutput(ns("cluster_assignments_summary12_2")),
                            selectInput(
                              ns("Screening12_2"),
                              label = "If you uploaded annotation data, select columns to view",
                              choices = NULL,
                              multiple = TRUE),
                            DT::DTOutput(ns("cluster_assignments_features12_2")),
                            downloadButton(ns("downloadcluster_assignments12_2"),
                                           "Data2_Features_TopModule12"),


                            h4("Classification between phenotypes by Data 2",
                               shinyBS::bsButton("surf-infoMCPPG", label = "", icon = icon("info", lib = "font-awesome"), style = "default", size = "extra-small")),
                            shinyBS::bsPopover(id = "surf-infoMCPPG", title = "More information",
                                      content = HTML(paste0("<p>Statistical analysis by Students t-test compares phenotypes chosen from a drop-down menu. The feactures of top correlated module, determined previously, are used as predictors. The user can also specify a significance threshold for the p-value, with the default set to 0.05. </p> <p> It returns a boxplot </p>  <li> Class: Lists the two levels of the phenotypes being compared. If there are more than two levels, it compares one level against the others. </li> </li> </ul> <p> Dots marking outliers and a legend describing the compared phenotype. </p> ")),
                                      placement = "right", trigger = "hover", options = list(container = "body")),
                            fluidRow(
                              column(6,
                                     selectInput(ns("phenotypeSelector_imp_12_2"),
                                                 label = "Select the phenotype of interest",
                                                 choices = NULL,
                                                 selected = NULL)
                              ),
                              column(6,
                                     numericInput(ns("pValueThreshold_imp_12_2"),
                                                  label = "Select p-value Threshold",
                                                  min = 0,
                                                  max = 1,
                                                  step = 0.001,
                                                  value = 0.05)
                              )
                            ),
                            DT::DTOutput(ns("classification_results_imp_12_2")),
                            plotOutput(ns("classification_plot_1_all_imp_12_2"))

                            ),
                   tabPanel("Data 1 - Data 3",
                            h4("Top modules correlations",
                               shinyBS::bsButton("surf-infoT5MM2", label = "", icon = icon("info", lib = "font-awesome"), style = "default", size = "extra-small")),
                            shinyBS::bsPopover(id = "surf-infoT5MM2", title = "More information",
                                               content = HTML(paste0("Table of the top 5 highly correlated modules, with the number of features within each module, the correlation between modules, and the enriched term for the proteins modules.")),
                                               placement = "right", trigger = "hover", options = list(container = "body")),
                            numericInput(ns("TopModules_13"),
                                         label = "Select the number of correlated modules to display",
                                         min = 1,
                                         max = 50,
                                         step = 1,
                                         value = 5),
                            DT::DTOutput(ns("ImportantVariables_13")),
                            h4("Top module correlation details",
                               shinyBS::bsButton("surf-infoTMCD2", label = "", icon = icon("info", lib = "font-awesome"), style = "default", size = "extra-small")),
                            shinyBS::bsPopover(id = "surf-infoTMCD2", title = "More information",
                                               content = HTML(paste0("The drop-down menu displays the details for each of the top 5 highly correlated modules. Select one option to see the features within each module and the correlation between each feature (Corrplot and table). The arrows to the right of each column title can be used to sort data from increasing or decreasing values. Users can also use the search bar to find the details of a protein/gene or metabolites of interest. The user can download the full .csv file of Module correlations at the bottom of the table. ")),
                                               placement = "right", trigger = "hover", options = list(container = "body")),
                            selectInput(ns("visualization_list_13"),
                                        label = "Select the correlated modules to display",
                                        choices = c("Top_1" = 1,
                                                    "Top_2" = 2,
                                                    "Top_3" = 3,
                                                    "Top_4" = 4,
                                                    "Top_5" = 5)),
                            h4("Features from data 1",
                               shinyBS::bsButton("surf-info_LofMet2", label = "", icon = icon("info", lib = "font-awesome"), style = "default", size = "extra-small")),
                            shinyBS::bsPopover(id = "surf-info_LofMet2", title = "More information",
                                               content = HTML(paste0("<p> Displays the list of features within a Data 1 module that is highly correlated with a Data 2 module. The Data 1 module ID is specified first, followed by the list of constituent metabolites. This information facilitates further pathway analysis and provides valuable insights into the relationships between metabolites and proteins. </p> <p> The full .csv file of the list of metabolites to perform pathway analysis further can be downloaded at the bottom of the table. </p>")),
                                               placement = "right", trigger = "hover", options = list(container = "body")

                            ),
                            verbatimTextOutput(ns("Important_features_13_1")),

                            h4("Features from data 3",
                               shinyBS::bsButton("surf-info_LofPG2", label = "", icon = icon("info", lib = "font-awesome"), style = "default", size = "extra-small")),
                            shinyBS::bsPopover(id = "surf-info_LofPG2", title = "More information",
                                               content = HTML(paste0("Displays the list of features within a Data 2 module that is highly correlated with a Data 1 module. The module ID is first specified, along with its enriched term, enriched genes, and p-value. This is followed by the list of constituent proteins/genes. This information enables further pathway analysis and provides valuable insights into the relationships between metabolites, proteins, and genes.")),
                                               placement = "right", trigger = "hover", options = list(container = "body")

                            ),
                            verbatimTextOutput(ns("Important_features_13_3")),

                            h4("Corrplot: Data 1 and Data 3"),
                            plotOutput(ns("CorplotImp13")),

                            h4("Modules correlation: Data 1 and Data 3",
                               shinyBS::bsButton("surf-infoMCMPG2", label = "", icon = icon("info", lib = "font-awesome"), style = "default", size = "extra-small")),
                            shinyBS::bsPopover(id = "surf-infoMCMPG2", title = "More information",
                                               content = HTML(paste0("Displays the list of features within a Data 2 module that is highly correlated with a Data 1 module. The module ID is first specified, along with its enriched term, enriched genes, and p-value. This is followed by the list of constituent proteins/genes. This information enables further pathway analysis and provides valuable insights into the relationships between metabolites, proteins, and genes.")),
                                               placement = "right", trigger = "hover", options = list(container = "body")),
                            DT::DTOutput(ns("Correlation_mod13")),
                            downloadButton(ns("downloadModCorrelation13"),
                                           "Modules_1-3_correlation"),

                            h4("Data 1: Features from top module",
                               shinyBS::bsButton("surf-infoMTopM2", label = "", icon = icon("info", lib = "font-awesome"), style = "default", size = "extra-small")),
                            shinyBS::bsPopover(id = "surf-infoMTopM2", title = "More information",
                                               content = HTML(paste0("<p> Displays the list of features within a Data 1 module that is highly correlated with a Data 2 module. The Data 1 module ID is specified first, followed by the list of constituent metabolites. This information facilitates further pathway analysis and provides valuable insights into the relationships between metabolites and proteins. </p> <p> The full .csv file of the list of metabolites to perform pathway analysis further can be downloaded at the bottom of the table. </p>")),
                                               placement = "right", trigger = "hover", options = list(container = "body")),
                            DT::DTOutput(ns("cluster_assignments_summary13_1")),
                            selectInput(
                              ns("Screening13_1"),
                              label = "If you uploaded annotation data, select columns to view",
                              choices = NULL,
                              multiple = TRUE),
                            DT::DTOutput(ns("cluster_assignments_features13_1")),
                            downloadButton(ns("downloadcluster_assignments13_1"),
                                           "Data1_Features_TopModule13"),

                            h4("Classification between phenotypes by Data 1",
                               shinyBS::bsButton("surf-infoMCPM2", label = "", icon = icon("info", lib = "font-awesome"), style = "default", size = "extra-small")),
                            shinyBS::bsPopover(id = "surf-infoMCPM2", title = "More information",
                                               content = HTML(paste0("<p>Statistical analysis by Students t-test compares phenotypes chosen from a drop-down menu. The feactures of top correlated module, determined previously, are used as predictors. The user can also specify a significance threshold for the p-value, with the default set to 0.05. </p> <p> It returns a boxplot </p>  <li> Class: Lists the two levels of the phenotypes being compared. If there are more than two levels, it compares one level against the others. </li> </li> </ul> <p> Dots marking outliers and a legend describing the compared phenotype. </p> ")),
                                               placement = "right", trigger = "hover", options = list(container = "body")),
                            fluidRow(
                              column(6,
                                     selectInput(ns("phenotypeSelector_imp_13_1"),
                                                 label = "Select the phenotype of interest",
                                                 choices = NULL,
                                                 selected = NULL)
                              ),
                              column(6,
                                     numericInput(ns("pValueThreshold_imp_13_1"),
                                                  label = "Select p-value Threshold",
                                                  min = 0,
                                                  max = 1,
                                                  step = 0.001,
                                                  value = 0.05)
                              )
                            ),
                            DT::DTOutput(ns("classification_results_imp_13_1")),
                            plotOutput(ns("classification_plot_1_all_imp_13_1")),

                            h4("Data 3: Features from top module"),
                            DT::DTOutput(ns("cluster_assignments_summary13_3")),
                            selectInput(
                              ns("Screening13_3"),
                              label = "If you uploaded annotation data, select columns to view",
                              choices = NULL,
                              multiple = TRUE),
                            DT::DTOutput(ns("cluster_assignments_features13_3")),
                            downloadButton(ns("downloadcluster_assignments13_3"),
                                           "Data3_Features_TopModule13"),


                            h4("Classification between phenotypes by Data 3",
                               shinyBS::bsButton("surf-infoMCPPG2", label = "", icon = icon("info", lib = "font-awesome"), style = "default", size = "extra-small")),
                            shinyBS::bsPopover(id = "surf-infoMCPPG2", title = "More information",
                                               content = HTML(paste0("<p>Statistical analysis by Students t-test compares phenotypes chosen from a drop-down menu. The feactures of top correlated module, determined previously, are used as predictors. The user can also specify a significance threshold for the p-value, with the default set to 0.05. </p> <p> It returns a boxplot </p>  <li> Class: Lists the two levels of the phenotypes being compared. If there are more than two levels, it compares one level against the others. </li> </li> </ul> <p> Dots marking outliers and a legend describing the compared phenotype. </p> ")),
                                               placement = "right", trigger = "hover", options = list(container = "body")),
                            fluidRow(
                              column(6,
                                     selectInput(ns("phenotypeSelector_imp_13_3"),
                                                 label = "Select the phenotype of interest",
                                                 choices = NULL,
                                                 selected = NULL)
                              ),
                              column(6,
                                     numericInput(ns("pValueThreshold_imp_13_3"),
                                                  label = "Select p-value Threshold",
                                                  min = 0,
                                                  max = 1,
                                                  step = 0.001,
                                                  value = 0.05)
                              )
                            ),
                            DT::DTOutput(ns("classification_results_imp_13_3")),
                            plotOutput(ns("classification_plot_1_all_imp_13_3"))

                   ),
                   tabPanel("Data 2 - Data 3",
                            h4("Top modules correlations",
                               shinyBS::bsButton("surf-infoT5MM3", label = "", icon = icon("info", lib = "font-awesome"), style = "default", size = "extra-small")),
                            shinyBS::bsPopover(id = "surf-infoT5MM3", title = "More information",
                                               content = HTML(paste0("Table of the top 5 highly correlated modules, with the number of features within each module, the correlation between modules, and the enriched term for the proteins modules.")),
                                               placement = "right", trigger = "hover", options = list(container = "body")),
                            numericInput(ns("TopModules_23"),
                                         label = "Select the number of correlated modules to display",
                                         min = 1,
                                         max = 50,
                                         step = 1,
                                         value = 5),
                            DT::DTOutput(ns("ImportantVariables_23")),
                            h4("Top module correlation details",
                               shinyBS::bsButton("surf-infoTMCD3", label = "", icon = icon("info", lib = "font-awesome"), style = "default", size = "extra-small")),
                            shinyBS::bsPopover(id = "surf-infoTMCD3", title = "More information",
                                               content = HTML(paste0("The drop-down menu displays the details for each of the top 5 highly correlated modules. Select one option to see the features within each module and the correlation between each feature (Corrplot and table). The arrows to the right of each column title can be used to sort data from increasing or decreasing values. Users can also use the search bar to find the details of a protein/gene or metabolites of interest. The user can download the full .csv file of Module correlations at the bottom of the table. ")),
                                               placement = "right", trigger = "hover", options = list(container = "body")),
                            selectInput(ns("visualization_list_23"),
                                        label = "Select the correlated modules to display",
                                        choices = c("Top_1" = 1,
                                                    "Top_2" = 2,
                                                    "Top_3" = 3,
                                                    "Top_4" = 4,
                                                    "Top_5" = 5)),
                            h4("Features from data 2",
                               shinyBS::bsButton("surf-info_LofMet3", label = "", icon = icon("info", lib = "font-awesome"), style = "default", size = "extra-small")),
                            shinyBS::bsPopover(id = "surf-info_LofMet3", title = "More information",
                                               content = HTML(paste0("<p> Displays the list of features within a Data 1 module that is highly correlated with a Data 2 module. The Data 1 module ID is specified first, followed by the list of constituent metabolites. This information facilitates further pathway analysis and provides valuable insights into the relationships between metabolites and proteins. </p> <p> The full .csv file of the list of metabolites to perform pathway analysis further can be downloaded at the bottom of the table. </p>")),
                                               placement = "right", trigger = "hover", options = list(container = "body")

                            ),
                            verbatimTextOutput(ns("Important_features_23_2")),

                            h4("Features from data 3",
                               shinyBS::bsButton("surf-info_LofPG3", label = "", icon = icon("info", lib = "font-awesome"), style = "default", size = "extra-small")),
                            shinyBS::bsPopover(id = "surf-info_LofPG", title = "More information",
                                               content = HTML(paste0("Displays the list of features within a Data 2 module that is highly correlated with a Data 1 module. The module ID is first specified, along with its enriched term, enriched genes, and p-value. This is followed by the list of constituent proteins/genes. This information enables further pathway analysis and provides valuable insights into the relationships between metabolites, proteins, and genes.")),
                                               placement = "right", trigger = "hover", options = list(container = "body")

                            ),
                            verbatimTextOutput(ns("Important_features_23_3")),

                            h4("Corrplot: Data 2 and Data 3"),
                            plotOutput(ns("CorplotImp23")),

                            h4("Modules correlation: Data 2 and Data 3",
                               shinyBS::bsButton("surf-infoMCMPG3", label = "", icon = icon("info", lib = "font-awesome"), style = "default", size = "extra-small")),
                            shinyBS::bsPopover(id = "surf-infoMCMPG3", title = "More information",
                                               content = HTML(paste0("Displays the list of features within a Data 2 module that is highly correlated with a Data 1 module. The module ID is first specified, along with its enriched term, enriched genes, and p-value. This is followed by the list of constituent proteins/genes. This information enables further pathway analysis and provides valuable insights into the relationships between metabolites, proteins, and genes.")),
                                               placement = "right", trigger = "hover", options = list(container = "body")),
                            DT::DTOutput(ns("Correlation_mod23")),
                            downloadButton(ns("downloadModCorrelation23"),
                                           "Modules_2-3_correlation"),

                            h4("Data 2: Features from top module",
                               shinyBS::bsButton("surf-infoMTopM3", label = "", icon = icon("info", lib = "font-awesome"), style = "default", size = "extra-small")),
                            shinyBS::bsPopover(id = "surf-infoMTopM3", title = "More information",
                                               content = HTML(paste0("<p> Displays the list of features within a Data 1 module that is highly correlated with a Data 2 module. The Data 1 module ID is specified first, followed by the list of constituent metabolites. This information facilitates further pathway analysis and provides valuable insights into the relationships between metabolites and proteins. </p> <p> The full .csv file of the list of metabolites to perform pathway analysis further can be downloaded at the bottom of the table. </p>")),
                                               placement = "right", trigger = "hover", options = list(container = "body")),
                            DT::DTOutput(ns("cluster_assignments_summary23_2")),
                            selectInput(
                              ns("Screening23_2"),
                              label = "If you uploaded annotation data, select columns to view",
                              choices = NULL,
                              multiple = TRUE),
                            DT::DTOutput(ns("cluster_assignments_features23_2")),
                            downloadButton(ns("downloadcluster_assignments23_2"),
                                           "Data2_Features_TopModule23"),

                            h4("Classification between phenotypes by Data 2",
                               shinyBS::bsButton("surf-infoMCPM3", label = "", icon = icon("info", lib = "font-awesome"), style = "default", size = "extra-small")),
                            shinyBS::bsPopover(id = "surf-infoMCPM3", title = "More information",
                                               content = HTML(paste0("<p>Statistical analysis by Students t-test compares phenotypes chosen from a drop-down menu. The feactures of top correlated module, determined previously, are used as predictors. The user can also specify a significance threshold for the p-value, with the default set to 0.05. </p> <p> It returns a boxplot </p>  <li> Class: Lists the two levels of the phenotypes being compared. If there are more than two levels, it compares one level against the others. </li> </li> </ul> <p> Dots marking outliers and a legend describing the compared phenotype. </p> ")),
                                               placement = "right", trigger = "hover", options = list(container = "body")),
                            fluidRow(
                              column(6,
                                     selectInput(ns("phenotypeSelector_imp_23_2"),
                                                 label = "Select the phenotype of interest",
                                                 choices = NULL,
                                                 selected = NULL)
                              ),
                              column(6,
                                     numericInput(ns("pValueThreshold_imp_23_2"),
                                                  label = "Select p-value Threshold",
                                                  min = 0,
                                                  max = 1,
                                                  step = 0.001,
                                                  value = 0.05)
                              )
                            ),
                            DT::DTOutput(ns("classification_results_imp_23_2")),
                            plotOutput(ns("classification_plot_1_all_imp_23_2")),

                            h4("Data 3: Features from top module"),
                            DT::DTOutput(ns("cluster_assignments_summary23_3")),
                            selectInput(
                              ns("Screening23_3"),
                              label = "If you uploaded annotation data, select columns to view",
                              choices = NULL,
                              multiple = TRUE),
                            DT::DTOutput(ns("cluster_assignments_features23_3")),
                            downloadButton(ns("downloadcluster_assignments23_3"),
                                           "Data3_Features_TopModule23"),


                            h4("Classification between phenotypes by Data 3",
                               shinyBS::bsButton("surf-infoMCPPG3", label = "", icon = icon("info", lib = "font-awesome"), style = "default", size = "extra-small")),
                            shinyBS::bsPopover(id = "surf-infoMCPPG3", title = "More information",
                                               content = HTML(paste0("<p>Statistical analysis by Students t-test compares phenotypes chosen from a drop-down menu. The feactures of top correlated module, determined previously, are used as predictors. The user can also specify a significance threshold for the p-value, with the default set to 0.05. </p> <p> It returns a boxplot </p>  <li> Class: Lists the two levels of the phenotypes being compared. If there are more than two levels, it compares one level against the others. </li> </li> </ul> <p> Dots marking outliers and a legend describing the compared phenotype. </p> ")),
                                               placement = "right", trigger = "hover", options = list(container = "body")),
                            fluidRow(
                              column(6,
                                     selectInput(ns("phenotypeSelector_imp_23_3"),
                                                 label = "Select the phenotype of interest",
                                                 choices = NULL,
                                                 selected = NULL)
                              ),
                              column(6,
                                     numericInput(ns("pValueThreshold_imp_23_3"),
                                                  label = "Select p-value Threshold",
                                                  min = 0,
                                                  max = 1,
                                                  step = 0.001,
                                                  value = 0.05)
                              )
                            ),
                            DT::DTOutput(ns("classification_results_imp_23_3")),
                            plotOutput(ns("classification_plot_1_all_imp_23_3"))

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

    # Variables
    Metab_exp <- reactiveVal(NULL)
    Metab_annot <- reactiveVal(NULL)
    metadata <- reactiveVal(NULL)

    Prot_exp <- reactiveVal(NULL)
    Prot_annot <- reactiveVal(NULL)

    Gene_exp <- reactiveVal(NULL)
    Gene_annot <- reactiveVal(NULL)

    classification_Metab <- reactiveVal()
    classification_Metab_imp <- reactiveVal()
    classification_Prot <- reactiveVal()
    classification_Prot_imp <- reactiveVal()
    classification_Gene <- reactiveVal()
    classification_Gene_imp <- reactiveVal()

    demo_par_cor_Metab <- reactiveVal(NULL)
    demo_par_cor_Prot <- reactiveVal(NULL)
    demo_enrich_Prot <- reactiveVal(NULL)

    demo_par_cor_Metab_All <- reactiveVal(NULL)
    demo_par_cor_Prot_All <- reactiveVal(NULL)

    demo_enrich_Prot_All <- reactiveVal(NULL)


    ###### Data1
    # Data Input

    observeEvent(input$Data1, {
      req(input$Data1)
      filedata_value <- read.csv(input$Data1$datapath)
      Metab_exp(filedata_value)
    })

    observeEvent(input$PhenoData1, {
      req(input$PhenoData1)
      filedata_value <- read.csv(input$PhenoData1$datapath)
      Metab_annot(filedata_value)
    })

    data_info <- reactive({
      req(Metab_exp())
      Nobservations <- nrow(Metab_exp())
      Ncells <- ncol(Metab_exp())-1
      SummaryData <- as.data.frame(list(Number = c(Ncells, Nobservations)))
      rownames(SummaryData) <- c("Samples", "Features")
      list(SummaryData = SummaryData)
    })
    output$infotable <- DT::renderDataTable({
      df <- data_info()$SummaryData
      DT::datatable(df)
    })
    output$table <- DT::renderDataTable({
      df <- Metab_exp()
      df[] <- lapply(df, function(x) if(is.numeric(x)) round(x, digits = 2) else x)
      DT::datatable(df)
    })

    variables_mapping1 <- reactive({
      names(Metab_annot())[-which(names(Metab_annot()) == "Feature_ID")]
    })

    observe({
      updateSelectInput(session, "Mapping1", choices = variables_mapping1())
    })

    pheno_variablesPCA <- reactive({
      names(metadata())[-which(names(metadata()) == "Sample")]
    })

    observe({
      updateSelectInput(session, "phenotypeSelectorPCA", choices = pheno_variablesPCA())
    })

    variables_Screening1 <- reactive({
      names(Metab_annot())[-which(names(Metab_annot()) == "Feature_ID")]
    })

    observe({
      updateSelectInput(session, "Screening1", choices = variables_Screening1())
    })

    load_data1 <- reactive({
      req(Metab_exp())
      Expression_mat = Metab_exp()
      feature_mat_t_imp_data = load_data(Expression_mat = Expression_mat)
      return(list(feature_mat_t_imp_data = feature_mat_t_imp_data))
    })


    pca1 <- reactive({
      pca_res <- prcomp(load_data1()$feature_mat_t_imp_data)
      return(list(pca_res = pca_res))
    })

    output$PCA1 <- renderPlot({
      requireNamespace("ggplot2", quietly = TRUE)
      requireNamespace("ggfortify", quietly = TRUE)
      if(is.null(metadata())){
        ggplot2::autoplot(pca1()$pca_res)
      } else {
        req(metadata())

        pca_data <- as.data.frame(pca1()$pca_res$x)
        combined_data <- cbind(pca_data, metadata())
        color_column <- input$phenotypeSelectorPCA

        if (!is.null(color_column) && color_column != "") {
          ggplot2::ggplot(combined_data, ggplot2::aes_string(x = "PC1", y = "PC2", color = color_column)) +
            ggplot2::geom_point()
        } else {
          showNotification("Please select a phenotype to color the PCA plot.", type = "message")
          return(NULL)
        }
      }
    })

    # Render the download handler
    output$downloadPCA <- downloadHandler(
      filename = function() {
        "PCA_Data1.png"
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


    output$tableAnnot1 <- DT::renderDataTable({
      df <- Metab_annot()
      DT::datatable(df)
    })

    # Module Assigments
    partial_cors1 <- reactive({
      withProgress(message = 'Calculating partial correlations Data 1...', value = 0, {
        if (is.null(demo_par_cor_Metab()) && is.null(demo_par_cor_Metab_All())) {
          req(load_data1()$feature_mat_t_imp_data)
          load_data1 = load_data1()$feature_mat_t_imp_data
          Sys.sleep(5)
          par_cor1 <- partial_cors(load_data = load_data1, rho = .25)
        } else if (demo_par_cor_Metab()) {
          Sys.sleep(5)
          par_cor1 <- readRDS(file.path(here::here(),"/inst/Example_data/ccRCC4_Data", "PartialCorMetabolites.rds"))
        } else if (demo_par_cor_Metab_All()) {
          Sys.sleep(5)
          #par_cor1 <- iModMixData::loadPartialCorMetabolites()
          par_cor1 <- readRDS(file.path(here::here(),"/inst/Example_data/FloresData_K_TK", "PartialCorMetabolites.rds"))
        }
        incProgress(100, detail = 'Complete!')
        list(par_cor1 = par_cor1)
      })
    })

    output$matrizTable <- renderPrint({
      as.matrix(partial_cors1()$par_cor1[seq_len(5), seq_len(5)])
    })

    # Render the download handler
    output$downloadParCor <- downloadHandler(
      filename = function() {
        "PartialCorData1.csv"
      },
      content = function(file) {
        write.csv(partial_cors1()$par_cor1, file, row.names = TRUE)
      }
    )

    hierarchical_cluster1 <- reactive({
      par_cor = as.matrix(partial_cors1()$par_cor1)
      hc = hierarchical_cluster(parcor_mat = par_cor, tom = TRUE, min_module_size = 10)
      hclusterTree = hc$hclustTree
      hcDynMods = hc$dynamicMods_numeric
      hcCluster_assignments = hc$cluster_assignments
      return(list(hclusterTree = hclusterTree, hcDynMods = hcDynMods, hcCluster_assignments = hcCluster_assignments))
    })

    unique_variables <- reactive({
      unique(hierarchical_cluster1()$hcCluster_assignments$col)
    })

    observe({
      updateSelectInput(session, "moduleSelector", choices = unique_variables())
    })

    output$hc_plot <- renderPlot({
      hcClu = hierarchical_cluster1()$hclusterTree
      hcMod = as.matrix(hierarchical_cluster1()$hcDynMods)
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
        hcClu = hierarchical_cluster1()$hclusterTree
        hcMod = hierarchical_cluster1()$hcDynMods
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

    cluster_assignments_Data1 <- reactive({
      cluster = as.data.frame(hierarchical_cluster1()$hcCluster_assignments)
      if (is.null(Metab_annot())) {
        cluster_assignments_D1 <- cluster_assignments(cluster = cluster, PhenoData = NULL, selected_columns = NULL)
      } else {
        Metab_annot = Metab_annot()
        annot_Uni <- Metab_annot[Metab_annot$Feature_ID %in% colnames(partial_cors1()$par_cor), ]
        cluster_assignments_D1 <- cluster_assignments(cluster = cluster, PhenoData = annot_Uni, selected_columns = input$Mapping1)
      }
      return(list(cluster_assignments_D1 = cluster_assignments_D1))
    })

    output$tableClusterAssig <- DT::renderDataTable({
      df1 = cluster_assignments_Data1()$cluster_assignments_D1
      df1 = df1[, -which(names(df1) == "cluster")]
      df1 = df1[, -which(names(df1) == "feature_name")]
      names(df1)[names(df1) == "feature"]  <- "Feature_ID"
      names(df1)[names(df1) == "col"] <- "Module_id"
      DT::datatable(df1)
    })

    # Render the download handler
    output$downloadClusterAssig <- downloadHandler(
      filename = function() {
        "ClusterAssigData1.csv"
      },
      content = function(file) {
        write.csv(cluster_assignments_Data1()$cluster_assignments_D1, file, row.names = TRUE)
      }
    )

    Eigengene1 <- reactive({
      req(load_data1()$feature_mat_t_imp_data)
      load_data = load_data1()$feature_mat_t_imp_data
      Cluster_assignments = hierarchical_cluster1()$hcCluster_assignments[,3]
      Eigengenes = Eigengenes( load_data = load_data, cluster_assignments = Cluster_assignments)$module_eigenmetab_Me
      return(list(Eigengenes = Eigengenes))
    })

    output$tableEigengene <- DT::renderDataTable({
      df2 = as.data.frame(Eigengene1()$Eigengenes)
      df2[] <- lapply(df2, function(x) if(is.numeric(x)) round(x, digits = 4) else x)
      DT::datatable(df2)
    })

    # Render the download handler
    output$downloadtableEigengene <- downloadHandler(
      filename = function() {
        "EigenfeaturesData1.csv"
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
        show_row_names = FALSE, row_names_side = "left", row_names_gp = grid::gpar(fontsize = 8),
        show_column_names = TRUE
      )

      ComplexHeatmap::draw(metab_heatmap_plot, heatmap_legend_side = "right",
                           annotation_legend_side = "left", padding = ggplot2::unit(c(2, 3, 2, 40), "mm"))
    })

    output$downloadheatmapEigenMetab <- downloadHandler(
      filename = function() {
        "HeatmapEigenData1.png"
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

    databaseSelectorListData1 <- reactive({
      gene_set_library = readxl::read_excel("inst/Example_data/Gene_set_Library.xlsx", col_names = FALSE)
      choices <- gene_set_library[[1]]
      data.frame(choices = choices)
    })

    observe({
      updateSelectInput(session, "databaseSelector1", choices = databaseSelectorListData1()$choices)
    })

    curl::has_internet()
    assign("has_internet_via_proxy", TRUE, environment(curl::has_internet))
    httr::set_config(httr::config(ssl_verifypeer = 0L))
    requireNamespace("enrichR", quietly = TRUE)
    enrichR::listEnrichrSites()

    Data1_enrich <- reactive({
      req(input$runEnrichment1)
      withProgress(message = 'Performing enrichment analysis...', value = 0, {
        if (is.null(demo_enrich_Prot()) && is.null(demo_enrich_Prot_All())) {
          req(input$databaseSelector1)
          selected_database <- input$databaseSelector1
          cluster_assignments_D1 <- cluster_assignments_Data1()$cluster_assignments_D1
          cluster_assignments_enrich_D1 <- Assigment_genes_enrichr(cluster_assignments_ProtGenes = cluster_assignments_D1,
                                                                     database = selected_database)
          Sys.sleep(1)
        } else if (demo_enrich_Prot()) {
          Sys.sleep(5)
          cluster_assignments_enrich_D1 <- readRDS(file.path(here::here(),"/inst/Example_data/ccRCC4_Data", "Enrichment.rds"))
        } else if (demo_enrich_Prot_All()) {
          Sys.sleep(5)
          cluster_assignments_enrich_D1 <- readRDS(file.path(here::here(),"/inst/Example_data/FloresData_K_TK", "EnrichmentMouse.rds"))
        }
        incProgress(100, detail = 'Complete!')
        list(cluster_assignments_enrich_D1 = cluster_assignments_enrich_D1)
      })
    })

    output$tableClusterAssigAnnot1 <- DT::renderDataTable({
      df3 = Data1_enrich()$cluster_assignments_enrich_D1
      names(df3)[names(df3) == "col"] = "Module_id"
      df3$enriched_P.value <- round(df3$enriched_P.value, digits = 4)
      df3$enriched_Adjusted.P.value <- round(df3$enriched_Adjusted.P.value, digits = 4)
      DT::datatable(df3)
    })

    # Render the download handler
    output$downloadEnrichment1 <- downloadHandler(
      filename = function() {
        "EnrichmentbyModulesData1.csv"
      },
      content = function(file) {
        write.csv(Data1_enrich()$cluster_assignments_enrich_D1, file, row.names = TRUE)
      }
    )

    # Phenotype

    observeEvent(input$metadata, {
      req(input$metadata)
      filedata_metadata <- read.csv(input$metadata$datapath)
      metadata(filedata_metadata)
    })

    output$tableM1 <- DT::renderDataTable({
      df <- metadata()
      DT::datatable(df)
    })

    pheno_variables <- reactive({
      names(metadata())[-which(names(metadata()) == "Sample")]
    })

    observe({
      updateSelectInput(session, "phenotypeSelector", choices = pheno_variables())
    })


    Classification_Data1 <- reactive({
      eigengenes_metab = as.data.frame(Eigengene1()$Eigengenes)
      metadata <- as.data.frame(metadata())
      phenotype_variable = input$phenotypeSelector
      significance_threshold = input$pValueThreshold
      Classification_Data <- perform_classification( eigengene_data = eigengenes_metab,
                                                           metadata = metadata,
                                                           phenotype_variable = phenotype_variable,
                                                           significance_threshold = significance_threshold
                                                     )
      return(list(
        result = Classification_Data$result,
        plots = Classification_Data$plots))
    })

    output$classification_results <- DT::renderDataTable({
      df <- Classification_Data1()$result
      rownames(df) <- NULL
      names(df)[names(df) == "Variable"] <- "Module_id"
      DT::datatable(df)
    })

    # Render the download handler
    output$downloadClassification_results <- downloadHandler(
      filename = function() {
        "ClassByEigenfeatures_Data1.csv"
      },
      content = function(file) {
        write.csv(Classification_Data1()$result, file, row.names = TRUE)
      }
    )

    output$classification_plot_1_all <- renderPlot({
      selected_variable <- input$phenotypeSelector
      levels_selected_variable <- unique(metadata()[[selected_variable]])
      if (length(levels_selected_variable) < 3) {
        class_names <- levels_selected_variable
        class_label <- paste(class_names, collapse = " vs ")
        plot <- Classification_Data1()$plots[[1]]
        plot <- plot +
          ggplot2::labs(title = class_label, fill = as.factor(levels_selected_variable),
                        x = "Variables",
                        y = "Class") +
          ggplot2::theme(
            axis.text.x = ggplot2::element_text(angle = 90, hjust = 1)
          )
        classification_Metab(plot)
        return(plot)
      } else {
        # Print multiple boxplot charts, one for each level of the selected variable
        plots_list <- lapply(seq_along(levels_selected_variable), function(i) {
          Classification_Data1()$plots[[i]]
        })
        plot <- cowplot::plot_grid(plotlist = plots_list)
        classification_Metab(plot)  # Store the plot in the reactive variable
        return(plot)
      }
    })

    # Render the download handler
    output$downloadClassification_plot_1_all <- downloadHandler(
      filename = function() {
        "Boxplot_classData1.png"
      },
      content = function(file) {
        ggplot2::ggsave(file, plot = classification_Metab(), device = "png")
      }
    )

    loadings_metab <- reactive({
      selected_variable <- input$phenotypeSelector
      cluster_Metab <- subset(hierarchical_cluster1()$hcCluster_assignments, col == input$moduleSelector)
      cluster_variables_Metab <- cluster_Metab$feature
      cluster_variables_MetabKEGG <- cluster_variables_Metab
      cluster_expression_matrix_Metab <- load_data1()$feature_mat_t_imp_data[, colnames(load_data1()$feature_mat_t_imp_data) %in% cluster_variables_Metab, drop = FALSE]
      combined_data <- merge(metadata()[,c("Sample", selected_variable)], cluster_expression_matrix_Metab, by.x = "Sample", by.y = "row.names", all.x = TRUE)
      heatmap_data_sub_order <- combined_data[order(combined_data[[selected_variable]]), ]
      data_heat= t(as.matrix(heatmap_data_sub_order[ , 3:ncol(heatmap_data_sub_order)]))
      pca_res <- prcomp(cluster_expression_matrix_Metab)
      return(list(pca_res = pca_res, data_heat= data_heat, heatmap_data_sub_order = heatmap_data_sub_order, cluster_variables_MetabKEGG = cluster_variables_MetabKEGG))
    })

  output$ModuleFeaturesAnnot1 <- DT::renderDataTable({
    req(loadings_metab())
    df2 <- as.data.frame(loadings_metab()$cluster_variables_MetabKEGG)
    names(df2) <- "Feature_ID"
    selected_columns <- input$Screening1
    if (!is.null(Metab_annot()) && !is.null(selected_columns)) {
      AnnoMeta <- as.data.frame(Metab_annot())
      df2 <- merge(df2, AnnoMeta[, c("Feature_ID", selected_columns)], by = "Feature_ID", all.x = TRUE)
    }
    DT::datatable(df2, rownames = FALSE)
  })

    # Render the download handler
    output$downloadModuleFeaturesAnnot1 <- downloadHandler(
      filename = function() {
        "FeaturesOnMetabolomicsModule.csv"
      },
      content = function(file) {
        df2 = as.data.frame(loadings_metab()$cluster_variables_MetabKEGG)
        names(df2) = "Feature_ID"
        if (!is.null(Metab_annot())) {
          AnnoMeta = as.data.frame(Metab_annot())
          df2 <- merge(df2, AnnoMeta[, c("Feature_ID", "KEGG", "Metabolite")], by = "Feature_ID", all.x = TRUE)
        }
        write.csv(df2, file, row.names = FALSE)
      }
    )

    output$Loadings1 <- renderPlot({
      requireNamespace("ggplot2", quietly = TRUE)
      requireNamespace("ggfortify", quietly = TRUE)
      ggplot2::autoplot(loadings_metab()$pca_res, data = metadata(), colour = input$phenotypeSelector, loadings = TRUE)
    })

    # Render the download handler
    output$downloadLoadings1 <- downloadHandler(
      filename = function() {
        "Loadings_Data1.png"
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


    ###### Data2
    # Data Input

    observeEvent(input$Data2, {
      req(input$Data2)
      filedata_value <- read.csv(input$Data2$datapath)
      Prot_exp(filedata_value)
    })

    observeEvent(input$PhenoData2, {
      req(input$PhenoData2)
      filedata_value <- read.csv(input$PhenoData2$datapath)
      Prot_annot(filedata_value)
    })

    data_info2 <- reactive({
      req(Prot_exp())
      Nobservations <- nrow(Prot_exp())
      Ncells <- ncol(Prot_exp())-1
      SummaryData <- as.data.frame(list(Number = c(Ncells, Nobservations)))
      rownames(SummaryData) <- c("Samples", "Features")
      list(SummaryData = SummaryData)
    })
    output$infotable2 <- DT::renderDataTable({
      df <- data_info2()$SummaryData
      DT::datatable(df)
    })

    output$table2 <- DT::renderDataTable({
      df <- Prot_exp()
      df[] <- lapply(df, function(x) if(is.numeric(x)) round(x, digits = 2) else x)
      DT::datatable(df)
    })

    variables_mapping2 <- reactive({
      names(Prot_annot())[-which(names(Prot_annot()) == "Feature_ID")]
    })

    observe({
      updateSelectInput(session, "Mapping2", choices = variables_mapping2())
    })

    pheno_variablesPCA2 <- reactive({
      names(metadata())[-which(names(metadata()) == "Sample")]
    })

    observe({
      updateSelectInput(session, "phenotypeSelectorPCA2", choices = pheno_variablesPCA2())
    })

    variables_Screening2 <- reactive({
      names(Prot_annot())[-which(names(Prot_annot()) == "Feature_ID")]
    })

    observe({
      updateSelectInput(session, "Screening2", choices = variables_Screening2())
    })

    load_data2 <- reactive({
      req(Prot_exp())
      Expression_mat = Prot_exp()
      feature_mat_t_imp_data = load_data(Expression_mat = Expression_mat)
      return(list(feature_mat_t_imp_data = feature_mat_t_imp_data))
    })

    pca2 <- reactive({
      pca_res <- prcomp(load_data2()$feature_mat_t_imp_data)
      return(list(pca_res = pca_res))
    })

    output$PCA2 <- renderPlot({
      requireNamespace("ggplot2", quietly = TRUE)
      requireNamespace("ggfortify", quietly = TRUE)
      if(is.null(metadata())){
        ggplot2::autoplot(pca2()$pca_res)
      } else {
        #ggplot2::autoplot(pca2()$pca_res, data = metadata(), colour = input$phenotypeSelectorPCA2)
        req(metadata())

        pca_data <- as.data.frame(pca2()$pca_res$x)
        combined_data <- cbind(pca_data, metadata())
        color_column <- input$phenotypeSelectorPCA2

        if (!is.null(color_column) && color_column != "") {
          ggplot2::ggplot(combined_data, ggplot2::aes_string(x = "PC1", y = "PC2", color = color_column)) +
            ggplot2::geom_point()
        } else {
          showNotification("Please select a phenotype to color the PCA plot.", type = "message")
          return(NULL)
        }
      }
    })

    # Render the download handler
    output$downloadPCA2 <- downloadHandler(
      filename = function() {
        "PCA_Data2.png"
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

    output$tableAnnot2 <- DT::renderDataTable({
      df <- Prot_annot()
      DT::datatable(df)
    })

    # Module Assigments
    partial_cors2 <- reactive({
      withProgress(message = 'Calculating partial correlations Data 2...', value = 0, {
        if (is.null(demo_par_cor_Prot()) && is.null(demo_par_cor_Prot_All())) {
          req(load_data2()$feature_mat_t_imp_data)
          load_data2 = load_data2()$feature_mat_t_imp_data
          Sys.sleep(5)
          par_cor <- partial_cors(load_data = load_data2, rho = .25)
        } else if (demo_par_cor_Prot()) {
          Sys.sleep(5)
          #par_cor <- iModMixData::loadPartialCorGenes()
          par_cor <- readRDS(file.path(here::here(),"/inst/Example_data/ccRCC4_Data", "PartialCorGenes.rds"))
        } else if (demo_par_cor_Prot_All()) {
          Sys.sleep(5)
          #par_cor <- iModMixData::loadPartialCorProt()
          par_cor <- readRDS(file.path(here::here(),"/inst/Example_data/FloresData_K_TK", "PartialCorProt.rds"))
        }
        incProgress(100, detail = 'Complete!')
        list(par_cor = par_cor)
      })
    })

    output$matrizTable2 <- renderPrint({
      partial_cors2()$par_cor[seq_len(5), seq_len(5)]
    })

    # Render the download handler
    output$downloadParCor2 <- downloadHandler(
      filename = function() {
        "PartialCorData2.csv"
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

    unique_variables2 <- reactive({
      unique(hierarchical_cluster2()$hcCluster_assignments$col)
    })

    observe({
      updateSelectInput(session, "moduleSelector2", choices = unique_variables2())
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
        "Hierarchical_clusterData2.png"
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

    cluster_assignments_Data2 <- reactive({
      cluster = as.data.frame(hierarchical_cluster2()$hcCluster_assignments)
      if (is.null(Prot_annot())) {
        cluster_assignments_D2 <- cluster_assignments(cluster = cluster, PhenoData = NULL, selected_columns = NULL)
      } else {
        Prot_annot = Prot_annot()
        annot_Uni <- Prot_annot[Prot_annot$Feature_ID %in% colnames(partial_cors2()$par_cor), ]
        cluster_assignments_D2 <- cluster_assignments(cluster = cluster, PhenoData = annot_Uni, selected_columns = input$Mapping2)
      }
      return(list(cluster_assignments_D2 = cluster_assignments_D2))
    })

    output$tableClusterAssig2 <- DT::renderDataTable({
      df1 = cluster_assignments_Data2()$cluster_assignments_D2
      df1 = df1[, -which(names(df1) == "cluster")]
      df1 = df1[, -which(names(df1) == "feature_name")]
      names(df1)[names(df1) == "feature"] = "Feature_ID"
      names(df1)[names(df1) == "col"] = "Module_id"
      DT::datatable(df1)
    })

    # Render the download handler
    output$downloadClusterAssig2 <- downloadHandler(
      filename = function() {
        "ClusterAssigData2.csv"
      },
      content = function(file) {
        write.csv(cluster_assignments_Data2()$cluster_assignments_D2, file, row.names = TRUE)
      }
    )

    Eigengene2 <- reactive({
      req(load_data2()$feature_mat_t_imp_data)
      load_data = load_data2()$feature_mat_t_imp_data
      Cluster_assignments = hierarchical_cluster2()$hcCluster_assignments[,3]
      Eigengenes = Eigengenes(load_data = load_data,
                              cluster_assignments = Cluster_assignments)$module_eigenmetab_Me
      return(list(Eigengenes = Eigengenes))
    })

    output$tableEigengene2 <- DT::renderDataTable({
      df3 = as.data.frame(Eigengene2()$Eigengenes)
      df3[] <- lapply(df3, function(x) if(is.numeric(x)) round(x, digits = 4) else x)
      DT::datatable(df3)
    })

    # Render the download handler
    output$downloadtableEigengene2 <- downloadHandler(
      filename = function() {
        "EigenfeaturesData2.csv"
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
        show_row_names = FALSE, row_names_side = "left", row_names_gp = grid::gpar(fontsize = 8),
        show_column_names = TRUE
      )
      ComplexHeatmap::draw(metab_heatmap_plot, heatmap_legend_side = "right",
                           annotation_legend_side = "left", padding = ggplot2::unit(c(2, 3, 2, 40), "mm"))
    })

    output$downloadheatmapEigenProt <- downloadHandler(
      filename = function() {
        "HeatmapEigenData2.png"
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


    databaseSelectorListData2 <- reactive({
      gene_set_library = readxl::read_excel("inst/Example_data/Gene_set_Library.xlsx", col_names = FALSE)
      choices <- gene_set_library[[1]]
      data.frame(choices = choices)
    })

    observe({
      updateSelectInput(session, "databaseSelector2", choices = databaseSelectorListData2()$choices)
    })


    Data2_enrich <- reactive({
      req(input$runEnrichment2)
      withProgress(message = 'Performing enrichment analysis...', value = 0, {
        if (is.null(demo_enrich_Prot()) && is.null(demo_enrich_Prot_All())) {
          req(input$databaseSelector2)
          selected_database <- input$databaseSelector2
          cluster_assignments_ProtGenes <- cluster_assignments_Data2()$cluster_assignments_D2
          cluster_assignments_Data2_enrich <- Assigment_genes_enrichr(cluster_assignments_ProtGenes = cluster_assignments_ProtGenes,
                                                                     database = selected_database)
          Sys.sleep(1)
        } else if (demo_enrich_Prot()) {
          Sys.sleep(5)
          cluster_assignments_Data2_enrich <- readRDS(file.path(here::here(),"/inst/Example_data/ccRCC4_Data", "Enrichment.rds"))
        } else if (demo_enrich_Prot_All()) {
          Sys.sleep(5)
          cluster_assignments_Data2_enrich <- readRDS(file.path(here::here(),"/inst/Example_data/FloresData_K_TK", "EnrichmentMouse.rds"))
        }
        incProgress(100, detail = 'Complete!')
        list(cluster_assignments_Data2_enrich = cluster_assignments_Data2_enrich)
      })
    })

    output$tableClusterAssigAnnot2 <- DT::renderDataTable({
      df3 = Data2_enrich()$cluster_assignments_Data2_enrich
      df3 = df3[, -which(names(df3) == "cluster")]
      names(df3)[names(df3) == "col"] = "Module_id"
      df3$enriched_P.value <- round(df3$enriched_P.value, digits = 4)
      df3$enriched_Adjusted.P.value <- round(df3$enriched_Adjusted.P.value, digits = 4)
      DT::datatable(df3)
    })

    # Render the download handler
    output$downloadEnrichment2 <- downloadHandler(
      filename = function() {
        "EnrichmentbyModules.csv"
      },
      content = function(file) {
        write.csv(Data2_enrich()$cluster_assignments_Data2_enrich, file, row.names = TRUE)
      }
    )

    # Phenotype

    output$tableM2 <- DT::renderDataTable({
      df <- metadata()
      DT::datatable(df)
    })

    pheno_variables2 <- reactive({
      names(metadata())[-which(names(metadata()) == "Sample")]
    })

    observe({
      updateSelectInput(session, "phenotypeSelector2", choices = pheno_variables2())
    })

    Classification_Data2 <- reactive({
      eigengenes_prot = as.data.frame(Eigengene2()$Eigengenes)
      metadata <- as.data.frame(metadata())
      phenotype_variable = input$phenotypeSelector2
      significance_threshold = input$pValueThreshold2
      Classification_Data2 <- perform_classification( eigengene_data = eigengenes_prot,
                                                         metadata = metadata,
                                                         phenotype_variable = phenotype_variable,
                                                         significance_threshold = significance_threshold)
      return(list(
        result = Classification_Data2$result,
        plots = Classification_Data2$plots))
    })

    output$classification_results2 <- DT::renderDataTable({
      df <- Classification_Data2()$result
      rownames(df) <- NULL
      names(df)[names(df) == "Variable"] = "Module_id"
      DT::datatable(df)
    })

    # Render the download handler
    output$downloadClassification_results2 <- downloadHandler(
      filename = function() {
        "ClassByEigenfeatures_Data2.csv"
      },
      content = function(file) {
        write.csv(Classification_Data2()$result, file, row.names = TRUE)
      }
    )

    output$classification_plot_2_all <- renderPlot({
      selected_variable <- input$phenotypeSelector2
      levels_selected_variable <- unique(metadata()[[selected_variable]])
      if (length(levels_selected_variable) < 3) {
        class_names <- levels_selected_variable
        class_label <- paste(class_names, collapse = " vs ")
        plot <- Classification_Data2()$plots[[1]]
        plot <- plot +
          ggplot2::labs(title = class_label, fill = as.factor(levels_selected_variable),
                        x = "Variables",
                        y = "Class") +
          ggplot2::theme(
            axis.text.x = ggplot2::element_text(angle = 90, hjust = 1)
          )
        classification_Prot(plot)
        return(plot)
      } else {
        # Print multiple boxplot charts, one for each level of the selected variable
        plots_list <- lapply(seq_along(levels_selected_variable), function(i) {
          Classification_Data2()$plots[[i]]
        })
        plot <- cowplot::plot_grid(plotlist = plots_list)
        classification_Prot(plot)  # Store the plot in the reactive variable
        return(plot)
      }
    })

    output$downloadClassification_plot_2_all <- downloadHandler(
      filename = function() {
        "Boxplot_classificationData2.png"
      },
      content = function(file) {
        # Save the plot stored in the reactive variable
        ggplot2::ggsave(file, plot = classification_Prot(), device = "png")
      }
    )

    loadings_Prot <- reactive({
      selected_variable <- input$phenotypeSelector2
      cluster_Prot <- subset(hierarchical_cluster2()$hcCluster_assignments, col == input$moduleSelector2)
      #cluster_Metab <- subset(cluster_assignments_Data1()$cluster_assignments_D1, cluster == "cluster_000011")
      cluster_variables_Prot <- cluster_Prot$feature
      cluster_variables_ProtSymbol <- cluster_variables_Prot
      cluster_expression_matrix_Prot <- load_data2()$feature_mat_t_imp_data[, colnames(load_data2()$feature_mat_t_imp_data) %in% cluster_variables_Prot, drop = FALSE]
      combined_data <- merge(metadata()[,c("Sample", selected_variable)], cluster_expression_matrix_Prot, by.x = "Sample", by.y = "row.names", all.x = TRUE)
      heatmap_data_sub_order <- combined_data[order(combined_data[[selected_variable]]), ]
      data_heat= t(as.matrix(heatmap_data_sub_order[ , 3:ncol(heatmap_data_sub_order)]))
      pca_res <- prcomp(cluster_expression_matrix_Prot)
      return(list(pca_res = pca_res, data_heat= data_heat, heatmap_data_sub_order = heatmap_data_sub_order, cluster_variables_ProtSymbol = cluster_variables_ProtSymbol))
    })

    output$ModuleFeaturesAnnot2 <- DT::renderDataTable({
      req(loadings_Prot())
      df2 = as.data.frame(loadings_Prot()$cluster_variables_ProtSymbol)
      names(df2) = "Feature_ID"
      selected_columns <- input$Screening2
      if (!is.null(Prot_annot()) && !is.null(selected_columns)) {
        AnnoProt = as.data.frame(Prot_annot())
        df2 <- merge(df2, AnnoProt[, c("Feature_ID", selected_columns)], by = "Feature_ID", all.x = TRUE)
      }
      DT::datatable(df2, rownames = FALSE)
    })

    # Render the download handler
    output$downloadModuleFeaturesAnnot2 <- downloadHandler(
      filename = function() {
        "FeaturesOnProteomicsModule.csv"
      },
      content = function(file) {
        df2 = as.data.frame(loadings_Prot()$cluster_variables_ProtSymbol)
        names(df2) = "Feature_ID"
        selected_columns <- input$Screening2
        if (!is.null(Prot_annot()) && !is.null(selected_columns)) {
          AnnoProt = as.data.frame(Prot_annot())
          df2 <- merge(df2, AnnoProt[, c("Feature_ID", selected_columns)], by = "Feature_ID", all.x = TRUE)
        }
        write.csv(df2, file, row.names = FALSE)
      }
    )

    output$Loadings2 <- renderPlot({
      requireNamespace("ggplot2", quietly = TRUE)
      requireNamespace("ggfortify", quietly = TRUE)
      ggplot2::autoplot(loadings_Prot()$pca_res, data = metadata(), colour = input$phenotypeSelector2, loadings = TRUE)
    })

    # Render the download handler
    output$downloadLoadings2 <- downloadHandler(
      filename = function() {
        "Loadings_Data2.png"
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
        "HeatmapModData2.png"
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

    ####### Data3
    # Data Input

    observeEvent(input$Data3, {
      req(input$Data3)
      filedata_value <- read.csv(input$Data3$datapath)
      Gene_exp(filedata_value)
    })

    observeEvent(input$PhenoData3, {
      req(input$PhenoData3)
      filedata_value <- read.csv(input$PhenoData3$datapath)
      Gene_annot(filedata_value)
    })
    data_info3 <- reactive({
      req(Gene_exp())
      Nobservations <- nrow(Gene_exp())
      Ncells <- ncol(Gene_exp())-1
      SummaryData <- as.data.frame(list(Number = c(Ncells, Nobservations)))
      rownames(SummaryData) <- c("Samples", "Features")
      list(SummaryData = SummaryData)
    })
    output$infotable3 <- DT::renderDataTable({
      df <- data_info3()$SummaryData
      DT::datatable(df)
    })

    output$table3 <- DT::renderDataTable({
      df <- Gene_exp()
      df[] <- lapply(df, function(x) if(is.numeric(x)) round(x, digits = 2) else x)
      DT::datatable(df)
    })

    variables_mapping3 <- reactive({
      names(Gene_annot())[-which(names(Gene_annot()) == "Feature_ID")]
    })

    observe({
      updateSelectInput(session, "Mapping3", choices = variables_mapping3())
    })

    pheno_variablesPCA3 <- reactive({
      if (is.null(Gene_exp())) {
        return(NULL)
      } else {
        names(metadata())[names(metadata()) != "Sample"]
      }
    })

    observe({
      updateSelectInput(session, "phenotypeSelectorPCA3", choices = pheno_variablesPCA3())
    })

    variables_Screening3 <- reactive({
      names(Gene_annot())[-which(names(Gene_annot()) == "Feature_ID")]
    })

    observe({
      updateSelectInput(session, "Screening3", choices = variables_Screening3())
    })

    load_data3 <- reactive({
      req(Gene_exp())
      Expression_mat = Gene_exp()
      feature_mat_t_imp_data = load_data(Expression_mat = Expression_mat)
      return(list(feature_mat_t_imp_data = feature_mat_t_imp_data))
    })

    pca3 <- reactive({
      pca_res <- prcomp(load_data3()$feature_mat_t_imp_data)
      return(list(pca_res = pca_res))
    })

    output$PCA3 <- renderPlot({
      requireNamespace("ggplot2", quietly = TRUE)
      requireNamespace("ggfortify", quietly = TRUE)
      if(is.null(metadata())){
        ggplot2::autoplot(pca3()$pca_res)
      } else {
        #ggplot2::autoplot(pca3()$pca_res, data = metadata(), colour = input$phenotypeSelectorPCA3)
        req(metadata())

        pca_data <- as.data.frame(pca3()$pca_res$x)
        combined_data <- cbind(pca_data, metadata())
        color_column <- input$phenotypeSelectorPCA3

        if (!is.null(color_column) && color_column != "") {
          ggplot2::ggplot(combined_data, ggplot2::aes_string(x = "PC1", y = "PC2", color = color_column)) +
            ggplot2::geom_point()
        } else {
          showNotification("Please select a phenotype to color the PCA plot.", type = "message")
          return(NULL)
        }
      }
    })

    # Render the download handler
    output$downloadPCA3 <- downloadHandler(
      filename = function() {
        "PCA_Data3.png"
      },
      content = function(file) {
        if (is.null(metadata())) {
          p <- ggplot2::autoplot(pca3()$pca_res)
        } else {
          req(metadata())
          p <- ggplot2::autoplot(pca3()$pca_res, data = metadata(), colour = input$phenotypeSelectorPCA3)
        }
        ggplot2::ggsave(file, plot = p, device = "png")
      }
    )

    output$tableAnnot3 <- DT::renderDataTable({
      df <- Gene_annot()
      DT::datatable(df)
    })

    # Module Assigments
    partial_cors3 <- reactive({
      withProgress(message = 'Calculating partial correlations Data 3...', value = 0, {
          req(load_data3()$feature_mat_t_imp_data)
          load_data3 = load_data3()$feature_mat_t_imp_data
          Sys.sleep(5)
          par_cor <- partial_cors(load_data = load_data3, rho = .25)
        incProgress(100, detail = 'Complete!')
        list(par_cor = par_cor)
      })
    })

    output$matrizTable3 <- renderPrint({
      partial_cors3()$par_cor[seq_len(5), seq_len(5)]
    })

    # Render the download handler
    output$downloadParCor3 <- downloadHandler(
      filename = function() {
        "PartialCorData3.csv"
      },
      content = function(file) {
        write.csv(partial_cors3()$par_cor, file, row.names = TRUE)
      }
    )

    hierarchical_cluster3 <- reactive({
      par_cor = partial_cors3()$par_cor
      hc = hierarchical_cluster(parcor_mat = par_cor, tom = TRUE, min_module_size = 10)
      hclusterTree = hc$hclustTree
      hcDynMods = hc$dynamicMods_numeric
      hcCluster_assignments = hc$cluster_assignments
      return(list(hclusterTree = hclusterTree,
                  hcDynMods = hcDynMods,
                  hcCluster_assignments = hcCluster_assignments ))
    })

    unique_variables3 <- reactive({
      unique(hierarchical_cluster3()$hcCluster_assignments$col)
    })

    observe({
      updateSelectInput(session, "moduleSelector3", choices = unique_variables3())
    })

    output$hc_plot3 <- renderPlot({
      hcClu = hierarchical_cluster3()$hclusterTree
      hcMod = hierarchical_cluster3()$hcDynMods
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
    output$downloadhc_plot3 <- downloadHandler(
      filename = function() {
        "Hierarchical_clusterData3.png"
      },
      content = function(file) {
        png(file)
        hcClu = hierarchical_cluster3()$hclusterTree
        hcMod = hierarchical_cluster3()$hcDynMods
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

    cluster_assignments_Data3 <- reactive({
      cluster = as.data.frame(hierarchical_cluster3()$hcCluster_assignments)
      if (is.null(Gene_annot())) {
        cluster_assignments_D3 <- cluster_assignments(cluster = cluster, PhenoData = NULL, selected_columns = NULL)
      } else {
        Gene_annot = Gene_annot()
        annot_Uni <- Gene_annot[Gene_annot$Feature_ID %in% colnames(partial_cors3()$par_cor), ]
        cluster_assignments_D3 <- cluster_assignments(cluster = cluster, PhenoData = annot_Uni, selected_columns = input$Mapping3)
      }
      return(list(cluster_assignments_D3 = cluster_assignments_D3))
    })

    output$tableClusterAssig3 <- DT::renderDataTable({
      df1 = cluster_assignments_Data3()$cluster_assignments_D3
      df1 = df1[, -which(names(df1) == "cluster")]
      df1 = df1[, -which(names(df1) == "feature_name")]
      names(df1)[names(df1) == "feature"] = "Feature_ID"
      names(df1)[names(df1) == "col"] = "Module_id"
      DT::datatable(df1)
    })

    # Render the download handler
    output$downloadClusterAssig3 <- downloadHandler(
      filename = function() {
        "ClusterAssigData3.csv"
      },
      content = function(file) {
        write.csv(cluster_assignments_Data3()$cluster_assignments_D3, file, row.names = TRUE)
      }
    )

    databaseSelectorListData3 <- reactive({
      gene_set_library = readxl::read_excel("inst/Example_data/Gene_set_Library.xlsx", col_names = FALSE)
      choices <- gene_set_library[[1]]
      data.frame(choices = choices)
    })

    observe({
      updateSelectInput(session, "databaseSelector3", choices = databaseSelectorListData3()$choices)
    })

    Data3_enrich <- reactive({
      req(input$runEnrichment3)
      withProgress(message = 'Performing enrichment analysis...', value = 0, {
          req(input$databaseSelector3)
          selected_database <- input$databaseSelector3
          cluster_assignments_ProtGenes <- cluster_assignments_Data3()$cluster_assignments_D3
          cluster_assignments_Data3_enrich <- Assigment_genes_enrichr(cluster_assignments_ProtGenes = cluster_assignments_ProtGenes,
                                                                     database = selected_database)
          Sys.sleep(5)
        incProgress(100, detail = 'Complete!')
        list(cluster_assignments_Data3_enrich = cluster_assignments_Data3_enrich)
      })
    })

    output$tableClusterAssigAnnot3 <- DT::renderDataTable({
      df3 = Data3_enrich()$cluster_assignments_Data3_enrich
      df3 = df3[, -which(names(df3) == "cluster")]
      names(df3)[names(df3) == "col"] = "Module_id"
      df3$enriched_P.value <- round(df3$enriched_P.value, digits = 4)
      df3$enriched_Adjusted.P.value <- round(df3$enriched_Adjusted.P.value, digits = 4)
      DT::datatable(df3)
    })

    # Render the download handler
    output$downloadEnrichment3 <- downloadHandler(
      filename = function() {
        "EnrichmentbyData3Modules.csv"
      },
      content = function(file) {
        write.csv(Data3_enrich()$cluster_assignments_Data3_enrich, file, row.names = TRUE)
      }
    )

    Eigengene3 <- reactive({
      req(load_data3()$feature_mat_t_imp_data)
      load_data = load_data3()$feature_mat_t_imp_data
      Cluster_assignments = hierarchical_cluster3()$hcCluster_assignments[,3]
      Eigengenes = Eigengenes(load_data = load_data,
                              cluster_assignments = Cluster_assignments)$module_eigenmetab_Me
      return(list(Eigengenes = Eigengenes))
    })

    output$tableEigengene3 <- DT::renderDataTable({
      df3 = as.data.frame(Eigengene3()$Eigengenes)
      df3[] <- lapply(df3, function(x) if(is.numeric(x)) round(x, digits = 4) else x)
      DT::datatable(df3)
    })

    # Render the download handler
    output$downloadtableEigengene3 <- downloadHandler(
      filename = function() {
        "EigenfeaturesData3.csv"
      },
      content = function(file) {
        write.csv(Eigengene3()$Eigengenes, file, row.names = TRUE)
      }
    )

    output$heatmapEigenGene <- renderPlot({
      heatmap_plot = ComplexHeatmap::Heatmap(
        as.data.frame(t(Eigengene3()$Eigengenes)), cluster_columns = FALSE, cluster_rows = TRUE,
        row_title = "Eigenfeatures", column_title = "Samples", name = "Z-score",
        heatmap_legend_param = list(title_position = "topleft", legend_direction = "vertical"),
        show_row_names = FALSE, row_names_side = "left", row_names_gp = grid::gpar(fontsize = 8),
        show_column_names = TRUE
      )
      ComplexHeatmap::draw(heatmap_plot, heatmap_legend_side = "right",
                           annotation_legend_side = "left", padding = ggplot2::unit(c(2, 3, 2, 40), "mm"))
    })

    output$downloadheatmapEigenGene <- downloadHandler(
      filename = function() {
        "HeatmapEigenData3.png"
      },
      content = function(file) {
        png(file)
        heatmap_plot = ComplexHeatmap::Heatmap(
          as.data.frame(t(Eigengene3()$Eigengenes)), cluster_columns = FALSE, cluster_rows = TRUE,
          row_title = "Eigenfeatures", column_title = "Samples", name = "Z-score",
          heatmap_legend_param = list(title_position = "topleft", legend_direction = "vertical"),
          show_row_names = TRUE, row_names_side = "left", row_names_gp = grid::gpar(fontsize = 8),
          show_column_names = TRUE
        )
        ComplexHeatmap::draw(heatmap_plot, heatmap_legend_side = "right",
                             annotation_legend_side = "left", padding = ggplot2::unit(c(2, 3, 2, 40), "mm"))
        dev.off()
      }
    )

    # Phenotype
    output$tableM3 <- DT::renderDataTable({
      if (is.null(Gene_exp())) {
        return(NULL)
      } else {
        df <- metadata()
        DT::datatable(df)
      }
    })

    pheno_variables3 <- reactive({
      if (is.null(Gene_exp())) {
        return(NULL)
      } else {
        names(metadata())[names(metadata()) != "Sample"]
      }
    })

    observe({
      updateSelectInput(session, "phenotypeSelector3", choices = pheno_variables3())
    })

    Classification_Data3 <- reactive({
      eigengenes_prot = as.data.frame(Eigengene3()$Eigengenes)
      metadata <- as.data.frame(metadata())
      phenotype_variable = input$phenotypeSelector3
      significance_threshold = input$pValueThreshold3
      Classification_Data <- perform_classification( eigengene_data = eigengenes_prot,
                                                     metadata = metadata,
                                                     phenotype_variable = phenotype_variable,
                                                     significance_threshold = significance_threshold)
      return(list(
        result = Classification_Data$result,
        plots = Classification_Data$plots))
    })

    output$classification_results3 <- DT::renderDataTable({
      df <- Classification_Data3()$result
      rownames(df) <- NULL
      names(df)[names(df) == "Variable"] = "Module_id"
      DT::datatable(df)
    })

    # Render the download handler
    output$downloadClassification_results3 <- downloadHandler(
      filename = function() {
        "ClassByEigenfeatures_Data3.csv"
      },
      content = function(file) {
        write.csv(Classification_Data3()$result, file, row.names = TRUE)
      }
    )

    output$classification_plot_3_all <- renderPlot({
      selected_variable <- input$phenotypeSelector3
      levels_selected_variable <- unique(metadata()[[selected_variable]])
      if (length(levels_selected_variable) < 3) {
        class_names <- levels_selected_variable
        class_label <- paste(class_names, collapse = " vs ")
        plot <- Classification_Data3()$plots[[1]]
        plot <- plot +
          ggplot2::labs(title = class_label, fill = as.factor(levels_selected_variable),
                        x = "Variables",
                        y = "Class") +
          ggplot2::theme(
            axis.text.x = ggplot2::element_text(angle = 90, hjust = 1)
          )
        classification_Gene(plot)
        return(plot)
      } else {
        # Print multiple boxplot charts, one for each level of the selected variable
        plots_list <- lapply(seq_along(levels_selected_variable), function(i) {
          Classification_Data3()$plots[[i]]
        })
        plot <- cowplot::plot_grid(plotlist = plots_list)
        classification_Gene(plot)  # Store the plot in the reactive variable
        return(plot)
      }
    })

    output$downloadClassification_plot_3_all <- downloadHandler(
      filename = function() {
        "Boxplot_classificationData3.png"
      },
      content = function(file) {
        # Save the plot stored in the reactive variable
        ggplot2::ggsave(file, plot = classification_Gene(), device = "png")
      }
    )

    loadings_Gene <- reactive({
      selected_variable <- input$phenotypeSelector3
      cluster_Prot <- subset(hierarchical_cluster3()$hcCluster_assignments, col == input$moduleSelector3)
      #cluster_Metab <- subset(cluster_assignments_Data1()$cluster_assignments_D1, cluster == "cluster_000011")
      cluster_variables_Prot <- cluster_Prot$feature
      cluster_variables_ProtSymbol <- cluster_variables_Prot
      cluster_expression_matrix_Prot <- load_data3()$feature_mat_t_imp_data[, colnames(load_data3()$feature_mat_t_imp_data) %in% cluster_variables_Prot, drop = FALSE]
      combined_data <- merge(metadata()[,c("Sample", selected_variable)], cluster_expression_matrix_Prot, by.x = "Sample", by.y = "row.names", all.x = TRUE)
      heatmap_data_sub_order <- combined_data[order(combined_data[[selected_variable]]), ]
      data_heat= t(as.matrix(heatmap_data_sub_order[ , 3:ncol(heatmap_data_sub_order)]))
      pca_res <- prcomp(cluster_expression_matrix_Prot)
      return(list(pca_res = pca_res, data_heat= data_heat, heatmap_data_sub_order = heatmap_data_sub_order, cluster_variables_ProtSymbol = cluster_variables_ProtSymbol))
    })

    output$ModuleFeaturesAnnot3 <- DT::renderDataTable({
      req(loadings_Gene())
      df2 = as.data.frame(loadings_Gene()$cluster_variables_ProtSymbol)
      names(df2) = "Feature_ID"
      selected_columns <- input$Screening3
      if (!is.null(Prot_annot()) && !is.null(selected_columns)) {
        AnnoProt = as.data.frame(Prot_annot())
        df2 <- merge(df2, AnnoProt[, c("Feature_ID", selected_columns)], by = "Feature_ID", all.x = TRUE)
      }
      DT::datatable(df2, rownames = FALSE)
    })


    # Render the download handler
    output$downloadModuleFeaturesAnnot3 <- downloadHandler(
      filename = function() {
        "FeaturesOnData3Module.csv"
      },
      content = function(file) {
        df2 = as.data.frame(loadings_Gene()$cluster_variables_ProtSymbol)
        names(df2) = "Feature_ID"
        selected_columns <- input$Screening3
        if (!is.null(Prot_annot()) && !is.null(selected_columns)) {
          AnnoProt = as.data.frame(Prot_annot())
          df2 <- merge(df2, AnnoProt[, c("Feature_ID", selected_columns)], by = "Feature_ID", all.x = TRUE)
        }
        write.csv(df2, file, row.names = FALSE)
      }
    )

    output$Loadings3 <- renderPlot({
      requireNamespace("ggplot2", quietly = TRUE)
      requireNamespace("ggfortify", quietly = TRUE)
      ggplot2::autoplot(loadings_Gene()$pca_res, data = metadata(), colour = input$phenotypeSelector3, loadings = TRUE)
    })

    # Render the download handler
    output$downloadLoadings3 <- downloadHandler(
      filename = function() {
        "Loadings_Data3.png"
      },
      content = function(file) {
        p <- ggplot2::autoplot(loadings_Gene()$pca_res, data = metadata(), colour = input$phenotypeSelector3, loadings = TRUE)
        ggplot2::ggsave(file, plot = p, device = "png")
      }
    )

    output$heatmap3 <- renderPlot({
      selected_variable <- input$phenotypeSelector3
      levels_selected_variable <- unique(metadata()[[selected_variable]])

      if (length(levels_selected_variable) == 2) {
        col_palette <- c("Level1" = "#1B9E77", "Level2" = "#D95F02")
      } else {
        col_palette <- RColorBrewer::brewer.pal(length(levels_selected_variable), "Set1")
      }

      # Column annotation
      column_anno = ComplexHeatmap::HeatmapAnnotation(
        selected_variable = as.factor(loadings_Gene()$heatmap_data_sub_order[[selected_variable]]),
        col = list(selected_variable = setNames(col_palette, levels_selected_variable)),
        annotation_legend_param = list(selected_variable = list(title_position = "topleft", legend_direction = "vertical"))
      )

      heatmap_plot = ComplexHeatmap::Heatmap(
        loadings_Gene()$data_heat, cluster_columns = FALSE, cluster_rows = TRUE,
        row_title = "Genes Abundance", column_title = "Tissues", name = "Z-score",
        heatmap_legend_param = list(title_position = "topleft", legend_direction = "vertical"),
        show_row_names = TRUE, row_names_side = "left", row_names_gp = grid::gpar(fontsize = 8),
        show_column_names = FALSE,  top_annotation = column_anno
      )

      ComplexHeatmap::draw(heatmap_plot, heatmap_legend_side = "right",
                           annotation_legend_side = "left", padding = ggplot2::unit(c(2, 3, 2, 40), "mm"))
    })

    output$downloadHeatmap3 <- downloadHandler(
      filename = function() {
        "HeatmapModData3.png"
      },
      content = function(file) {
        png(file, width = 800, height = 600)  # Adjust dimensions as needed
        selected_variable <- input$phenotypeSelector3
        levels_selected_variable <- unique(metadata()[[selected_variable]])

        if (length(levels_selected_variable) == 2) {
          col_palette <- c("Level1" = "#1B9E77", "Level2" = "#D95F02")
        } else {
          col_palette <- RColorBrewer::brewer.pal(length(levels_selected_variable), "Set1")
        }

        column_anno = ComplexHeatmap::HeatmapAnnotation(
          selected_variable = as.factor(loadings_Gene()$heatmap_data_sub_order[[selected_variable]]),
          col = list(selected_variable = setNames(col_palette, levels_selected_variable)),
          annotation_legend_param = list(selected_variable = list(title_position = "topleft", legend_direction = "vertical"))
        )

        heatmap_plot = ComplexHeatmap::Heatmap(
          loadings_Gene()$data_heat, cluster_columns = FALSE, cluster_rows = TRUE,
          row_title = "Genes Abundance", column_title = "Tissues", name = "Z-score",
          heatmap_legend_param = list(title_position = "topleft", legend_direction = "vertical"),
          show_row_names = TRUE, row_names_side = "left", row_names_gp = grid::gpar(fontsize = 8),
          show_column_names = FALSE, top_annotation = column_anno
        )

        ComplexHeatmap::draw(heatmap_plot, heatmap_legend_side = "right",
                             annotation_legend_side = "left", padding = ggplot2::unit(c(2, 3, 2, 40), "mm"))
        dev.off()
      }
    )
    ######## Multiomics
    # Modules Correlations
    Cor_Data_n <- reactive({
      threshold <- input$pValueThresholdcor
      eigengenes_list <- list(Eigengene1()$Eigengenes, Eigengene2()$Eigengenes)
      cluster_list <- list(hierarchical_cluster1()$hcCluster_assignments, hierarchical_cluster2()$hcCluster_assignments)
      if (!is.null(Gene_exp())) {
        eigengenes_list <- c(eigengenes_list, list(Eigengene3()$Eigengenes))
        cluster_list <- c(cluster_list, list(hierarchical_cluster3()$hcCluster_assignments))
      }
      Cor_Prot_Metab <- Modules_correlation(eigengenes_list, cluster_list, threshold = threshold)
        Top_cor_Prot_metab <- Cor_Prot_Metab$Top_cor_Prot_metab
        Correlation_Plot <- Cor_Prot_Metab$Correlation_Plot
        Cor_list <- Cor_Prot_Metab$Cor_list
        edges <- Cor_Prot_Metab$edges
        nodes <- Cor_Prot_Metab$nodes
        n  <- Cor_Prot_Metab$n
        return(list(Top_cor_Prot_metab = Top_cor_Prot_metab,
                    Cor_list = Cor_list,
                    Correlation_Plot = Correlation_Plot,
                    edges = edges,
                    nodes = nodes,
                    n = n))
    })

    output$tableCorrelation <- DT::renderDataTable({
      TopCor = as.data.frame(Cor_Data_n()$Top_cor_Prot_metab)
      nodes <- as.data.frame(Cor_Data_n()$nodes)
      df4from <- merge(TopCor, nodes, by.x = "from", by.y = "id", all.x = TRUE)
      names(df4from)[names(df4from) == "label"] = "# of var into the From_module"
      df4 <- merge(df4from, nodes, by.x = "to", by.y = "id", all.x = TRUE)
      names(df4)[names(df4) == "from"] = "From_module_id"
      names(df4)[names(df4) == "to"] = "To_module_id"
      names(df4)[names(df4) == "value.x"] = "Correlation"
      names(df4)[names(df4) == "label"] = "# of var into the To_module"
      rownames(df4) <- NULL
      df4 = df4[,c("From_module_id", "# of var into the From_module", "To_module_id", "# of var into the To_module", "Correlation")]
      df4 <- df4[order(-abs(df4$Correlation)), ]
      DT::datatable(df4)
    })

    # Render the download handler
    output$downloadOmicsCorrelation <- downloadHandler(
      filename = function() {
        "OmicsCorrelation.csv"
      },
      content = function(file) {
        write.csv(Cor_Data_n()$Top_cor_Prot_metab, file, row.names = TRUE)
      }
    )

    # Create a histogram of correlation
    output$Correlation_plot12 <- renderPlot({
      Correlation_Plot <- Cor_Data_n()$Correlation_Plot

      if (length(Correlation_Plot) == 1) {
        graphics::par(mfrow = c(1, 1))
        hist(Correlation_Plot[[1]], main = "Correlation: Data 1 / Data 2 ")
      } else {
        graphics::par(mfrow = c(1, 3))
        hist(Correlation_Plot[[1]], main = "Correlation: Data 1 / Data 2")
        hist(Correlation_Plot[[2]], main = "Correlation: Data 1 / Data 3")
        hist(Correlation_Plot[[3]], main = "Correlation: Data 2 / Data 3")
        graphics::par(mfrow = c(1, 1))  # Restablecer la disposición de los gráficos
      }
    })


    mynetwork <- reactive({
      requireNamespace("dplyr", quietly = TRUE)

      nodes <- as.data.frame(Cor_Data_n()$nodes)
      edges <- as.data.frame(Cor_Data_n()$edges)
      n <- Cor_Data_n()$n

      shapes <- c("diamond", "triangle", "dot")
      colors <- c("orange", "darkgreen", "darkblue")

      network <- visNetwork::visNetwork(nodes = nodes, edges = edges, width = "100%", height = "800px")
      network <- visNetwork::visLegend(network, useGroups = FALSE, addNodes = data.frame(label = paste0("Data", seq_len(n), " Modules"),
                                                                                            shape = shapes[seq_len(n)], color = colors[seq_len(n)]),
                                       addEdges = data.frame(label = "Correlation", shape = "line", length = 200, color = "darkgreen"))
      network <- visNetwork::visInteraction(network, navigationButtons = TRUE)

      if (input$runEnrichment1) {
        Enriched_Data1 <- Data2_enrich()$cluster_assignments_Data1_enrich
        Enriched_Data1 <- Enriched_Data1[!duplicated(Enriched_Data1$col), ]
        Enriched_Data1$col <- paste0("D1", Enriched_Data1$col)
        nodes <- merge(nodes, Enriched_Data1[, c("col", "enriched_Term")], by.x = "id", by.y = "col", all.x = TRUE)
        nodes$enriched_Term0 <- ifelse(nchar(nodes$enriched_Term) > 20, paste0(substring(nodes$enriched_Term, 1, 20), "..."), nodes$enriched_Term)
        nodes$label <- ifelse(grepl("^D1", nodes$id), paste(nodes$label, nodes$enriched_Term, sep = "\n"), nodes$label)
        nodes <- nodes[, seq_len(min(7, ncol(nodes)))]
      }

      if (input$runEnrichment2) {
        Enriched_Data2 <- Data2_enrich()$cluster_assignments_Data2_enrich
        Enriched_Data2 <- Enriched_Data2[!duplicated(Enriched_Data2$col), ]
        Enriched_Data2$col <- paste0("D2", Enriched_Data2$col)
        nodes <- merge(nodes, Enriched_Data2[, c("col", "enriched_Term")], by.x = "id", by.y = "col", all.x = TRUE)
        nodes$enriched_Term0 <- ifelse(nchar(nodes$enriched_Term) > 20, paste0(substring(nodes$enriched_Term, 1, 20), "..."), nodes$enriched_Term)
        nodes$label <- ifelse(grepl("^D2", nodes$id), paste(nodes$label, nodes$enriched_Term, sep = "\n"), nodes$label)
        nodes <- nodes[, seq_len(min(7, ncol(nodes)))]
      }

      if (input$runEnrichment3) {
        Enriched_Data3 <- Data3_enrich()$cluster_assignments_Data3_enrich
        Enriched_Data3 <- Enriched_Data3[!duplicated(Enriched_Data3$col), ]
        Enriched_Data3$col <- paste0("D3", Enriched_Data3$col)
        nodes <- merge(nodes, Enriched_Data3[, c("col", "enriched_Term")], by.x = "id", by.y = "col", all.x = TRUE)
        nodes$enriched_Term0 <- ifelse(nchar(nodes$enriched_Term) > 20, paste0(substring(nodes$enriched_Term, 1, 20), "..."), nodes$enriched_Term)
        nodes$label <- ifelse(grepl("^D3", nodes$id), paste(nodes$label, nodes$enriched_Term, sep = "\n"), nodes$label)
        nodes <- nodes[, seq_len(min(7, ncol(nodes)))]
      }

      network <- visNetwork::visNetwork(nodes = nodes, edges = edges, width = "100%", height = "800px")
      network <- visNetwork::visLegend(network, useGroups = FALSE, addNodes = data.frame(label = paste0("Data", seq_len(n), " Modules"),
                                                                                         shape = shapes[seq_len(n)], color = colors[seq_len(n)]),
                                       addEdges = data.frame(label = "Correlation", shape = "line", length = 200, color = "darkgreen"))
      network <- visNetwork::visInteraction(network, navigationButtons = TRUE)


      return(network)
    })
    output$network <- visNetwork::renderVisNetwork({
      mynetwork()
    })

    output$downloadNetwork <- downloadHandler(
      filename = function() {
        paste('network-', Sys.Date(), '.html', sep='')
      },
      content = function(con) {
        network <- mynetwork()
        visNetwork::visSave(network, con)
      }
    )

    # Important features Data 1 - Data 2

    observeEvent(input$TopModules_12, {
      n <- input$TopModules_12
      choices <- setNames(as.list(seq_len(n)), paste0("Top_", seq_len(n)))
      updateSelectInput(session, "visualization_list_12", choices = choices)
    })

    pheno_variables_imp_12_1 <- reactive({
      names(metadata())[-which(names(metadata()) == "Sample")]
    })
    observe({
      updateSelectInput(session, "phenotypeSelector_imp_12_1", choices = pheno_variables_imp_12_1())
    })

    pheno_variables_imp_12_2 <- reactive({
      names(metadata())[-which(names(metadata()) == "Sample")]
    })
    observe({
      updateSelectInput(session, "phenotypeSelector_imp_12_2", choices = pheno_variables_imp_12_2())
    })

    variables_Screening_12_1 <- reactive({
      names(Metab_annot())[-which(names(Metab_annot()) == "Feature_ID")]
    })

    observe({
      updateSelectInput(session, "Screening12_1", choices = variables_Screening_12_1())
    })

    variables_Screening_12_2 <- reactive({
      names(Prot_annot())[-which(names(Prot_annot()) == "Feature_ID")]
    })

    observe({
      updateSelectInput(session, "Screening12_2", choices = variables_Screening_12_2())
    })


    ImpVar_D1_D2 <- reactive({
      top_n <- input$TopModules_12
      Cor_Data1_Data2 = as.data.frame(Cor_Data_n()$Cor_list[[1]])
      cluster_assignments_D1 = cluster_assignments_Data1()$cluster_assignments_D1
      cluster_assignments_D2 = cluster_assignments_Data2()$cluster_assignments_D2
      req(load_data1()$feature_mat_t_imp_data)
      load_data1 = load_data1()$feature_mat_t_imp_data
      req(load_data2()$feature_mat_t_imp_data)
      load_data2 = load_data2()$feature_mat_t_imp_data
      ImpVar_Prot_Metab <- FeaturesAnnot_correlation(Cor_Datai_Dataj = Cor_Data1_Data2,
                                                     cluster_assignments_D1 = cluster_assignments_D1,
                                                     cluster_assignments_D2 = cluster_assignments_D2,
                                                     load_data1 = load_data1,
                                                     load_data2 = load_data2,
                                                     top_n = top_n) #$correlation_matrices_list
      return(list(
        Top_correlations = ImpVar_Prot_Metab$Top_correlations,
        cluster_assignments = ImpVar_Prot_Metab$cluster_assignments,
        expression_matrices = ImpVar_Prot_Metab$expression_matrices,
        correlation_matrices = ImpVar_Prot_Metab$correlation_matrices,
        Important_features = ImpVar_Prot_Metab$Important_features,
        correlation_List = ImpVar_Prot_Metab$correlation_List
      ))
    })

    Important_Features12 <- reactive({
      custom_palette <- colorRampPalette(c(RColorBrewer::brewer.pal(11, "RdYlBu")[11], "white", RColorBrewer::brewer.pal(11, "RdYlBu")[1]))(n = 100)

      n <- input$visualization_list_12
      df_list <- list()

      for (i in seq_len(n)) {
        df1_1 <- as.data.frame(ImpVar_D1_D2()$cluster_assignments[[2*i - 1]])
        df1_2 <- as.data.frame(ImpVar_D1_D2()$cluster_assignments[[2*i]])
        df2_1 <- as.data.frame(ImpVar_D1_D2()$expression_matrices[[2*i - 1]])
        df2_2 <- as.data.frame(ImpVar_D1_D2()$expression_matrices[[2*i]])
        df4 <- ImpVar_D1_D2()$correlation_List[[i]]
        df4_1 <- hist(ImpVar_D1_D2()$correlation_matrices[[i]], main = paste("Top", i, "Modules Correlation"))
        df4_2 <- corrplot::corrplot(ImpVar_D1_D2()$correlation_matrices[[i]], type = "upper", tl.col = "black", col = custom_palette)
        df5_1 <- ImpVar_D1_D2()$Important_features[[2*i - 1]]
        df5_2 <- ImpVar_D1_D2()$Important_features[[2*i]]

        df_list[[i]] <- list(df1_1 = df1_1, df1_2 = df1_2, df2_1 = df2_1, df2_2 = df2_2, df4 = df4, df4_1 = df4_1, df4_2 = df4_2, df5_1 = df5_1, df5_2 = df5_2)
      }

      return(df_list)
    })

    output$ImportantVariables_12 <- DT::renderDataTable({
      df4nodes = as.data.frame(ImpVar_D1_D2()$Top_correlations)
      colnames(df4nodes) <- c("From", "# of var into the D1_module", "To", "# of var into the D2_module", "Correlation")
      rownames(df4nodes) <- NULL
      df4nodes <- df4nodes[order(-abs(df4nodes$Correlation)), ]
      DT::datatable(df4nodes)
    })

    output$CorplotImp12 <- renderPlot({
      df_list <- Important_Features12()
      selected_index <- as.numeric(sub("Top_", "", input$visualization_list_12))
      df4_2 <- df_list[[selected_index]]$df4_2
    })

    output$Correlation_mod12 <- DT::renderDataTable({
      df_list <- Important_Features12()
      selected_index <- as.numeric(sub("Top_", "", input$visualization_list_12))
      df4 <- df_list[[selected_index]]$df4
      DT::datatable(data.frame(df4))
    })

    # Render the download handler
    output$downloadModCorrelation12 <- downloadHandler(
      filename = function() {
        "ModulesCorrelation.csv"
      },
      content = function(file) {
        df_list <- Important_Features12()
        selected_index <- as.numeric(sub("Top_", "", input$visualization_list_12))
        df4 <- df_list[[selected_index]]$df4
        write.csv(df4, file, row.names = TRUE)
      }
    )

    output$cluster_assignments_features12_2 <- DT::renderDataTable({
      df_list <- Important_Features12()
      selected_index <- as.numeric(sub("Top_", "", input$visualization_list_12))
      df <- df_list[[selected_index]]$df1_2
      df_features <- dplyr::select(df, feature)
      df_features <- dplyr::distinct(df_features)
      names(df_features)[names(df_features) == "feature"] = "Feature_ID"
      selected_columns <- input$Screening12_2
      if (!is.null(Prot_annot()) && !is.null(selected_columns)) {
        AnnoMeta <- as.data.frame(Prot_annot())
        df_features <- merge(df_features, AnnoMeta[, c("Feature_ID", selected_columns)], by = "Feature_ID", all.x = TRUE)
      }
      DT::datatable(df_features)
    })

    output$cluster_assignments_summary12_2 <- DT::renderDataTable({
      df_list <- Important_Features12()
      selected_index <- as.numeric(sub("Top_", "", input$visualization_list_12))
      df <- df_list[[selected_index]]$df1_2
      cluster <- unique(df$cluster)
      col <- unique(df$col)
      SummaryData <- data.frame(cluster = cluster, col = col, stringsAsFactors = FALSE)

      SummaryData <- dplyr::select(SummaryData, -cluster)
      SummaryData <- dplyr::rename(SummaryData, `Module_id` = col)

      DT::datatable(SummaryData)
    })

    output$downloadcluster_assignments12_2 <- downloadHandler(
      filename = function() {
        "Data1_TopModule.csv"
      },
      content = function(file) {
        df_list <- Important_Features12()
        selected_index <- as.numeric(sub("Top_", "", input$visualization_list_12))
        df1_2 <- df_list[[selected_index]]$df1_2
        write.csv(df1_2, file, row.names = TRUE)
      }
    )

    Classification_imp_12_1 <- reactive({
      df_list <- Important_Features12()
      selected_index <- as.numeric(sub("Top_", "", input$visualization_list_12))
      df2_1 <- as.data.frame(df_list[[selected_index]]$df2_1)
      metadata <- as.data.frame(metadata())
      phenotype_variable = input$phenotypeSelector_imp_12_1
      significance_threshold = input$pValueThreshold_imp_12_1
      Classification_Data <- perform_classification( eigengene_data = df2_1,
                                                           metadata = metadata,
                                                           phenotype_variable = phenotype_variable,
                                                           significance_threshold = significance_threshold)
      return(list(
        result = Classification_Data$result,
        plots = Classification_Data$plots))
    })

    output$classification_results_imp_12_1 <- DT::renderDataTable({
      df <- Classification_imp_12_1()$result
      rownames(df) <- NULL
      names(df)[names(df) == "Variable"] <- "Feature"
      DT::datatable(df)
    })

    output$classification_plot_1_all_imp_12_1 <- renderPlot({
      selected_variable <- input$phenotypeSelector_imp_12_1
      levels_selected_variable <- unique(metadata()[[selected_variable]])
      if (length(levels_selected_variable) < 3) {
        class_names <- levels_selected_variable
        class_label <- paste(class_names, collapse = " vs ")
        plot <- Classification_imp_12_1()$plots[[1]]
        plot <- plot +
          ggplot2::labs(title = class_label, fill = as.factor(levels_selected_variable),
                        x = "Variables",
                        y = "Class") +
          ggplot2::theme(
            axis.text.x = ggplot2::element_text(angle = 90, hjust = 1)
          )
        classification_Metab_imp(plot)
        return(plot)
      } else {
        # Print multiple boxplot charts, one for each level of the selected variable
        plots_list <- lapply(seq_along(levels_selected_variable), function(i) {
          Classification_imp_12_1()$plots[[i]]
        })
        plot <- cowplot::plot_grid(plotlist = plots_list)
        classification_Metab_imp(plot)  # Store the plot in the reactive variable
        return(plot)
      }
    })

    output$cluster_assignments_features12_1 <- DT::renderDataTable({
      df_list <- Important_Features12()
      selected_index <- as.numeric(sub("Top_", "", input$visualization_list_12))
      df <- df_list[[selected_index]]$df1_1
      df_features <- dplyr::select(df, feature)
      df_features <- dplyr::distinct(df_features)
      names(df_features)[names(df_features) == "feature"] = "Feature_ID"
      selected_columns <- input$Screening12_1
      if (!is.null(Metab_annot()) && !is.null(selected_columns)) {
        AnnoMeta <- as.data.frame(Metab_annot())
        df_features <- merge(df_features, AnnoMeta[, c("Feature_ID", selected_columns)], by = "Feature_ID", all.x = TRUE)
      }
      DT::datatable(df_features)
    })

    output$cluster_assignments_summary12_1 <- DT::renderDataTable({
      df_list <- Important_Features12()
      selected_index <- as.numeric(sub("Top_", "", input$visualization_list_12))
      df <- df_list[[selected_index]]$df1_1
      cluster <- unique(df$cluster)
      col <- unique(df$col)
      df_summary <- data.frame(cluster = cluster, col = col, stringsAsFactors = FALSE)
      df_summary <- dplyr::select(df_summary, -cluster)
      df_summary <- dplyr::rename(df_summary, `Module_id` = col)
      DT::datatable(df_summary)
    })

    # Render the download handler
    output$downloadcluster_assignments12_1 <- downloadHandler(
      filename = function() {
        "Data2_TopModule.csv"
      },
      content = function(file) {
        df_list <- Important_Features12()
        selected_index <- as.numeric(sub("Top_", "", input$visualization_list_12))
        df1_1 <- df_list[[selected_index]]$df1_1
        write.csv(df1_1, file, row.names = TRUE)
      }
    )

    Classification_imp_12_2 <- reactive({
      df_list <- Important_Features12()
      selected_index <- as.numeric(sub("Top_", "", input$visualization_list_12))
      df2_2 <- df_list[[selected_index]]$df2_2
      metadata <- as.data.frame(metadata())
      phenotype_variable = input$phenotypeSelector_imp_12_2
      significance_threshold = input$pValueThreshold_imp_12_2
      Classification_Data <- perform_classification( eigengene_data = df2_2,
                                                           metadata = metadata,
                                                           phenotype_variable = phenotype_variable,
                                                           significance_threshold = significance_threshold)
      return(list(
        result = Classification_Data$result,
        plots = Classification_Data$plots))
    })

    output$classification_results_imp_12_2 <- DT::renderDataTable({
      df <- Classification_imp_12_2()$result
      rownames(df) <- NULL
      names(df)[names(df) == "Variable"] = "Feature"
      DT::datatable(df)
    })

    output$classification_plot_1_all_imp_12_2 <- renderPlot({
      selected_variable <- input$phenotypeSelector_imp_12_2
      levels_selected_variable <- unique(metadata()[[selected_variable]])
      if (length(levels_selected_variable) < 3) {
        class_names <- levels_selected_variable
        class_label <- paste(class_names, collapse = " vs ")
        plot <- Classification_imp_12_2()$plots[[1]]
        plot <- plot +
          ggplot2::labs(title = class_label, fill = as.factor(levels_selected_variable),
                        x = "Variables",
                        y = "Class") +
          ggplot2::theme(
            axis.text.x = ggplot2::element_text(angle = 90, hjust = 1)
          )
        classification_Prot_imp(plot)
        return(plot)
      } else {
        # Print multiple boxplot charts, one for each level of the selected variable
        plots_list <- lapply(seq_along(levels_selected_variable), function(i) {
          Classification_imp_12_2()$plots[[i]]
        })
        plot <- cowplot::plot_grid(plotlist = plots_list)
        classification_Prot_imp(plot)  # Store the plot in the reactive variable
        return(plot)
      }
    })

    output$Important_features_12_1 <- renderText({
      df_list <- Important_Features12()
      selected_index <- as.numeric(sub("Top_", "", input$visualization_list_12))
      df5_1 <- df_list[[selected_index]]$df5_1
    })

    output$Important_features_12_2 <- renderText({
      df_list <- Important_Features12()
      selected_index <- as.numeric(sub("Top_", "", input$visualization_list_12))
      df5_2 <- df_list[[selected_index]]$df5_2
    })

    # Important features Data 1 - Data 3

    observeEvent(input$TopModules_13, {
      n <- input$TopModules_13
      choices <- setNames(as.list(seq_len(n)), paste0("Top_", seq_len(n)))
      updateSelectInput(session, "visualization_list_13", choices = choices)
    })

    pheno_variables_imp_13_1 <- reactive({
      names(metadata())[-which(names(metadata()) == "Sample")]
    })
    observe({
      updateSelectInput(session, "phenotypeSelector_imp_13_1", choices = pheno_variables_imp_13_1())
    })

    pheno_variables_imp_13_3 <- reactive({
      names(metadata())[-which(names(metadata()) == "Sample")]
    })
    observe({
      updateSelectInput(session, "phenotypeSelector_imp_13_3", choices = pheno_variables_imp_13_3())
    })

    variables_Screening_13_1 <- reactive({
      names(Metab_annot())[-which(names(Metab_annot()) == "Feature_ID")]
    })

    observe({
      updateSelectInput(session, "Screening13_1", choices = variables_Screening_13_1())
    })

    variables_Screening_13_3 <- reactive({
      names(Gene_annot())[-which(names(Gene_annot()) == "Feature_ID")]
    })

    observe({
      updateSelectInput(session, "Screening13_3", choices = variables_Screening_13_3())
    })


    ImpVar_D1_D3 <- reactive({
      top_n <- input$TopModules_13
      Cor_Data1_Data3 = as.data.frame(Cor_Data_n()$Cor_list[[2]])
      cluster_assignments_D1 = cluster_assignments_Data1()$cluster_assignments_D1
      cluster_assignments_D3 = cluster_assignments_Data3()$cluster_assignments_D3
      req(load_data1()$feature_mat_t_imp_data)
      load_data1 = load_data1()$feature_mat_t_imp_data
      req(load_data3()$feature_mat_t_imp_data)
      load_data3 = load_data3()$feature_mat_t_imp_data
      ImpVar_Prot_Metab <- FeaturesAnnot_correlation(Cor_Datai_Dataj = Cor_Data1_Data3,
                                                     cluster_assignments_D1 = cluster_assignments_D1,
                                                     cluster_assignments_D2 = cluster_assignments_D3,
                                                     load_data1 = load_data1,
                                                     load_data2 = load_data3,
                                                     top_n = top_n) #$correlation_matrices_list
      return(list(
        Top_correlations = ImpVar_Prot_Metab$Top_correlations,
        cluster_assignments = ImpVar_Prot_Metab$cluster_assignments,
        expression_matrices = ImpVar_Prot_Metab$expression_matrices,
        correlation_matrices = ImpVar_Prot_Metab$correlation_matrices,
        Important_features = ImpVar_Prot_Metab$Important_features,
        correlation_List = ImpVar_Prot_Metab$correlation_List
      ))
    })

    Important_Features13 <- reactive({
      custom_palette <- colorRampPalette(c(RColorBrewer::brewer.pal(11, "RdYlBu")[11], "white", RColorBrewer::brewer.pal(11, "RdYlBu")[1]))(n = 100)

      n <- input$visualization_list_13
      df_list <- list()

      for (i in seq_len(n)) {
        df1_1 <- as.data.frame(ImpVar_D1_D3()$cluster_assignments[[2*i - 1]])
        df1_2 <- as.data.frame(ImpVar_D1_D3()$cluster_assignments[[2*i]])
        df2_1 <- as.data.frame(ImpVar_D1_D3()$expression_matrices[[2*i - 1]])
        df2_2 <- as.data.frame(ImpVar_D1_D3()$expression_matrices[[2*i]])
        df4 <- ImpVar_D1_D3()$correlation_List[[i]]
        df4_1 <- hist(ImpVar_D1_D3()$correlation_matrices[[i]], main = paste("Top", i, "Modules Correlation"))
        df4_2 <- corrplot::corrplot(ImpVar_D1_D3()$correlation_matrices[[i]], type = "upper", tl.col = "black", col = custom_palette)
        df5_1 <- ImpVar_D1_D3()$Important_features[[2*i - 1]]
        df5_2 <- ImpVar_D1_D3()$Important_features[[2*i]]

        df_list[[i]] <- list(df1_1 = df1_1, df1_2 = df1_2, df2_1 = df2_1, df2_2 = df2_2, df4 = df4, df4_1 = df4_1, df4_2 = df4_2, df5_1 = df5_1, df5_2 = df5_2)
      }
      return(df_list)
    })

    output$ImportantVariables_13 <- DT::renderDataTable({
      if (is.null(Gene_exp())) {
        return(NULL)
      } else {
        df4nodes = as.data.frame(ImpVar_D1_D3()$Top_correlations)
        colnames(df4nodes) <- c("From", "# of var into the D1_module", "To", "# of var into the D2_module", "Correlation")
        rownames(df4nodes) <- NULL
        df4nodes <- df4nodes[order(-abs(df4nodes$Correlation)), ]
        DT::datatable(df4nodes)
      }
    })

    output$CorplotImp13 <- renderPlot({
      if (is.null(Gene_exp())) {
        return(NULL)
      } else {
      df_list <- Important_Features13()
      selected_index <- as.numeric(sub("Top_", "", input$visualization_list_13))
      df4_2 <- df_list[[selected_index]]$df4_2
      }
    })

    output$Correlation_mod13 <- DT::renderDataTable({
      if (is.null(Gene_exp())) {
        return(NULL)
      } else {
      df_list <- Important_Features13()
      selected_index <- as.numeric(sub("Top_", "", input$visualization_list_13))
      df4 <- df_list[[selected_index]]$df4
      names(df4)[names(df4) == "Data2"] = "Data3"
      DT::datatable(data.frame(df4))
      }
    })

    # Render the download handler
    output$downloadModCorrelation13 <- downloadHandler(
      filename = function() {
        "ModulesCorrelation.csv"
      },
      content = function(file) {
        df_list <- Important_Features13()
        selected_index <- as.numeric(sub("Top_", "", input$visualization_list_13))
        df4 <- df_list[[selected_index]]$df4
        write.csv(df4, file, row.names = TRUE)
      }
    )

    output$cluster_assignments_features13_3 <- DT::renderDataTable({
      if (is.null(Gene_exp())) {
        return(NULL)
      } else {
      df_list <- Important_Features13()
      selected_index <- as.numeric(sub("Top_", "", input$visualization_list_13))
      df <- df_list[[selected_index]]$df1_2
      df_features <- dplyr::select(df, feature)
      df_features <- dplyr::distinct(df_features)
      names(df_features)[names(df_features) == "feature"] = "Feature_ID"
      selected_columns <- input$Screening13_3
      if (!is.null(Gene_annot()) && !is.null(selected_columns)) {
        AnnoMeta <- as.data.frame(Gene_annot())
        df_features <- merge(df_features, AnnoMeta[, c("Feature_ID", selected_columns)], by = "Feature_ID", all.x = TRUE)
      }
      DT::datatable(df_features)
      }
    })

    output$cluster_assignments_summary13_3 <- DT::renderDataTable({
      if (is.null(Gene_exp())) {
        return(NULL)
      } else {
      df_list <- Important_Features13()
      selected_index <- as.numeric(sub("Top_", "", input$visualization_list_13))
      df <- df_list[[selected_index]]$df1_2
      cluster <- unique(df$cluster)
      col <- unique(df$col)
      SummaryData <- data.frame(cluster = cluster, col = col, stringsAsFactors = FALSE)

      SummaryData <- dplyr::select(SummaryData, -cluster)
      SummaryData <- dplyr::rename(SummaryData, `Module_id` = col)

      DT::datatable(SummaryData)
      }
    })

    output$downloadcluster_assignments13_3 <- downloadHandler(
      filename = function() {
        "Data1_TopModule.csv"
      },
      content = function(file) {
        df_list <- Important_Features13()
        selected_index <- as.numeric(sub("Top_", "", input$visualization_list_13))
        df1_2 <- df_list[[selected_index]]$df1_2
        write.csv(df1_2, file, row.names = TRUE)
      }
    )

    Classification_imp_13_1 <- reactive({
      df_list <- Important_Features13()
      selected_index <- as.numeric(sub("Top_", "", input$visualization_list_13))
      df2_1 <- as.data.frame(df_list[[selected_index]]$df2_1)
      metadata <- as.data.frame(metadata())
      phenotype_variable = input$phenotypeSelector_imp_13_1
      significance_threshold = input$pValueThreshold_imp_13_1
      Classification_Data <- perform_classification( eigengene_data = df2_1,
                                                           metadata = metadata,
                                                           phenotype_variable = phenotype_variable,
                                                           significance_threshold = significance_threshold)
      return(list(
        result = Classification_Data$result,
        plots = Classification_Data$plots))
    })

    output$classification_results_imp_13_1 <- DT::renderDataTable({
      if (is.null(Gene_exp())) {
        return(NULL)
      } else {
      df <- Classification_imp_13_1()$result
      rownames(df) <- NULL
      names(df)[names(df) == "Variable"] = "Feature"
      DT::datatable(df)
      }
    })

    output$classification_plot_1_all_imp_13_1 <- renderPlot({
      selected_variable <- input$phenotypeSelector_imp_13_1
      levels_selected_variable <- unique(metadata()[[selected_variable]])
      if (length(levels_selected_variable) < 3) {
        class_names <- levels_selected_variable
        class_label <- paste(class_names, collapse = " vs ")
        plot <- Classification_imp_13_1()$plots[[1]]
        plot <- plot +
          ggplot2::labs(title = class_label, fill = as.factor(levels_selected_variable),
                        x = "Variables",
                        y = "Class") +
          ggplot2::theme(
            axis.text.x = ggplot2::element_text(angle = 90, hjust = 1)
          )
        classification_Metab_imp(plot)
        return(plot)
      } else {
        # Print multiple boxplot charts, one for each level of the selected variable
        plots_list <- lapply(seq_len(levels_selected_variable), function(i) {
          Classification_imp_13_1()$plots[[i]]
        })
        plot <- cowplot::plot_grid(plotlist = plots_list)
        classification_Metab_imp(plot)  # Store the plot in the reactive variable
        return(plot)
      }
    })

    output$cluster_assignments_features13_1 <- DT::renderDataTable({
      if (is.null(Gene_exp())) {
        return(NULL)
      } else {
      df_list <- Important_Features13()
      selected_index <- as.numeric(sub("Top_", "", input$visualization_list_13))
      df <- df_list[[selected_index]]$df1_1
      df_features <- dplyr::select(df, feature)
      df_features <- dplyr::distinct(df_features)
      names(df_features)[names(df_features) == "feature"] = "Feature_ID"
      selected_columns <- input$Screening13_1
      if (!is.null(Metab_annot()) && !is.null(selected_columns)) {
        AnnoMeta <- as.data.frame(Metab_annot())
        df_features <- merge(df_features, AnnoMeta[, c("Feature_ID", selected_columns)], by = "Feature_ID", all.x = TRUE)
      }
      DT::datatable(df_features)
      }
    })

    output$cluster_assignments_summary13_1 <- DT::renderDataTable({
      if (is.null(Gene_exp())) {
        return(NULL)
      } else {
      df_list <- Important_Features13()
      selected_index <- as.numeric(sub("Top_", "", input$visualization_list_13))
      df <- df_list[[selected_index]]$df1_1
      cluster <- unique(df$cluster)
      col <- unique(df$col)
      df_summary <- data.frame(cluster = cluster, col = col, stringsAsFactors = FALSE)
      df_summary <- dplyr::select(df_summary, -cluster)
      df_summary <- dplyr::rename(df_summary, `Module_id` = col)
      DT::datatable(df_summary)
      }
    })

    # Render the download handler
    output$downloadcluster_assignments13_1 <- downloadHandler(
      filename = function() {
        "Data3_TopModule.csv"
      },
      content = function(file) {
        df_list <- Important_Features13()
        selected_index <- as.numeric(sub("Top_", "", input$visualization_list_13))
        df1_1 <- df_list[[selected_index]]$df1_1
        write.csv(df1_1, file, row.names = TRUE)
      }
    )

    Classification_imp_13_3 <- reactive({
      df_list <- Important_Features13()
      selected_index <- as.numeric(sub("Top_", "", input$visualization_list_13))
      df2_2 <- df_list[[selected_index]]$df2_2
      metadata <- as.data.frame(metadata())
      phenotype_variable = input$phenotypeSelector_imp_13_3
      significance_threshold = input$pValueThreshold_imp_13_3
      Classification_Data <- perform_classification( eigengene_data = df2_2,
                                                           metadata = metadata,
                                                           phenotype_variable = phenotype_variable,
                                                           significance_threshold = significance_threshold)
      return(list(
        result = Classification_Data$result,
        plots = Classification_Data$plots))
    })

    output$classification_results_imp_13_3 <- DT::renderDataTable({
      if (is.null(Gene_exp())) {
        return(NULL)
      } else {
      df <- Classification_imp_13_3()$result
      rownames(df) <- NULL
      names(df)[names(df) == "Variable"] = "Feature"
      DT::datatable(df)
      }
    })

    output$classification_plot_1_all_imp_13_3 <- renderPlot({
      selected_variable <- input$phenotypeSelector_imp_13_3
      levels_selected_variable <- unique(metadata()[[selected_variable]])
      if (length(levels_selected_variable) < 3) {
        class_names <- levels_selected_variable
        class_label <- paste(class_names, collapse = " vs ")
        plot <- Classification_imp_13_3()$plots[[1]]
        plot <- plot +
          ggplot2::labs(title = class_label, fill = as.factor(levels_selected_variable),
                        x = "Variables",
                        y = "Class") +
          ggplot2::theme(
            axis.text.x = ggplot2::element_text(angle = 90, hjust = 1)
          )
        classification_Gene_imp(plot)
        return(plot)
      } else {
        # Print multiple boxplot charts, one for each level of the selected variable
        plots_list <- lapply(seq_len(levels_selected_variable), function(i) {
          Classification_imp_13_3()$plots[[i]]
        })
        plot <- cowplot::plot_grid(plotlist = plots_list)
        classification_Gene_imp(plot)  # Store the plot in the reactive variable
        return(plot)
      }
    })

    output$Important_features_13_1 <- renderText({
      if (is.null(Gene_exp())) {
        return(NULL)
      } else {
      df_list <- Important_Features13()
      selected_index <- as.numeric(sub("Top_", "", input$visualization_list_13))
      df5_1 <- df_list[[selected_index]]$df5_1
      }
    })

    output$Important_features_13_3 <- renderText({
      if (is.null(Gene_exp())) {
        return(NULL)
      } else {
      df_list <- Important_Features13()
      selected_index <- as.numeric(sub("Top_", "", input$visualization_list_13))
      df5_2 <- df_list[[selected_index]]$df5_2
      }
    })

    # Important features Data 2 - Data 3

    observeEvent(input$TopModules_23, {
      n <- input$TopModules_23
      choices <- setNames(as.list(seq_len(n)), paste0("Top_", seq_len(n)))
      updateSelectInput(session, "visualization_list_23", choices = choices)
    })

    pheno_variables_imp_23_2 <- reactive({
      names(metadata())[-which(names(metadata()) == "Sample")]
    })
    observe({
      updateSelectInput(session, "phenotypeSelector_imp_23_2", choices = pheno_variables_imp_23_2())
    })

    pheno_variables_imp_23_3 <- reactive({
      names(metadata())[-which(names(metadata()) == "Sample")]
    })
    observe({
      updateSelectInput(session, "phenotypeSelector_imp_23_3", choices = pheno_variables_imp_23_3())
    })

    variables_Screening_23_2 <- reactive({
      names(Prot_annot())[-which(names(Prot_annot()) == "Feature_ID")]
    })

    observe({
      updateSelectInput(session, "Screening23_2", choices = variables_Screening_23_2())
    })

    variables_Screening_23_3 <- reactive({
      names(Gene_annot())[-which(names(Gene_annot()) == "Feature_ID")]
    })

    observe({
      updateSelectInput(session, "Screening23_3", choices = variables_Screening_23_3())
    })


    ImpVar_D2_D3 <- reactive({
      top_n <- input$TopModules_23
      Cor_Data2_Data3 = as.data.frame(Cor_Data_n()$Cor_list[[3]])
      cluster_assignments_D2 = cluster_assignments_Data2()$cluster_assignments_D2
      cluster_assignments_D3 = cluster_assignments_Data3()$cluster_assignments_D3
      req(load_data2()$feature_mat_t_imp_data)
      load_data2 = load_data2()$feature_mat_t_imp_data
      req(load_data3()$feature_mat_t_imp_data)
      load_data3 = load_data3()$feature_mat_t_imp_data
      ImpVar_Prot_Metab <- FeaturesAnnot_correlation(Cor_Datai_Dataj = Cor_Data2_Data3,
                                                     cluster_assignments_D1 = cluster_assignments_D2,
                                                     cluster_assignments_D2 = cluster_assignments_D3,
                                                     load_data1 = load_data2,
                                                     load_data2 = load_data3,
                                                     top_n = top_n) #$correlation_matrices_list
      return(list(
        Top_correlations = ImpVar_Prot_Metab$Top_correlations,
        cluster_assignments = ImpVar_Prot_Metab$cluster_assignments,
        expression_matrices = ImpVar_Prot_Metab$expression_matrices,
        correlation_matrices = ImpVar_Prot_Metab$correlation_matrices,
        Important_features = ImpVar_Prot_Metab$Important_features,
        correlation_List = ImpVar_Prot_Metab$correlation_List
      ))
    })

    Important_Features23 <- reactive({
      custom_palette <- colorRampPalette(c(RColorBrewer::brewer.pal(11, "RdYlBu")[11], "white", RColorBrewer::brewer.pal(11, "RdYlBu")[1]))(n = 100)

      n <- input$visualization_list_23
      df_list <- list()

      for (i in seq_len(n)) {
        df1_1 <- as.data.frame(ImpVar_D2_D3()$cluster_assignments[[2*i - 1]])
        df1_2 <- as.data.frame(ImpVar_D2_D3()$cluster_assignments[[2*i]])
        df2_1 <- as.data.frame(ImpVar_D2_D3()$expression_matrices[[2*i - 1]])
        df2_2 <- as.data.frame(ImpVar_D2_D3()$expression_matrices[[2*i]])
        df4 <- ImpVar_D2_D3()$correlation_List[[i]]
        df4_1 <- hist(ImpVar_D2_D3()$correlation_matrices[[i]], main = paste("Top", i, "Modules Correlation"))
        df4_2 <- corrplot::corrplot(ImpVar_D2_D3()$correlation_matrices[[i]], type = "upper", tl.col = "black", col = custom_palette)
        df5_1 <- ImpVar_D2_D3()$Important_features[[2*i - 1]]
        df5_2 <- ImpVar_D2_D3()$Important_features[[2*i]]

        df_list[[i]] <- list(df1_1 = df1_1, df1_2 = df1_2, df2_1 = df2_1, df2_2 = df2_2, df4 = df4, df4_1 = df4_1, df4_2 = df4_2, df5_1 = df5_1, df5_2 = df5_2)
      }

      return(df_list)
    })

    output$ImportantVariables_23 <- DT::renderDataTable({
      if (is.null(Gene_exp())) {
        return(NULL)
      } else {
        df4nodes = as.data.frame(ImpVar_D2_D3()$Top_correlations)
        colnames(df4nodes) <- c("From", "# of var into the D1_module", "To", "# of var into the D2_module", "Correlation")
        rownames(df4nodes) <- NULL
        df4nodes <- df4nodes[order(-abs(df4nodes$Correlation)), ]
        DT::datatable(df4nodes)
      }
    })

    output$CorplotImp23 <- renderPlot({
      if (is.null(Gene_exp())) {
        return(NULL)
      } else {
      df_list <- Important_Features23()
      selected_index <- as.numeric(sub("Top_", "", input$visualization_list_23))
      df4_2 <- df_list[[selected_index]]$df4_2
      }
    })

    output$Correlation_mod23 <- DT::renderDataTable({
      if (is.null(Gene_exp())) {
        return(NULL)
      } else {
      df_list <- Important_Features23()
      selected_index <- as.numeric(sub("Top_", "", input$visualization_list_23))
      df4 <- df_list[[selected_index]]$df4
      names(df4)[names(df4) == "Data2"] = "Data3"
      names(df4)[names(df4) == "Data1"] = "Data2"
      DT::datatable(data.frame(df4))
      }
    })

    # Render the download handler
    output$downloadModCorrelation23 <- downloadHandler(
      filename = function() {
        "ModulesCorrelation.csv"
      },
      content = function(file) {
        df_list <- Important_Features23()
        selected_index <- as.numeric(sub("Top_", "", input$visualization_list_23))
        df4 <- df_list[[selected_index]]$df4
        write.csv(df4, file, row.names = TRUE)
      }
    )

    output$cluster_assignments_features23_3 <- DT::renderDataTable({
      if (is.null(Gene_exp())) {
        return(NULL)
      } else {
      df_list <- Important_Features23()
      selected_index <- as.numeric(sub("Top_", "", input$visualization_list_23))
      df <- df_list[[selected_index]]$df1_2
      df_features <- dplyr::select(df, feature)
      df_features <- dplyr::distinct(df_features)
      names(df_features)[names(df_features) == "feature"] = "Feature_ID"
      selected_columns <- input$Screening23_3
      if (!is.null(Gene_annot()) && !is.null(selected_columns)) {
        AnnoMeta <- as.data.frame(Gene_annot())
        df_features <- merge(df_features, AnnoMeta[, c("Feature_ID", selected_columns)], by = "Feature_ID", all.x = TRUE)
      }
      DT::datatable(df_features)
      }
    })

    output$cluster_assignments_summary23_3 <- DT::renderDataTable({
      if (is.null(Gene_exp())) {
        return(NULL)
      } else {
      df_list <- Important_Features23()
      selected_index <- as.numeric(sub("Top_", "", input$visualization_list_23))
      df <- df_list[[selected_index]]$df1_2
      cluster <- unique(df$cluster)
      col <- unique(df$col)
      SummaryData <- data.frame(cluster = cluster, col = col, stringsAsFactors = FALSE)
      SummaryData <- dplyr::select(SummaryData, -cluster)
      SummaryData <- dplyr::rename(SummaryData, `Module_id` = col)
      DT::datatable(SummaryData)
      }
    })

    output$downloadcluster_assignments23_3 <- downloadHandler(
      filename = function() {
        "Data2_TopModule.csv"
      },
      content = function(file) {
        df_list <- Important_Features23()
        selected_index <- as.numeric(sub("Top_", "", input$visualization_list_23))
        df1_2 <- df_list[[selected_index]]$df1_2
        write.csv(df1_2, file, row.names = TRUE)
      }
    )

    Classification_imp_23_2 <- reactive({
      df_list <- Important_Features23()
      selected_index <- as.numeric(sub("Top_", "", input$visualization_list_23))
      df2_1 <- as.data.frame(df_list[[selected_index]]$df2_1)
      metadata <- as.data.frame(metadata())
      phenotype_variable = input$phenotypeSelector_imp_23_2
      significance_threshold = input$pValueThreshold_imp_23_2
      Classification_Data <- perform_classification( eigengene_data = df2_1,
                                                           metadata = metadata,
                                                           phenotype_variable = phenotype_variable,
                                                           significance_threshold = significance_threshold)
      return(list(
        result = Classification_Data$result,
        plots = Classification_Data$plots))
    })

    output$classification_results_imp_23_2 <- DT::renderDataTable({
      if (is.null(Gene_exp())) {
        return(NULL)
      } else {
      df <- Classification_imp_23_2()$result
      rownames(df) <- NULL
      names(df)[names(df) == "Variable"] <- "Feature"
      DT::datatable(df)
      }
    })

    output$classification_plot_1_all_imp_23_2 <- renderPlot({
      selected_variable <- input$phenotypeSelector_imp_23_2
      levels_selected_variable <- unique(metadata()[[selected_variable]])
      if (length(levels_selected_variable) < 3) {
        class_names <- levels_selected_variable
        class_label <- paste(class_names, collapse = " vs ")
        plot <- Classification_imp_23_2()$plots[[1]]
        plot <- plot +
          ggplot2::labs(title = class_label, fill = as.factor(levels_selected_variable),
                        x = "Variables",
                        y = "Class") +
          ggplot2::theme(
            axis.text.x = ggplot2::element_text(angle = 90, hjust = 1)
          )
        classification_Prot_imp(plot)
        return(plot)
      } else {
        # Print multiple boxplot charts, one for each level of the selected variable
        plots_list <- lapply(seq_along(levels_selected_variable), function(i) {
          Classification_imp_23_2()$plots[[i]]
        })
        plot <- cowplot::plot_grid(plotlist = plots_list)
        classification_Prot_imp(plot)  # Store the plot in the reactive variable
        return(plot)
      }
    })

    output$cluster_assignments_features23_2 <- DT::renderDataTable({
      if (is.null(Gene_exp())) {
        return(NULL)
      } else {
      df_list <- Important_Features23()
      selected_index <- as.numeric(sub("Top_", "", input$visualization_list_23))
      df <- df_list[[selected_index]]$df1_1
      df_features <- dplyr::select(df, feature)
      df_features <- dplyr::distinct(df_features)
      names(df_features)[names(df_features) == "feature"] <- "Feature_ID"
      selected_columns <- input$Screening23_2
      if (!is.null(Prot_annot()) && !is.null(selected_columns)) {
        AnnoMeta <- as.data.frame(Prot_annot())
        df_features <- merge(df_features, AnnoMeta[, c("Feature_ID", selected_columns)], by = "Feature_ID", all.x = TRUE)
      }
      DT::datatable(df_features)
      }
    })

    output$cluster_assignments_summary23_2 <- DT::renderDataTable({
      if (is.null(Gene_exp())) {
        return(NULL)
      } else {
      df_list <- Important_Features23()
      selected_index <- as.numeric(sub("Top_", "", input$visualization_list_23))
      df <- df_list[[selected_index]]$df1_1
      cluster <- unique(df$cluster)
      col <- unique(df$col)
      df_summary <- data.frame(cluster = cluster, col = col, stringsAsFactors = FALSE)
      df_summary <- dplyr::select(df_summary, -cluster)
      df_summary <- dplyr::rename(df_summary, `Module_id` = col)
      DT::datatable(df_summary)
      }
    })

    # Render the download handler
    output$downloadcluster_assignments23_2 <- downloadHandler(
      filename = function() {
        "Data3_TopModule.csv"
      },
      content = function(file) {
        df_list <- Important_Features23()
        selected_index <- as.numeric(sub("Top_", "", input$visualization_list_23))
        df1_1 <- df_list[[selected_index]]$df1_1
        write.csv(df1_1, file, row.names = TRUE)
      }
    )

    Classification_imp_23_3 <- reactive({
      df_list <- Important_Features23()
      selected_index <- as.numeric(sub("Top_", "", input$visualization_list_23))
      df2_2 <- df_list[[selected_index]]$df2_2
      metadata <- as.data.frame(metadata())
      phenotype_variable <- input$phenotypeSelector_imp_23_3
      significance_threshold <- input$pValueThreshold_imp_23_3
      Classification_Data <- perform_classification( eigengene_data = df2_2,
                                                           metadata = metadata,
                                                           phenotype_variable = phenotype_variable,
                                                           significance_threshold = significance_threshold)
      return(list(
        result = Classification_Data$result,
        plots = Classification_Data$plots))
    })

    output$classification_results_imp_23_3 <- DT::renderDataTable({
      if (is.null(Gene_exp())) {
        return(NULL)
      } else {
      df <- Classification_imp_23_3()$result
      rownames(df) <- NULL
      names(df)[names(df) == "Variable"] <- "Feature"
      DT::datatable(df)
      }
    })

    output$classification_plot_1_all_imp_23_3 <- renderPlot({
      selected_variable <- input$phenotypeSelector_imp_23_3
      levels_selected_variable <- unique(metadata()[[selected_variable]])
      if (length(levels_selected_variable) < 3) {
        class_names <- levels_selected_variable
        class_label <- paste(class_names, collapse = " vs ")
        plot <- Classification_imp_23_3()$plots[[1]]
        plot <- plot +
          ggplot2::labs(title = class_label, fill = as.factor(levels_selected_variable),
                        x = "Variables",
                        y = "Class") +
          ggplot2::theme(
            axis.text.x = ggplot2::element_text(angle = 90, hjust = 1)
          )
        classification_Gene_imp(plot)
        return(plot)
      } else {
        # Print multiple boxplot charts, one for each level of the selected variable
        plots_list <- lapply(seq_along(levels_selected_variable), function(i) {
          Classification_imp_23_3()$plots[[i]]
        })
        plot <- cowplot::plot_grid(plotlist = plots_list)
        classification_Gene_imp(plot)  # Store the plot in the reactive variable
        return(plot)
      }
    })

    output$Important_features_23_2 <- renderText({
      if (is.null(Gene_exp())) {
        return(NULL)
      } else {
      df_list <- Important_Features23()
      selected_index <- as.numeric(sub("Top_", "", input$visualization_list_23))
      df5_1 <- df_list[[selected_index]]$df5_1
      }
    })

    output$Important_features_23_3 <- renderText({
      if (is.null(Gene_exp())) {
        return(NULL)
      } else {
      df_list <- Important_Features23()
      selected_index <- as.numeric(sub("Top_", "", input$visualization_list_23))
      df5_2 <- df_list[[selected_index]]$df5_2
      }
    })

    # Demodata
    #ccRCC

    observeEvent(input$runDemo, {
      withProgress(message = 'Loading example data...', value = 0, {
        demo_data_path <- paste0(here::here(),"/inst/Example_data/ccRCC4_Data")
        file_path_metab_exp <- file.path(demo_data_path, "Metab_exp.rds")
        file_path_metab_annot <- file.path(demo_data_path, "Metab_annot.rds")
        file_path_RNA_exp <- file.path(demo_data_path, "RNA_exp.rds")
        file_path_RNA_annot <- file.path(demo_data_path, "RNA_annot.rds")
        file_path_metadata <- file.path(demo_data_path, "Metadata.rds")

        precargados_metab_exp <- readRDS(file_path_metab_exp)
        precargados_metab_annot <- readRDS(file_path_metab_annot)
        precargados_RNA_exp <- readRDS(file_path_RNA_exp)
        precargados_RNA_annot <- readRDS(file_path_RNA_annot)
        precargados_metadata <- readRDS(file_path_metadata)

        incProgress(0, detail = 'Loading Metab_exp.csv')
        Metab_exp(precargados_metab_exp)
        Sys.sleep(5)

        incProgress(10, detail = 'Loading Metab_annot.csv')
        Metab_annot(precargados_metab_annot)
        Sys.sleep(3)

        incProgress(20, detail = 'Loading Prot_exp.csv')
        Prot_exp(precargados_RNA_exp)
        Sys.sleep(6)

        incProgress(30, detail = 'Loading Prot_annot.csv')
        Prot_annot(precargados_RNA_annot)
        Sys.sleep(3)

        incProgress(50, detail = 'Loading Metadata.csv...')
        metadata(precargados_metadata)
        Sys.sleep(2)

        demo_par_cor_Metab(TRUE)
        demo_par_cor_Metab_All(FALSE)
        demo_par_cor_Prot(TRUE)
        demo_par_cor_Prot_All(FALSE)
        demo_enrich_Prot(TRUE)

        demo_enrich_Prot_All(FALSE)


        updateSelectInput(session, "Mapping1", choices = variables_mapping1(), selected = c("Metabolite", "KEGG"))
        updateSelectInput(session, "Screening1", choices = variables_Screening1(), selected = c("Metabolite", "KEGG"))
        updateSelectInput(session, "Mapping2", choices = variables_mapping2(), selected = "Symbol")
        updateCheckboxInput(session, "runEnrichment2", value = TRUE)
        updateSelectInput(session, "Screening2", choices = variables_Screening2(), selected = "Symbol")
        updateSelectInput(session, "Screening12_1", choices = variables_Screening_12_1(), selected = c("Metabolite", "KEGG"))
        updateSelectInput(session, "Screening12_2", choices = variables_Screening_12_2(), selected = "Symbol")

        updateSelectInput(session, "databaseSelector2", selected = "GO_Biological_Process_2023")
        updateSliderInput(session, "pValueThresholdcor", value = 0.60)
        updateSliderInput(session, "pValueThreshold_imp_12_1", value = 0.05)
        updateSliderInput(session, "pValueThreshold_imp_12_2", value = 0.05)
        incProgress(100, detail = 'Complete!')
      })
    })

    #LUAD

    observeEvent(input$runDemoAll, {
      withProgress(message = 'Loading example data...', value = 0, {
        demo_data_pathAll <- paste0(here::here(),"/inst/Example_data/FloresData_K_TK")
        file_path_metab_expAll <- file.path(demo_data_pathAll, "Metab_exp.rds")
        file_path_metab_annotAll <- file.path(demo_data_pathAll, "Metab_annot.rds")
        file_path_RNA_expAll <- file.path(demo_data_pathAll, "Prot_exp.rds")
        file_path_RNA_annotAll <- file.path(demo_data_pathAll, "Prot_annot.rds")
        file_path_metadataAll <- file.path(demo_data_pathAll, "Metadata.rds")

        precargados_metab_expAll <- readRDS(file_path_metab_expAll)
        precargados_metab_annotAll <- readRDS(file_path_metab_annotAll)
        precargados_RNA_expAll <- readRDS(file_path_RNA_expAll)
        precargados_RNA_annotAll <- readRDS(file_path_RNA_annotAll)
        precargados_metadataAll <- readRDS(file_path_metadataAll)

        incProgress(0, detail = 'Loading Metab_exp.csv')
        Metab_exp(precargados_metab_expAll)
        Sys.sleep(5)

        incProgress(10, detail = 'Loading Metab_annot.csv')
        Metab_annot(precargados_metab_annotAll)
        Sys.sleep(3)

        incProgress(20, detail = 'Loading Prot_exp.csv')
        Prot_exp(precargados_RNA_expAll)
        Sys.sleep(6)

        incProgress(30, detail = 'Loading Prot_annot.csv')
        Prot_annot(precargados_RNA_annotAll)
        Sys.sleep(3)

        incProgress(50, detail = 'Loading Metadata.csv...')
        metadata(precargados_metadataAll)
        Sys.sleep(2)

        demo_par_cor_Metab(FALSE)
        demo_par_cor_Metab_All(TRUE)
        demo_par_cor_Prot(FALSE)
        demo_par_cor_Prot_All(TRUE)
        demo_enrich_Prot(FALSE)
        demo_enrich_Prot_All(TRUE)


        updateSelectInput(session, "Mapping1", choices = variables_mapping1(), selected = c("Metabolite", "KEGG"))
        updateSelectInput(session, "Screening1", choices = variables_Screening1(), selected = c("Metabolite", "KEGG"))
        updateSelectInput(session, "Mapping2", choices = variables_mapping2(), selected = "Symbol")
        updateCheckboxInput(session, "runEnrichment2", value = TRUE)
        updateSelectInput(session, "Screening2", choices = variables_Screening2(), selected = "Symbol")
        updateSelectInput(session, "Screening12_1", choices = variables_Screening_12_1(), selected = c("Metabolite", "KEGG"))
        updateSelectInput(session, "Screening12_2", choices = variables_Screening_12_2(), selected = "Symbol")

        updateSelectInput(session, "databaseSelector2", selected = "KEGG_2019_Mouse")
        updateSliderInput(session, "pValueThresholdcor", value = 0.90)
        updateSliderInput(session, "pValueThreshold_imp_12_1", value = 0.5)
        updateSliderInput(session, "pValueThreshold_imp_12_2", value = 0.5)
        incProgress(100, detail = 'Complete!')
      })
    })



  })
}


## To be copied in the UI
# mod_module1_ui("module1_1")

## To be copied in the server
# mod_module1_server("module1_1")
