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
mod_module1_ui <- function(id, input, output, session) {

  ns <- NS(id)

  tagList(

    sidebarPanel(
      width = 4,
      # selectInput(
      #   ns("speciesSelector"),
      #   label = "Select Species",
      #   choices = c("Human", "Mouse"),
      #   selected = "Human"
      # ),

      selectInput(
        ns("databaseSelector"),
        label = "Select Library",
        choices = c("Achilles_fitness_decrease",
                    "Achilles_fitness_increase",
                    "Aging_Perturbations_from_GEO_down",
                    "Aging_Perturbations_from_GEO_up",
                    "Allen_Brain_Atlas_10x_scRNA_2021",
                    "Allen_Brain_Atlas_down",
                    "Allen_Brain_Atlas_up",
                    "ARCHS4_Cell-lines",
                    "ARCHS4_IDG_Coexp",
                    "ARCHS4_Kinases_Coexp",
                    "ARCHS4_TFs_Coexp",
                    "ARCHS4_Tissues",
                    "Azimuth_2023",
                    "Azimuth_Cell_Types_2021",
                    "BioCarta_2013",
                    "BioCarta_2015",
                    "BioCarta_2016",
                    "BioPlanet_2019",
                    "BioPlex_2017",
                    "Cancer_Cell_Line_Encyclopedia",
                    "CCLE_Proteomics_2020",
                    "CellMarker_2024",
                    "CellMarker_Augmented_2021",
                    "ChEA_2013",
                    "ChEA_2015",
                    "ChEA_2016",
                    "ChEA_2022",
                    "Chromosome_Location",
                    "Chromosome_Location_hg19",
                    "ClinVar_2019",
                    "CORUM",
                    "COVID-19_Related_Gene_Sets",
                    "COVID-19_Related_Gene_Sets_2021",
                    "Data_Acquisition_Method_Most_Popular_Genes",
                    "dbGaP",
                    "DepMap_WG_CRISPR_Screens_Broad_CellLines_2019",
                    "DepMap_WG_CRISPR_Screens_Sanger_CellLines_2019",
                    "Descartes_Cell_Types_and_Tissue_2021",
                    "Diabetes_Perturbations_GEO_2022",
                    "Disease_Perturbations_from_GEO_down",
                    "Disease_Perturbations_from_GEO_up",
                    "Disease_Signatures_from_GEO_down_2014",
                    "Disease_Signatures_from_GEO_up_2014",
                    "DisGeNET",
                    "Drug_Perturbations_from_GEO_2014",
                    "Drug_Perturbations_from_GEO_down",
                    "Drug_Perturbations_from_GEO_up",
                    "DrugMatrix",
                    "DSigDB",
                    "Elsevier_Pathway_Collection",
                    "ENCODE_and_ChEA_Consensus_TFs_from_ChIP-X",
                    "ENCODE_Histone_Modifications_2013",
                    "ENCODE_Histone_Modifications_2015",
                    "ENCODE_TF_ChIP-seq_2014",
                    "ENCODE_TF_ChIP-seq_2015",
                    "Enrichr_Libraries_Most_Popular_Genes",
                    "Enrichr_Submissions_TF-Gene_Coocurrence",
                    "Enrichr_Users_Contributed_Lists_2020",
                    "Epigenomics_Roadmap_HM_ChIP-seq",
                    "ESCAPE",
                    "FANTOM6_lncRNA_KD_DEGs",
                    "GeDiPNet_2023",
                    "Gene_Perturbations_from_GEO_down",
                    "Gene_Perturbations_from_GEO_up",
                    "Genes_Associated_with_NIH_Grants",
                    "GeneSigDB",
                    "Genome_Browser_PWMs",
                    "GlyGen_Glycosylated_Proteins_2022",
                    "GO_Biological_Process_2013",
                    "GO_Biological_Process_2015",
                    "GO_Biological_Process_2017",
                    "GO_Biological_Process_2017b",
                    "GO_Biological_Process_2018",
                    "GO_Biological_Process_2021",
                    "GO_Biological_Process_2023",
                    "GO_Cellular_Component_2013",
                    "GO_Cellular_Component_2015",
                    "GO_Cellular_Component_2017",
                    "GO_Cellular_Component_2017b",
                    "GO_Cellular_Component_2018",
                    "GO_Cellular_Component_2021",
                    "GO_Cellular_Component_2023",
                    "GO_Molecular_Function_2013",
                    "GO_Molecular_Function_2015",
                    "GO_Molecular_Function_2017",
                    "GO_Molecular_Function_2017b",
                    "GO_Molecular_Function_2018",
                    "GO_Molecular_Function_2021",
                    "GO_Molecular_Function_2023",
                    "GTEx_Aging_Signatures_2021",
                    "GTEx_Tissue_Expression_Down",
                    "GTEx_Tissue_Expression_Up",
                    "GTEx_Tissues_V8_2023",
                    "GWAS_Catalog_2019",
                    "GWAS_Catalog_2023",
                    "HDSigDB_Human_2021",
                    "HDSigDB_Mouse_2021",
                    "HMDB_Metabolites",
                    "HMS_LINCS_KinomeScan",
                    "HomoloGene",
                    "HuBMAP_ASCT_plus_B_augmented_w_RNAseq_Coexpression",
                    "HuBMAP_ASCTplusB_augmented_2022",
                    "Human_Gene_Atlas",
                    "Human_Phenotype_Ontology",
                    "HumanCyc_2015",
                    "HumanCyc_2016",
                    "huMAP",
                    "IDG_Drug_Targets_2022",
                    "InterPro_Domains_2019",
                    "Jensen_COMPARTMENTS",
                    "Jensen_DISEASES",
                    "Jensen_TISSUES",
                    "KEA_2013",
                    "KEA_2015",
                    "KEGG_2013",
                    "KEGG_2015",
                    "KEGG_2016",
                    "KEGG_2019_Human",
                    "KEGG_2019_Mouse",
                    "KEGG_2021_Human",
                    "Kinase_Perturbations_from_GEO_down",
                    "Kinase_Perturbations_from_GEO_up",
                    "KOMP2_Mouse_Phenotypes_2022",
                    "L1000_Kinase_and_GPCR_Perturbations_down",
                    "L1000_Kinase_and_GPCR_Perturbations_up",
                    "Ligand_Perturbations_from_GEO_down",
                    "Ligand_Perturbations_from_GEO_up",
                    "LINCS_L1000_Chem_Pert_Consensus_Sigs",
                    "LINCS_L1000_Chem_Pert_down",
                    "LINCS_L1000_Chem_Pert_up",
                    "LINCS_L1000_CRISPR_KO_Consensus_Sigs",
                    "LINCS_L1000_Ligand_Perturbations_down",
                    "LINCS_L1000_Ligand_Perturbations_up",
                    "lncHUB_lncRNA_Co-Expression",
                    "MAGMA_Drugs_and_Diseases",
                    "MAGNET_2023",
                    "MCF7_Perturbations_from_GEO_down",
                    "MCF7_Perturbations_from_GEO_up",
                    "Metabolomics_Workbench_Metabolites_2022",
                    "MGI_Mammalian_Phenotype_2013",
                    "MGI_Mammalian_Phenotype_2017",
                    "MGI_Mammalian_Phenotype_Level_3",
                    "MGI_Mammalian_Phenotype_Level_4",
                    "MGI_Mammalian_Phenotype_Level_4_2019",
                    "MGI_Mammalian_Phenotype_Level_4_2021",
                    "Microbe_Perturbations_from_GEO_down",
                    "Microbe_Perturbations_from_GEO_up",
                    "miRTarBase_2017",
                    "MoTrPAC_2023",
                    "Mouse_Gene_Atlas",
                    "MSigDB_Computational",
                    "MSigDB_Hallmark_2020",
                    "MSigDB_Oncogenic_Signatures",
                    "NCI-60_Cancer_Cell_Lines",
                    "NCI-Nature_2015",
                    "NCI-Nature_2016",
                    "NIH_Funded_PIs_2017_AutoRIF_ARCHS4_Predictions",
                    "NIH_Funded_PIs_2017_GeneRIF_ARCHS4_Predictions",
                    "NIH_Funded_PIs_2017_Human_AutoRIF",
                    "NIH_Funded_PIs_2017_Human_GeneRIF",
                    "NURSA_Human_Endogenous_Complexome",
                    "Old_CMAP_down",
                    "Old_CMAP_up",
                    "OMIM_Disease",
                    "OMIM_Expanded",
                    "Orphanet_Augmented_2021",
                    "PanglaoDB_Augmented_2021",
                    "Panther_2015",
                    "Panther_2016",
                    "Pfam_Domains_2019",
                    "Pfam_InterPro_Domains",
                    "PFOCR_Pathways",
                    "PFOCR_Pathways_2023",
                    "PhenGenI_Association_2021",
                    "PheWeb_2019",
                    "Phosphatase_Substrates_from_DEPOD",
                    "PPI_Hub_Proteins",
                    "Proteomics_Drug_Atlas_2023",
                    "ProteomicsDB_2020",
                    "Rare_Diseases_AutoRIF_ARCHS4_Predictions",
                    "Rare_Diseases_AutoRIF_Gene_Lists",
                    "Rare_Diseases_GeneRIF_ARCHS4_Predictions",
                    "Rare_Diseases_GeneRIF_Gene_Lists",
                    "Reactome_2013",
                    "Reactome_2015",
                    "Reactome_2016",
                    "Reactome_2022",
                    "RNA-Seq_Disease_Gene_and_Drug_Signatures_from_GEO",
                    "RNAseq_Automatic_GEO_Signatures_Human_Down",
                    "RNAseq_Automatic_GEO_Signatures_Human_Up",
                    "RNAseq_Automatic_GEO_Signatures_Mouse_Down",
                    "RNAseq_Automatic_GEO_Signatures_Mouse_Up",
                    "Rummagene_kinases",
                    "Rummagene_signatures",
                    "Rummagene_transcription_factors",
                    "SILAC_Phosphoproteomics",
                    "SubCell_BarCode",
                    "SynGO_2022",
                    "SynGO_2024",
                    "SysMyo_Muscle_Gene_Sets",
                    "Table_Mining_of_CRISPR_Studies",
                    "Tabula_Muris",
                    "Tabula_Sapiens",
                    "TargetScan_microRNA",
                    "TargetScan_microRNA_2017",
                    "TF-LOF_Expression_from_GEO",
                    "TF_Perturbations_Followed_by_Expression",
                    "TG_GATES_2020",
                    "The_Kinase_Library_2023",
                    "Tissue_Protein_Expression_from_Human_Proteome_Map",
                    "Tissue_Protein_Expression_from_ProteomicsDB",
                    "Transcription_Factor_PPIs",
                    "TRANSFAC_and_JASPAR_PWMs",
                    "TRRUST_Transcription_Factors_2019",
                    "UK_Biobank_GWAS_v1",
                    "Virus-Host_PPI_P-HIPSTer_2020",
                    "Virus_Perturbations_from_GEO_down",
                    "Virus_Perturbations_from_GEO_up",
                    "VirusMINT",
                    "WikiPathway_2021_Human",
                    "WikiPathway_2023_Human",
                    "WikiPathways_2013",
                    "WikiPathways_2015",
                    "WikiPathways_2016",
                    "WikiPathways_2019_Human",
                    "WikiPathways_2019_Mouse"),
        selected = "GO_Biological_Process_2023"
      ),

      fileInput(
        ns("DataSet"),
        accept = c('text/csv',
                   'text/comma-separated-values',
                   'text/plain',
                   '.csv'),
        label = h5("Metabolomics data")
      ),

      fileInput(
        ns("DataSet3"),
        accept = c('text/csv',
                   'text/comma-separated-values',
                   'text/plain',
                   '.csv'),
        label = h5("Annotation Metabolomics data")
      ),

      div(style = "border-top: 1px solid #ccc; margin-top: 10px; margin-bottom: 10px;"),

      fileInput(
        ns("DataSet2"),
        accept = c('text/csv',
                   'text/comma-separated-values',
                   'text/plain',
                   '.csv'),
        label = h5("Proteomics/Genomics data")
      ),

      fileInput(
        ns("DataSet4"),
        accept = c('text/csv',
                   'text/comma-separated-values',
                   'text/plain',
                   '.csv'),
        label = h5("Annotation Proteomics data")
      ),

      div(style = "border-top: 1px solid #ccc; margin-top: 10px; margin-bottom: 10px;"),

      fileInput(
        ns("metadata"),
        accept = c('text/csv',
                   'text/comma-separated-values',
                   'text/plain',
                   '.csv'),
        label = h5("Metadata")
      )
    ),


    mainPanel(
      tabsetPanel(
        type = "tabs",
        tabPanel("Metabolomics",
                 tabsetPanel(
                   type = "tabs",
                   tabPanel("Data Input",
                            h4("Expression matrix: Metabolites"),
                            DT::DTOutput(ns("infotable")),
                            DT::DTOutput(ns("table")),
                            selectInput(ns("phenotypeSelectorPCA"),
                                        label = "Select the phenotype of interest",
                                        choices = NULL,
                                        selected = NULL),
                            plotOutput(ns("PCA1")),
                            h4("Annotation: Metabolites"),
                            DT::DTOutput(ns("table3"))
                   ),
                   tabPanel("Module Assignments",
                            h4("Sparse partial correlations: Metabolites"),
                            verbatimTextOutput(ns("matrizTable")),
                            h4("Herarchical clustering"),
                            plotOutput(ns("hc_plot")),
                            h4("Cluster Assignments"),
                            DT::DTOutput(ns("tableClusterAssig2")),
                            DT::DTOutput(ns("tableClusterAssig")),
                            h4("First principal component from each module"),
                            DT::DTOutput(ns("tableEigengene"))
                   ),
                   tabPanel("Phenotype",
                            h4("Phenotype data"),
                            DT::DTOutput(ns("table5")),
                            fluidRow(
                              column(6,
                            selectInput(ns("phenotypeSelector"),
                                        label = "Select the phenotype of interest",
                                        choices = NULL,
                                        selected = NULL)
                              ),
                            column(6,
                            sliderInput(ns("pValueThreshold"),
                              label = "Select p-value Threshold",
                              min = 0,
                              max = 1,
                              step = 0.0001,
                              value = 0.05)
                            )
                            ),
                            h4("Classification"),
                            plotOutput(ns("classification_plot_1_all")),
                            DT::DTOutput(ns("classification_results")),
                            h4("Module: Screening"),
                            selectInput(ns("moduleSelector"),
                                        label = "Select the module of interest",
                                        choices = NULL,
                                        selected = NULL),
                            fluidRow(
                              splitLayout(cellWidths = c("50%", "50%"),
                                          DT::DTOutput(ns("ModuleFeatures")),
                                          DT::DTOutput(ns("ModuleFeaturesAnnot")))
                            ),
                            plotOutput(ns("Loadings1")),
                            plotOutput(ns("heatmap1"))

                   )

                 )

        ),
        tabPanel("Proteomics/Genomics",
                 tabsetPanel(
                   type = "tabs",
                   tabPanel("Data Input",
                            h4("Expression matrix: Proteins/Genes"),
                            DT::DTOutput(ns("infotable2")),
                            DT::DTOutput(ns("table2")),
                            selectInput(ns("phenotypeSelectorPCA2"),
                                        label = "Select the phenotype of interest",
                                        choices = NULL,
                                        selected = NULL),
                            plotOutput(ns("PCA2")),
                            h4("Annotation: Proteins/Genes"),
                            DT::DTOutput(ns("table4"))
                   ),
                   tabPanel("Module Assignments",
                            h4("Sparse partial correlations: Proteins/Genes"),
                            verbatimTextOutput(ns("matrizTable2")),
                            h4("Herarchical clustering"),
                            plotOutput(ns("hc_plot2")),
                            #DT::DTOutput(ns("tableClusterAssig2")),
                            h4("Cluster Assignments"),
                            DT::DTOutput(ns("tableClusterAssig3")),
                            h4("First principal component from each module"),
                            DT::DTOutput(ns("tableEigengene2"))

                   ),
                   tabPanel("Phenotype",
                            h4("Phenotype data"),
                            DT::DTOutput(ns("table6")),
                            fluidRow(
                              column(6,
                                     selectInput(ns("phenotypeSelector2"),
                                                 label = "Select the phenotype of interest",
                                                 choices = NULL,
                                                 selected = NULL)
                              ),
                              column(6,
                                     sliderInput(ns("pValueThreshold2"),
                                                 label = "Select p-value Threshold",
                                                 min = 0,
                                                 max = 1,
                                                 step = 0.0001,
                                                 value = 0.05)
                              )
                            ),
                            h4("Classification"),
                            #plotOutput(ns("classification_plot_2_1")),
                            plotOutput(ns("classification_plot_2_all")),
                            DT::DTOutput(ns("classification_results2")),
                            h4("Module: Screening"),
                            selectInput(ns("moduleSelector2"),
                                        label = "Select the module of interest",
                                        choices = NULL,
                                        selected = NULL),
                            fluidRow(
                              splitLayout(cellWidths = c("50%", "50%"),
                                          DT::DTOutput(ns("ModuleFeatures2")),
                                          DT::DTOutput(ns("ModuleFeatures2Annot")))
                            ),
                            plotOutput(ns("Loadings2")),
                            plotOutput(ns("heatmap2"))

                   )
                 )

                 ),
        tabPanel("Metabolomics-Proteomics/Genomics",
                 tabsetPanel(
                   type = "tabs",
                   tabPanel("Modules correlation",
                            sliderInput(ns("pValueThreshold3"),
                                        label = "Select Correlation Threshold",
                                        min = 0,
                                        max = 1,
                                        step = 0.05,
                                        value = 0.5),
                            h4("Correlation: Metabolites and Proteins/Genes"),
                            DT::DTOutput(ns("tableCorrelation")),
                            plotOutput(ns("Correlation_plot")),
                            h4("Module Network of Metabolites and Proteins/Genes"),
                            plotOutput(ns("Network_plot"))
                   ),
                   tabPanel("Important features",
                            DT::DTOutput(ns("ImportantVariables")),
                            # DT::DTOutput(ns("matrizTableImp")),
                            selectInput(ns("visualization_list"),
                                        label = "Select Lists to Visualize",
                                        choices = c("Top_1" = 1,
                                                    "Top_2" = 2,
                                                    "Top_3" = 3,
                                                    "Top_4" = 4,
                                                    "Top_5" = 5)
                                        ),
                            h4("List of Metabolites"),
                            verbatimTextOutput(ns("Important_features_2")),

                            h4("List of Proteins/Genes"),
                            verbatimTextOutput(ns("Important_features_1")),

                            # h4("Modules correlation: Metabolites and Proteins/Genes"),
                            # plotOutput(ns("Correlation_plotImp")),

                            #h4("Corrplot: Metabolites and Proteins/Genes"),
                            #plotOutput(ns("CorplotImp")),

                            fluidRow(
                              splitLayout(cellWidths = c("50%", "50%"),
                                          plotOutput(ns("Correlation_plotImp")),
                                         plotOutput(ns("CorplotImp")))
                            ),

                            h4("Modules correlation: Metabolites and Proteins/Genes"),
                            DT::DTOutput(ns("Correlation_mod")),

                            h4("Metabolites"),
                            DT::DTOutput(ns("cluster_assignments_2")),

                            h4("Proteins/Genes"),
                            DT::DTOutput(ns("cluster_assignments_1"))



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
mod_module1_server <- function(id, input, output, session){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    filedata <- reactive({
      req(input$DataSet)
      fileInput <- load_file(input$DataSet$name, input$DataSet$datapath)
      fileInput <- as.data.frame(fileInput)
      return(list(fileInput = fileInput))
    })
    data_info <- reactive({
      req(filedata()$fileInput)
      Nobservations <- nrow(filedata()$fileInput)
      Ncells <- ncol(filedata()$fileInput)-1
      SummaryData <- as.data.frame(list(Number = c(Ncells, Nobservations)))
      rownames(SummaryData) <- c("Samples", "Features")
      list(SummaryData = SummaryData)
    })
    output$infotable <- DT::renderDataTable({
      df <- data_info()$SummaryData
      DT::datatable(df)
    })
    output$table <- DT::renderDataTable({
      df <- filedata()$fileInput
      DT::datatable(df)
    })

    pca1 <- reactive({
      req(filedata()$fileInput)
      data = filedata()$fileInput
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
      if(is.null(filedata5()$fileInput)){
      ggplot2::autoplot(pca1()$pca_res)
       } else {
        req(filedata5()$fileInput)
        ggplot2::autoplot(pca1()$pca_res, data = filedata5()$fileInput, colour = input$phenotypeSelectorPCA)
      }
    })

    filedata2 <- reactive({
      req(input$DataSet2)
      fileInput <- load_file(input$DataSet2$name, input$DataSet2$datapath)
      fileInput <- as.data.frame(fileInput)
      return(list(fileInput = fileInput))
    })
    data_info2 <- reactive({
      req(filedata2()$fileInput)
      Nobservations <- nrow(filedata2()$fileInput)
      Ncells <- ncol(filedata2()$fileInput)-1
      SummaryData <- as.data.frame(list(Number = c(Ncells, Nobservations)))
      rownames(SummaryData) <- c("Samples", "Features")
      list(SummaryData = SummaryData)
    })
    output$infotable2 <- DT::renderDataTable({
      df <- data_info2()$SummaryData
      DT::datatable(df)
    })
    output$table2 <- DT::renderDataTable({
      df <- filedata2()$fileInput
      DT::datatable(df)
    })

    pca2 <- reactive({
      req(filedata2()$fileInput)
      data = filedata2()$fileInput
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
      if(is.null(filedata5()$fileInput)){
        ggplot2::autoplot(pca2()$pca_res)
      } else {
        ggplot2::autoplot(pca2()$pca_res, data = filedata5()$fileInput, colour = input$phenotypeSelectorPCA2)
      }
    })


    filedata3 <- reactive({
      req(input$DataSet3)
      fileInput <- load_file(input$DataSet3$name, input$DataSet3$datapath)
      fileInput <- as.data.frame(fileInput)
      return(list(fileInput = fileInput))
    })
    output$table3 <- DT::renderDataTable({
      df <- filedata3()$fileInput
      DT::datatable(df)
    })

    filedata4 <- reactive({
      req(input$DataSet4)
      fileInput <- load_file(input$DataSet4$name, input$DataSet4$datapath)
      fileInput <- as.data.frame(fileInput)
      return(list(fileInput = fileInput))
    })
    output$table4 <- DT::renderDataTable({
      df <- filedata4()$fileInput
      DT::datatable(df)
    })

    filedata5 <- reactive({
      req(input$metadata)
      fileInput <- load_file(input$metadata$name, input$metadata$datapath)
      fileInput <- as.data.frame(fileInput)
      return(list(fileInput = fileInput))
    })

    output$table5 <- DT::renderDataTable({
      df <- filedata5()$fileInput
      DT::datatable(df)
    })

    pheno_variablesPCA <- reactive({
      names(filedata5()$fileInput)
    })

    # For metabolites global PCA
    observe({
      updateSelectInput(session, "phenotypeSelectorPCA", choices = pheno_variablesPCA())
    })

    pheno_variables <- reactive({
      names(filedata5()$fileInput)
    })

    # for modules loading plot
    observe({
      updateSelectInput(session, "phenotypeSelector", choices = pheno_variables())
    })

    pheno_variables2 <- reactive({
      names(filedata5()$fileInput)
    })

    # for modules loading plot
    observe({
      updateSelectInput(session, "phenotypeSelector2", choices = pheno_variables2())
    })


    partial_cors1 <- reactive({
      req(filedata()$fileInput)
      data = filedata()$fileInput
      data$missing_count = rowSums(is.na(data))
      feature_mat = subset(data, missing_count <= 0.1 * (ncol(data)-2))
      features <- feature_mat[,1]
      feature_mat_t <- as.matrix(scale(t(feature_mat[,-c(1,ncol(feature_mat))])))
      colnames(feature_mat_t) <- features
      par_cor1=partial_cors(feature_mat_t = feature_mat_t)$partial_cor_mat
      return(list(par_cor1 = par_cor1, feature_mat_t = feature_mat_t))
    })

    output$matrizTable <- renderPrint({
      partial_cors1()$par_cor1[1:5,1:5]
    })


    hierarchical_cluster1 <- reactive({
      par_cor2 = partial_cors1()$par_cor1
      hc2 = hierarchical_cluster(parcor_mat = par_cor2, tom = TRUE, min_module_size = 10)
      hclusterTree2 = hc2$hclustTree
      hcDynMods2 = hc2$dynamicMods_numeric
      hcCluster_assignments2 = hc2$cluster_assignments
      return(list(hclusterTree2 = hclusterTree2, hcDynMods2 = hcDynMods2, hcCluster_assignments2 = hcCluster_assignments2 ))
    })

    output$tableClusterAssig2 <- DT::renderDataTable({
      df1 = hierarchical_cluster1()$hcCluster_assignments2
      DT::datatable(df1)
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



    cluster_assignments_metabolites1 <- reactive({
      #metab_annotation_data = filedata3()$fileInput
      cluster_metabolites = as.data.frame(hierarchical_cluster1()$hcCluster_assignments2)
      if (is.null(filedata3()$fileInput)) {
        cluster_assignments_metab <- cluster_assignments_metabolites(cluster_metabolites = cluster_metabolites, metab_annotation = NULL)
      } else {
        cluster_assignments_metab <- cluster_assignments_metabolites(cluster_metabolites = cluster_metabolites, metab_annotation = filedata3()$fileInput)
      }
      #cluster_assignments_metab = cluster_assignments_metabolites(cluster_metabolites = cluster_metabolites, metab_annotation = metab_annotation)
      return(list(cluster_assignments_metab = cluster_assignments_metab))
    })

    output$tableClusterAssig <- DT::renderDataTable({
      df1 = cluster_assignments_metabolites1()$cluster_assignments_metab
      DT::datatable(df1)
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

    Eigengene1 <- reactive({
      req(filedata()$fileInput)
      data = filedata()$fileInput
      data$missing_count = rowSums(is.na(data))
      feature_mat = subset(data, missing_count <= 0.1 * (ncol(data)-2))
      features <- feature_mat[,1]
      feature_mat_t <- as.matrix(scale(t(feature_mat[,-c(1,ncol(feature_mat))])))
      colnames(feature_mat_t) <- features
      Cluster_assignments = hierarchical_cluster1()$hcCluster_assignments2[,3]
      Eigengenes = Eigengenes(feature_mat_t = feature_mat_t, cluster_assignments = Cluster_assignments)$module_eigenmetab_Me
      return(list(Eigengenes = Eigengenes))
    })

    output$tableEigengene <- DT::renderDataTable({
      df2 = as.data.frame(Eigengene1()$Eigengenes)
      DT::datatable(df2)
    })

    Classification_Metabolites <- reactive({
      eigengenes_metab = as.data.frame(Eigengene1()$Eigengenes)
      metadata <- as.data.frame(filedata5()$fileInput)
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

    output$classification_plot_1_all <- renderPlot({
      selected_variable <- input$phenotypeSelector
      levels_selected_variable <- unique(filedata5()$fileInput[[selected_variable]])
      if (length(levels_selected_variable) < 3) {
        class_names <- levels_selected_variable
        class_label <- paste(class_names, collapse = " vs ")
        plot <- Classification_Metabolites()$plots[[1]]
        plot <- plot +
          ggplot2::labs(title = class_label, fill = as.factor(levels_selected_variable),
               x = "Variables",
               y = "Class")
        return(plot)
      } else {
        # Print multiple boxplot charts, one for each level of the selected variable
        plots_list <- lapply(1:length(levels_selected_variable), function(i) {
          Classification_Metabolites()$plots[[i]]
        })
        cowplot::plot_grid(plotlist = plots_list)
      }
    })


    loadings_metab <- reactive({
      req(filedata()$fileInput)
      data = filedata()$fileInput

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
      combined_data <- merge(filedata5()$fileInput[,c("Sample", selected_variable)], cluster_expression_matrix_Metab, by.x = "Sample", by.y = "row.names", all.x = TRUE)
      heatmap_data_sub_order <- combined_data[order(combined_data[[selected_variable]]), ]
      data_heat= t(as.matrix(heatmap_data_sub_order[ , 3:ncol(heatmap_data_sub_order)]))

      pca_res <- prcomp(cluster_expression_matrix_Metab)
      return(list(pca_res = pca_res, data_heat= data_heat, heatmap_data_sub_order = heatmap_data_sub_order, cluster_variables_MetabKEGG = cluster_variables_MetabKEGG))
    })

    output$ModuleFeatures <- DT::renderDataTable({
      df2 = as.data.frame(loadings_metab()$cluster_variables_MetabKEGG)
      names(df2) = "Feature_ID"
      DT::datatable(df2)
    })

    output$ModuleFeaturesAnnot <- DT::renderDataTable({
        req(filedata3()$fileInput)
        AnnoMeta = as.data.frame(filedata3()$fileInput)
        cluster_variables_Metab = loadings_metab()$cluster_variables_MetabKEGG
        cluster_variables_MetabKEGG <- AnnoMeta[AnnoMeta$Feature_ID %in% cluster_variables_Metab, c("Feature_ID", "KEGG")]
      DT::datatable(cluster_variables_MetabKEGG, rownames = FALSE)
    })

    output$Loadings1 <- renderPlot({
        ggplot2::autoplot(loadings_metab()$pca_res, data = filedata5()$fileInput, colour = input$phenotypeSelector, loadings = TRUE)
    })

    output$heatmap1 <- renderPlot({
      selected_variable <- input$phenotypeSelector
      levels_selected_variable <- unique(filedata5()$fileInput[[selected_variable]])

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


    partial_cors2 <- reactive({
      req(filedata2()$fileInput)
      data = filedata2()$fileInput
      data$missing_count = rowSums(is.na(data))
      feature_mat = subset(data, missing_count <= 0.1 * (ncol(data)-2))
      features <- feature_mat[,1]
      feature_mat_t <- as.matrix(scale(t(feature_mat[,-c(1,ncol(feature_mat))])))
      colnames(feature_mat_t) <- features
      par_cor=partial_cors(feature_mat_t = feature_mat_t)$partial_cor_mat
      return(list(par_cor = par_cor, feature_mat_t = feature_mat_t))
    })

    output$matrizTable2 <- renderPrint({
      partial_cors2()$par_cor[1:5,1:5]
    })

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

    cluster_assignments_genes1 <- reactive({
      req(filedata4()$fileInput)
      Prot_annotation = filedata4()$fileInput
      cluster_genes = hierarchical_cluster2()$hcCluster_assignments
      cluster_assignments_Prot = cluster_assignments_genes(cluster_genes = cluster_genes,
                                                           Prot_annotation = Prot_annotation)
      return(list(cluster_assignments_Prot = cluster_assignments_Prot))
    })

    # output$tableClusterAssig2 <- DT::renderDataTable({
    #   df2 = cluster_assignments_genes1()$cluster_assignments_Prot
    #   DT::datatable(df2)
    # })

    Genes_Prot_enrich <- reactive({
      #req(input$speciesSelector)
      #selected_species <- input$speciesSelector

      req(input$databaseSelector)
      selected_database <- input$databaseSelector

      cluster_assignments_ProtGenes = cluster_assignments_genes1()$cluster_assignments_Prot
      cluster_assignments_Prot_enrich <- Assigment_genes_enrichr(cluster_assignments_ProtGenes = cluster_assignments_ProtGenes,
                                                                 #species = selected_species,
                                                                 database = selected_database)
      return(list(cluster_assignments_Prot_enrich = cluster_assignments_Prot_enrich))
    })

    #curl::has_internet()
    #assign("has_internet_via_proxy", TRUE, environment(curl::has_internet))
    #library(enrichR)
    #enrichR::listEnrichrSites()
    output$tableClusterAssig3 <- DT::renderDataTable({
      df3 = Genes_Prot_enrich()$cluster_assignments_Prot_enrich
      DT::datatable(df3)
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

    Eigengene2 <- reactive({
      req(filedata2()$fileInput)
      data = filedata2()$fileInput
      data$missing_count = rowSums(is.na(data))
      feature_mat = subset(data, missing_count <= 0.1 * (ncol(data)-2))
      features <- feature_mat[,1]
      feature_mat_t <- as.matrix(scale(t(feature_mat[,-c(1,ncol(feature_mat))])))
      colnames(feature_mat_t) <- features
      Cluster_assignments = hierarchical_cluster2()$hcCluster_assignments[,3]
      Eigengenes = Eigengenes(feature_mat_t = feature_mat_t,
                              cluster_assignments = Cluster_assignments)$module_eigenmetab_Me
      return(list(Eigengenes = Eigengenes))
    })

    output$tableEigengene2 <- DT::renderDataTable({
      df3 = as.data.frame(Eigengene2()$Eigengenes)
      DT::datatable(df3)
    })

    filedata6 <- reactive({
      req(input$metadata)
      fileInput <- load_file(input$metadata$name, input$metadata$datapath)
      fileInput <- as.data.frame(fileInput)
      return(list(fileInput = fileInput))
    })
    output$table6 <- DT::renderDataTable({
      df <- filedata6()$fileInput
      DT::datatable(df)
    })

    pheno_variablesPCA2 <- reactive({
      names(filedata6()$fileInput)
    })

    # For metabolites global PCA
    observe({
      updateSelectInput(session, "phenotypeSelectorPCA2", choices = pheno_variablesPCA2())
    })

    Classification_Proteins <- reactive({
      eigengenes_prot = as.data.frame(Eigengene2()$Eigengenes)
      metadata <- as.data.frame(filedata6()$fileInput)
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

    output$classification_plot_2_all <- renderPlot({
      selected_variable <- input$phenotypeSelector2
      levels_selected_variable <- unique(filedata6()$fileInput[[selected_variable]])
      if (length(levels_selected_variable) < 3) {
        class_names <- levels_selected_variable
        class_label <- paste(class_names, collapse = " vs ")
        plot <- Classification_Proteins()$plots[[1]]
        plot <- plot +
          ggplot2::labs(title = class_label, fill = as.factor(levels_selected_variable),
                        x = "Variables",
                        y = "Class")
        return(plot)
      } else {
        # Print multiple boxplot charts, one for each level of the selected variable
        plots_list <- lapply(1:length(levels_selected_variable), function(i) {
          Classification_Proteins()$plots[[i]]
        })
        cowplot::plot_grid(plotlist = plots_list)
      }
    })


    loadings_Prot <- reactive({
      req(filedata2()$fileInput)
      data = filedata2()$fileInput

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
      combined_data <- merge(filedata6()$fileInput[,c("Sample", selected_variable)], cluster_expression_matrix_Prot, by.x = "Sample", by.y = "row.names", all.x = TRUE)
      heatmap_data_sub_order <- combined_data[order(combined_data[[selected_variable]]), ]
      data_heat= t(as.matrix(heatmap_data_sub_order[ , 3:ncol(heatmap_data_sub_order)]))

      pca_res <- prcomp(cluster_expression_matrix_Prot)
      return(list(pca_res = pca_res, data_heat= data_heat, heatmap_data_sub_order = heatmap_data_sub_order, cluster_variables_ProtSymbol = cluster_variables_ProtSymbol))
    })


    output$ModuleFeatures2 <- DT::renderDataTable({
      df2 = as.data.frame(loadings_Prot()$cluster_variables_ProtSymbol)
      names(df2) = "Feature_ID"
      DT::datatable(df2)
    })

    output$ModuleFeatures2Annot <- DT::renderDataTable({
      req(filedata4()$fileInput)
      AnnoProt = as.data.frame(filedata4()$fileInput)
      cluster_variables_Prot = loadings_Prot()$cluster_variables_ProtSymbol
      cluster_variables_ProtSymbol <- AnnoProt[AnnoProt$Feature_ID %in% cluster_variables_Prot, c("Feature_ID", "Symbol")]
      DT::datatable(cluster_variables_ProtSymbol, rownames = FALSE)
    })

    output$Loadings2 <- renderPlot({
      ggplot2::autoplot(loadings_Prot()$pca_res, data = filedata6()$fileInput, colour = input$phenotypeSelector2, loadings = TRUE)
    })

    output$heatmap2 <- renderPlot({
      selected_variable <- input$phenotypeSelector2
      levels_selected_variable <- unique(filedata6()$fileInput[[selected_variable]])

      if (length(levels_selected_variable) == 2) {
        # Usar una paleta diferente para dos niveles
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
        #show_row_names = FALSE,
        show_column_names = FALSE,  top_annotation = column_anno
      )

      ComplexHeatmap::draw(Prot_heatmap_plot, heatmap_legend_side = "right",
                           annotation_legend_side = "left", padding = ggplot2::unit(c(2, 3, 2, 40), "mm"))
    })


    Cor_Prot_Metab1 <- reactive({
      eigengenes_Prot = Eigengene2()$Eigengenes
      eigengenes_metab = Eigengene1()$Eigengenes
      cluster_assignments_metab = cluster_assignments_metabolites1()$cluster_assignments_metab
      #cluster_assignments_Prot_enrich = cluster_assignments_genes1()$cluster_assignments_Prot
      cluster_assignments_Prot_enrich = Genes_Prot_enrich()$cluster_assignments_Prot_enrich
      threshold = input$pValueThreshold3
      Cor_Prot_Metab = Modules_correlation(eigengenes_Prot, eigengenes_metab,
                                           cluster_assignments_Prot_enrich,
                                           cluster_assignments_metab, threshold = threshold)
      return(list(Top_cor_Prot_metab = Cor_Prot_Metab$Top_cor_Prot_metab,
                  filtered_cor_Prot_metab_list = Cor_Prot_Metab$filtered_cor_Prot_metab_list,
                  cor_Prot_metab_WGCNA = Cor_Prot_Metab$cor_Prot_metab_WGCNA))
    })

    output$tableCorrelation <- DT::renderDataTable({
      df4 = as.data.frame(Cor_Prot_Metab1()$Top_cor_Prot_metab)
      DT::datatable(df4)
    })

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
      condicion_tipo <- ifelse(grepl("Gene", igraph::V(network)$name), "lightgreen", "#E69F00")
      color_text <- ifelse(grepl("Gene", igraph::V(network)$name), "darkgreen", "orange")

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

     })


    ImpVar_Prot_Metab1 <- reactive({
      Cor_Prot_Metab = as.data.frame(Cor_Prot_Metab1()$Top_cor_Prot_metab)
      #cluster_assignments_Prot_enrich = cluster_assignments_genes1()$cluster_assignments_Prot
      cluster_assignments_Prot_enrich = Genes_Prot_enrich()$cluster_assignments_Prot_enrich
      cluster_assignments_metab = cluster_assignments_metabolites1()$cluster_assignments_metab
      Prot_annotation = filedata4()$fileInput
      metab_annotation = filedata3()$fileInput
      Prot_t = partial_cors2()$feature_mat_t
      metab_t = partial_cors1()$feature_mat_t
      ImpVar_Prot_Metab <- FeaturesAnnot_correlation(Cor_Prot_Metab,
                                                     cluster_assignments_Prot_enrich,
                                                     cluster_assignments_metab,
                                                     Prot_annotation,
                                                     metab_annotation,
                                                     Prot_t,
                                                     metab_t,
                                                     top_n = 5) #$correlation_matrices_list
      return(list(
        Top_correlations = ImpVar_Prot_Metab$Top_correlations,
        cluster_assignments = ImpVar_Prot_Metab$cluster_assignments,
        annotation_matrices = ImpVar_Prot_Metab$annotation_matrices,
        expression_matrices = ImpVar_Prot_Metab$expression_matrices,
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
        df2_1 = as.data.frame(ImpVar_Prot_Metab1()$annotation_matrices[[1]])
        df2_2 = as.data.frame(ImpVar_Prot_Metab1()$annotation_matrices[[2]])
        df3_1 = ImpVar_Prot_Metab1()$expression_matrices[[1]]
        df3_2 = ImpVar_Prot_Metab1()$expression_matrices[[2]]
        df4   = ImpVar_Prot_Metab1()$correlation_List[[1]]
        df4_1 = hist(ImpVar_Prot_Metab1()$correlation_matrices[[1]], main = "Top 1 Modules Correlation")
        df4_2 = corrplot::corrplot(ImpVar_Prot_Metab1()$correlation_matrices[[1]], type = "upper",  tl.col = "black", col = custom_palette)
        df5_1 = ImpVar_Prot_Metab1()$Important_features[[1]]
        df5_2 = ImpVar_Prot_Metab1()$Important_features[[2]]
      }

      if(input$visualization_list == 2){
        df1_1 = as.data.frame(ImpVar_Prot_Metab1()$cluster_assignments[[3]])
        df1_2 = as.data.frame(ImpVar_Prot_Metab1()$cluster_assignments[[4]])
        df2_1 = as.data.frame(ImpVar_Prot_Metab1()$annotation_matrices[[3]])
        df2_2 = as.data.frame(ImpVar_Prot_Metab1()$annotation_matrices[[4]])
        df3_1 = ImpVar_Prot_Metab1()$expression_matrices[[3]]
        df3_2 = ImpVar_Prot_Metab1()$expression_matrices[[4]]
        df4   = ImpVar_Prot_Metab1()$correlation_List[[2]]
        df4_1 = hist(ImpVar_Prot_Metab1()$correlation_matrices[[2]], main = "Top 2 Modules Correlation")
        df4_2 = corrplot::corrplot(ImpVar_Prot_Metab1()$correlation_matrices[[2]], type = "upper",  tl.col = "black", col = custom_palette)
        df5_1 = ImpVar_Prot_Metab1()$Important_features[[3]]
        df5_2 = ImpVar_Prot_Metab1()$Important_features[[4]]
      }

      if(input$visualization_list == 3){
        df1_1 = as.data.frame(ImpVar_Prot_Metab1()$cluster_assignments[[5]])
        df1_2 = as.data.frame(ImpVar_Prot_Metab1()$cluster_assignments[[6]])
        df2_1 = as.data.frame(ImpVar_Prot_Metab1()$annotation_matrices[[5]])
        df2_2 = as.data.frame(ImpVar_Prot_Metab1()$annotation_matrices[[6]])
        df3_1 = ImpVar_Prot_Metab1()$expression_matrices[[5]]
        df3_2 = ImpVar_Prot_Metab1()$expression_matrices[[6]]
        df4   = ImpVar_Prot_Metab1()$correlation_List[[3]]
        df4_1 = hist(ImpVar_Prot_Metab1()$correlation_matrices[[3]], main = "Top 3 Modules Correlation")
        df4_2 = corrplot::corrplot(ImpVar_Prot_Metab1()$correlation_matrices[[3]], type = "upper",  tl.col = "black", col = custom_palette)
        df5_1 = ImpVar_Prot_Metab1()$Important_features[[5]]
        df5_2 = ImpVar_Prot_Metab1()$Important_features[[6]]
      }

      if(input$visualization_list == 4){
        df1_1 = as.data.frame(ImpVar_Prot_Metab1()$cluster_assignments[[7]])
        df1_2 = as.data.frame(ImpVar_Prot_Metab1()$cluster_assignments[[8]])
        df2_1 = as.data.frame(ImpVar_Prot_Metab1()$annotation_matrices[[7]])
        df2_2 = as.data.frame(ImpVar_Prot_Metab1()$annotation_matrices[[8]])
        df3_1 = ImpVar_Prot_Metab1()$expression_matrices[[7]]
        df3_2 = ImpVar_Prot_Metab1()$expression_matrices[[8]]
        df4   = ImpVar_Prot_Metab1()$correlation_List[[4]]
        df4_1 = hist(ImpVar_Prot_Metab1()$correlation_matrices[[4]], main = "Top 2 Modules Correlation")
        df4_2 = corrplot::corrplot(ImpVar_Prot_Metab1()$correlation_matrices[[4]], type = "upper",  tl.col = "black", col = custom_palette)
        df5_1 = ImpVar_Prot_Metab1()$Important_features[[7]]
        df5_2 = ImpVar_Prot_Metab1()$Important_features[[8]]
      }

      if(input$visualization_list == 5){
        df1_1 = as.data.frame(ImpVar_Prot_Metab1()$cluster_assignments[[9]])
        df1_2 = as.data.frame(ImpVar_Prot_Metab1()$cluster_assignments[[10]])
        df2_1 = as.data.frame(ImpVar_Prot_Metab1()$annotation_matrices[[9]])
        df2_2 = as.data.frame(ImpVar_Prot_Metab1()$annotation_matrices[[10]])
        df3_1 = ImpVar_Prot_Metab1()$expression_matrices[[9]]
        df3_2 = ImpVar_Prot_Metab1()$expression_matrices[[10]]
        df4   = ImpVar_Prot_Metab1()$correlation_List[[5]]
        df4_1 = hist(ImpVar_Prot_Metab1()$correlation_matrices[[5]], main = "Top 3 Modules Correlation")
        df4_2 = corrplot::corrplot(ImpVar_Prot_Metab1()$correlation_matrices[[5]], type = "upper",  tl.col = "black", col = custom_palette)
        df5_1 = ImpVar_Prot_Metab1()$Important_features[[9]]
        df5_2 = ImpVar_Prot_Metab1()$Important_features[[10]]
      }

      return(list(df1_1 = df1_1, df1_2=df1_2, df5_1 = df5_1, df5_2=df5_2, df4=df4, df4_1=df4_1, df4_2=df4_2))

      #return(list(df1=df1, df2_1=df2_1, df2_2=df2_2, df3_1=df3_1, df3_2=df3_2, df4=df4, df5_1=df5_1, df5_2=df5_2 ))
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


    # output$Correlation_plotImp <- renderPlot({
    #   hist = Important_Features()$df4_1
    #   cor = Important_Features()$df4_2
    #   gridExtra::grid.arrange(hist, cor, ncol = 2)
    # })

    output$cluster_assignments_1 <- DT::renderDataTable({
      Important_Features()$df1_1
    })

    output$cluster_assignments_2 <- DT::renderDataTable({
      Important_Features()$df1_2
    })

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
