# IModMix Tutorial
`iModMix` is a novel network-based approach that integrates multi-omics data, including metabolomics, proteomics, and transcriptomics data, into a unified network to unveil associations across various layers of omics data and reveal significant associations with phenotypes of interest.

# Introduction
iModMix is an R package and Shiny App that can be viewed as a web interface through https://imodmix.moffitt.org/ or accessed locally by downloading and installing an app.R or docker container, or by installing an R-package. iMOdMIx is a novel approach that uses a graphical lasso to construct network modules for integration and analysis of multi-omics da 

iModMix can incorporate both identified and unidentified metabolites, addressing a key limitation of existing methodologies.

iModMix is available as a user-friendly R Shiny application that requires no programming experience (https://imodmix.moffitt.org), and it includes example data from several publicly available multi-omic studies for exploration. An R package is available for advanced users (https://github.com/biodatalab/iModMix). 

![Overview iModMix](https://github.com/user-attachments/assets/c341ef0f-4e02-4e28-b844-a0356c30d3f4)

## How to Install This App
The iModMix suite can be downloaded (cloned) and installed through the GitHub repository. The downloaded file can be unzipped to a destination folder, which should be set as the working directory or file path. Of note, some of the example files (e.g., gene set files) use relative paths, so the program may fail to identify the file if a working directory is not properly set. The suite was developed in R version 4.2. 

### Only the first time the app is installed, enter the following command in the console:
```r
install.packages("remotes")
```

### The following console command is to install from GitHub:
```r
remotes::install_github("biodatalab/iModmix")

```
### Install iModMix suite GitHub repository
```r
    * **Download and unzip repository https://github.com/biodatalab/iModMix**
    * Set working directory to iModMix folder
    * Install required R packages
    * Suite of tools was built on R version 4.2
    * R script for package installation is provided in the “1-Getting_Started” folder

```

# Load Data

## Load Example Data
iModMix includes two example datasets within the Shiny app. The first case study uses public data to validate our results. We used 24 normal and 52 tumor clear cell renal cell carcinoma (ccRCC) samples (Golkaram et al. 2022; Tang et al. 2023; Benedetti et al. 2023). The second case study includes untargeted metabolomics, highlighting iModMix’s ability to analyze and provide insight into unidentified metabolites. This study matched proteomics and metabolomics dataset was generated using two mouse models for lung adenocarcinoma (LUAD) (10 wild type, 10 knockout). 

## Input User Data
iModMix provides an intuitive method for users to input and preview a matrix expression, annotation and meta file.  

![](https://github.com/user-attachments/assets/c21c8c3e-46d9-4226-8024-92fd015bb653)


### Key Input Files
1. Required Files
    * Expression file where the feature ID are in the first column and the sample names are in the first-row header.
    * Meta information file that consists of a matrix with the first column the sample name. The remaining columns should contain any variables of interest.
    	
2. Optional Files
    * Annotation file is an optional file where the feature ID are in the first column and the remaining columns should contain mapping information (i.e, Metabolite name, KEGG, Symbol). 
    * Enrichment Analysis can be run in genemoics or proteomics data. To run the enrichment analysis, the Symbol information should be avaylable.

Expression, annotation matrix and meta files should be formatted similarly to the example datasets shown below. 

The matrix file should contain features in the first column and sample IDs in the first row.

![](https://github.com/user-attachments/assets/8cdfd5af-ee00-40d7-b72e-10f357b66275)


The meta file should contain sample IDs in the first column and any accompanying meta information in subsequent columns. The sample IDs should match between the matrix and meta files. The meta file should include at least one column of variable of of interes. iModMix defaults to selecting the second column of the meta file as the variable of interest. This can easily be changed by the user by simply selecting a different column using the dropdown menus. 

![](https://github.com/user-attachments/assets/44fbdb7a-5298-4040-9490-5ef1137f3d5a)


The annotation file should contain features ID in the first column.

![Image](https://github.com/user-attachments/assets/48937dde-b0dd-4652-8a48-43b13959221a)
![Image](https://github.com/user-attachments/assets/9aa463a2-6a39-4c78-94be-003ec2561f28)
![Image](https://github.com/user-attachments/assets/6ca48ef2-a0da-490a-9685-c190c7d3a7cd)
![Image](https://github.com/user-attachments/assets/b341bd04-f22f-4644-88cd-02eac9f0fab6)
![Image](https://github.com/user-attachments/assets/9b28d4c6-bb4b-40a5-9df7-4c1bd5cbfd9d)
![Image](https://github.com/user-attachments/assets/fb1117a6-300b-4256-b908-9331b0abfcf0)
![Image](https://github.com/user-attachments/assets/9230dfa7-39ff-416a-a5c3-fe623ce2163c)
![Image](https://github.com/user-attachments/assets/4aa94f8a-2cc6-4efe-b5cb-9fcaa7c87080)
![Image](https://github.com/user-attachments/assets/1dbf9848-74fc-4605-b6b7-c520e8e71e40)
![Image](https://github.com/user-attachments/assets/a19f842d-f9a2-418f-908f-b63efca1c45e)
![Image](https://github.com/user-attachments/assets/cc80d58f-2c48-4611-83b5-634deec518d6)
![Image](https://github.com/user-attachments/assets/eba0c466-f03f-4bde-bbcf-63956e04cd15)
![Image](https://github.com/user-attachments/assets/92fe0169-138b-4410-bdca-2d48f5719fa5)
![Image](https://github.com/user-attachments/assets/9cad9a8f-cec8-4723-b871-fe99344e3ee0)


## iModMix package: Example to use

This is a basic example which shows you how to analyze two datasets through iModMix package:

```r
# Load the package
library(iModMix)

# Run the Shiny app
run_app()

# Run through functions
# Data 1 modules 
load_data1 <- load_data(Data_exp1)
parcorData1 <- partial_cors(load_data1)
hcData1 <- hierarchical_cluster(parcor_mat = parcorData1, tom = TRUE, min_module_size = 10)
cluster_assig_Data1 <- cluster_assignments(as.data.frame(hcData1$cluster_assignments))
eigenData1 <- Eigengenes(load_data1, hcData1$cluster_assignments[,3])
eigengenes_Data1 <- eigenData1$module_eigenmetab_Me

# Data 2 modules 
load_data2 <- load_data(Data_exp2)
parcorData2 <- partial_cors(load_data2)
hcData2 <- hierarchical_cluster(parcor_mat = parcorData2, tom = TRUE, min_module_size = 10)
cluster_assig_Data2 <- cluster_assignments(as.data.frame(hcData2$cluster_assignments))
eigenData2 <- Eigengenes(load_data2, hcData2$cluster_assignments[,3])
eigengenes_Data2 <- eigenData2$module_eigenmetab_Me

# Integration of Data 1 and Data 2
eigengenes_list <- list(eigengenes_Data1, eigengenes_Data2)
cluster_list <- list(hcData1$hcCluster_assignments, hcData2$hcCluster_assignments)
Integration_Data1_Data2 <- Modules_correlation(eigengenes_list, cluster_list, threshold = 0.5)
Integration_Data1_Data2[["Top_cor_Prot_metab"]]
```

---

