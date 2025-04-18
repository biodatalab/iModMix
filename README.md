# IModMix
`iModMix` is a novel network-based approach that integrates multi-omics data, including metabolomics, proteomics, and transcriptomics data, into a unified network to unveil associations across various layers of omics data and reveal significant associations with phenotypes of interest.

## How to Install This App

### Only the first time the app is installed, enter the following command in the console:
```r
install.packages("remotes")
```

### The following console command is to install from GitHub:
```r
remotes::install_github("biodatalab/iModmix")
```

## Main Functions

```r
partial_cors()
Calculates partial correlations between the input variables.

hierarchical_cluster()
Performs hierarchical clustering based on partial correlations and assigns modules.

cluster_assignments()
Assigns modules to data.

Eigengenes()
Calculates eigengenes for the assigned modules.

Modules_correlation()
Integrates eigenfuatures through samples.  
```

## Example

This is a basic example which shows you how to analyze two datasets:

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

