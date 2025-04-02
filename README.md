# IModMix
iModMix is an R package designed for the integrated analysis of omics data.


## How install this app

### Only the first time the app is installed, enter the following command in the console
```
install.packages("remotes")
```
### The following console command is to install from github
```
remotes::install_github("biodatalab/iModmix")
```

## Example

This is a basic example which shows you how to analyse 2 datasets:

``` r
# Load the package
library(iModMix)

# Run the Shiny app
run_app()


# Data 1 modules 
parcorData1= partial_cors(Data1_abundance)
hcData1 = hierarchical_cluster(parcor_mat = parcorData1, tom = TRUE, min_module_size = 10)
cluster_assig_Data1 = cluster_assignments(as.data.frame(hcData1$cluster_assignments))
eigenData1 = Eigengenes(SteData1_exp_id, hcData1$cluster_assignments[,3])
eigengenes_Data1 = eigenData1$module_eigenData1_Me

# Data 2 modules 
parcorData2= partial_cors(Data2_abundance)
hcData2 = hierarchical_cluster(parcor_mat = parcorData2, tom = TRUE, min_module_size = 10)
cluster_assig_Data2 = cluster_assignments(as.data.frame(hcData2$cluster_assignments))
eigenData2 = Eigengenes(SteData2_exp_id, hcData2$cluster_assignments[,3])
eigengenes_Data2 = eigenData2$module_eigenData2_Me

## Integration Data 1 and Data 2
eigengenes_list <- list(eigengenes_Data1, Eeigengenes_Data2)
cluster_list <- list(hcData1()$hcCluster_assignments, hcData1()$hcCluster_assignments)
Integration_Data1_Data2 <- Modules_correlation(eigengenes_list, cluster_list, threshold = 0.5)

```
