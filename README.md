# IModMix
Description...


## How install this app

### Only the first time the app is installed, enter the following command in the console
```
install.packages("remotes")
```
### The following console command is to install from github
```
remotes::install_github("biodatalab/iModmix", auth_token = "your GitHub token")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
### Run app
library(iModMix)
run_app()

### Run R package
library(iModMix)

# Metabolites modules 
parcorMetab= partial_cors(Metabolomics_abundance)
hcMetab = hierarchical_cluster(parcor_mat = parcorMetab, tom = TRUE, min_module_size = 10)
cluster_assig_metab = cluster_assignments_metabolites(as.data.frame(hcMetab$cluster_assignments))
eigenmetab = Eigengenes(Stemetab_exp_id, hcMetab$cluster_assignments[,3])
eigengenes_metab = eigenmetab$module_eigenmetab_Me

## Proteins/Genes modules
parcorProt= partial_cors(Protein/gene_exp)
hcProt = hierarchical_cluster(parcor_mat = parcorProt, tom = TRUE, min_module_size = 10)
cluster_assig_prot = cluster_assignments_genes(as.data.frame(hcProt$cluster_assignments))
eigenprot= Eigengenes(SteProt_exp, hcProt$cluster_assignments[,3])

## Integration Metabolites and proteins
cor_Prot_metab <- cor(eigenprot$module_eigenmetab_Me, eigenmetab$module_eigenmetab_Me, method = 'spearman', use = "pairwise.complete.obs")

```
