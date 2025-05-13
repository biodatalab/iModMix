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
library(remotes)
```

### The following console command is to install from GitHub:
```r
remotes::install_github("biodatalab/iModmix")
library(iModmix)

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

The matrix file should contain features in the first column and sample IDs in the first row. Table reflecting the uploaded file expression Data. Check if the number of samples and the number of features are correct. The arrows to the right of each column title can be used for sorting data from increasing or decreasing values. The search bar can also be used to confirm the details of a feature of interest,
                                      

![](https://github.com/user-attachments/assets/8cdfd5af-ee00-40d7-b72e-10f357b66275)


The meta file should contain sample IDs in the first column and any accompanying meta information in subsequent columns. The sample IDs should match between the matrix and meta files. The meta file should include at least one column of variable of of interes. iModMix defaults to selecting the second column of the meta file as the variable of interest. This can easily be changed by the user by simply selecting a different column using the dropdown menus. 
Table reflecting the uploaded file Metadata. Check if the number of samples and the number of entries listed at the bottom of the table are the same. The arrows to the right of each column title can be used for sorting data from increasing or decreasing values. The search bar can also be used to confirm the details of a sample.
                                     

![](https://github.com/user-attachments/assets/44fbdb7a-5298-4040-9490-5ef1137f3d5a)


The annotation file should contain features ID in the first column. Table reflecting the uploaded file Annotation Data. Check if the total number of entries at bottom of table matches the total number of features in the Abundance Data. The arrows to the right of each column title can be used for sorting data from increasing or decreasing values. The search bar can also be used to confirm the details of a feature of interest.
                                      

![](https://github.com/user-attachments/assets/48937dde-b0dd-4652-8a48-43b13959221a)

## iModmix Outputs

### Individual Analysis

Preview of: a) The partial correlations matrix resulted based on graphical lasso. b) The Hierarchical clustering with a dendrogram of genes and modules.  

Partial correlation is a method of analyzing the relationship between two variables when other variables are present. Graphical Lasso (Glasso) is used to estimate the partial correlation and captures only direct associations. Below is a preview of the sparse partial correlation of the first five features. The full .csv file for the sparse partial correlation calculations for all the features can be downloaded at the bottom of the table.

Hierarchical clustering is used to identify common neighbors between the features. Calculations are determined using the topographical overlap matrix (TOM) and based on the sparse partial correlations. Hierarchical clustering is visualized as a dendrogram. 

Axes: The vertical axis (y-axis) represents the dissimilarity between features, while the horizontal axis (x-axis) shows the modules. 
Branches: Each line in the dendrogram represents a feature. Features that are closer in the hierarchy (i.e., joined at a lower height in the dendrogram) have more similar expression profiles.

![](https://github.com/user-attachments/assets/9aa463a2-6a39-4c78-94be-003ec2561f28)


Let's see the cluster assignments. Hierarchical clustering generates multiple modules (clusters) to which each feature is assigned. The table below details the following columns:

* **Feature**: Metabolite ID.
* **Module_id**: The module where the metabolite is assigned and the color used on the hierarchical clustering dendrogram.
* **Annotation Data**: If available, it can also show the KEGG ID, the metabolite name, and the information selected by the user.

The arrows to the right of each column title can be used for sorting data from increasing or decreasing values. The search bar can also be used to find the details of a metabolomic feature of interest. The full .csv file of Cluster Assignments for all the metabolomic features can be downloaded at the bottom of the table.

![](https://github.com/user-attachments/assets/6ca48ef2-a0da-490a-9685-c190c7d3a7cd)


The first principal component (PC1) is calculated for each module, referred to as an eigenfeature. Eigenfeatures are useful for

* Relating the modules to the phenotypes. 
* Obtaining the correlation between omics datasets (integration). 
    
The full .csv file of calculations for PC1 for metabolomics modules can be downloaded at the bottom of the table. 
The heatmap below shows eigenfeatures across samples. The vertical axis (y-axis) represents the eigenfeatures, and the horizontal axis (x-axis) displays the sample conditions.

![](https://github.com/user-attachments/assets/b341bd04-f22f-4644-88cd-02eac9f0fab6)


Statistical analysis by Students t-test compares phenotypes chosen from a drop-down menu. The eigenfeatures of each module, determined previously, are used as predictors. The user can also specify a significance threshold for the p-value, with the default set to 0.05. It returns a data frame with the following columns 

* **Variable**: Represents the ID of the module.
* **Class**: Lists the two levels of the phenotypes being compared. If there are more than two levels, it compares one level against the others.
* **Result_t**: The t-statistic value.
* **Result_pValue**: The p-value for the test.
* **Adjusted_pValue**: p-value correction using the Benjamin-Hochberg method to control the False Discovery Rate (FDR).

Boxplots are automatically generated at the bottom for significant eigenfeatures, with dots marking outliers and a legend describing the compared phenotype.

![](https://github.com/user-attachments/assets/9b28d4c6-bb4b-40a5-9df7-4c1bd5cbfd9d)


The drop-down menu displays all modules generated by iModMix. Users can view the features within a selected module. If annotation data is available, it also can shows the KEGG ID and metabolite name.

![](https://github.com/user-attachments/assets/fb1117a6-300b-4256-b908-9331b0abfcf0)


PCA loading and heatmap plots are generated to visualize the behavior of each specific module across the phenotype.

![](https://github.com/user-attachments/assets/9230dfa7-39ff-416a-a5c3-fe623ce2163c)


Drop-down menu displays available libraries for pathway analysis. Choose a library to automatically amend the dataset cluster descriptions on table below. Under column enriched_Term the most highly correlated pathway is displayed and in the following columns, along with enriched_Genes, and p-values as determined by Enrichr. The search bar can also be used to find the details of a protein/gene or module of interest. The full .csv file of Cluster Enrichments for dataset can be downloaded at the bottom of the table.

![](https://github.com/user-attachments/assets/4aa94f8a-2cc6-4efe-b5cb-9fcaa7c87080)


### Multi-omics Analysis 

Histogram depicting the correlation between datasets using Spearman correlation.
                                     
![](https://github.com/user-attachments/assets/1dbf9848-74fc-4605-b6b7-c520e8e71e40)


An interactive module network showing each Data1 modules as a yellow diamond, Data2 module as a green triangle and Data3 modules as a blue circle. Clicking directly on the triangle or diamond identifies the module number. Correlation coefficients are seen on arrows connecting modules. Modules can be fluidly switched into different order and moved on the screen. The network can be downloaded as an html for saving.
                                      
![](https://github.com/user-attachments/assets/a19f842d-f9a2-418f-908f-b63efca1c45e)


Table of the top highly correlated modules, with the number of features within each module, the correlation between modules.

![](https://github.com/user-attachments/assets/cc80d58f-2c48-4611-83b5-634deec518d6)


The drop-down menu displays the details for each of the top slecetd highly correlated modules. Select one option to see the features within each module and the correlation between each feature (Corrplot and table). The arrows to the right of each column title can be used to sort data from increasing or decreasing values. Users can also use the search bar to find the details of a feature of interest. The user can download the full .csv file of Module correlations at the bottom of the table.

![](https://github.com/user-attachments/assets/eba0c466-f03f-4bde-bbcf-63956e04cd15)


Displays the list of features within a Data 1 module that is highly correlated with a Data 2 module. The Data 1 module ID is specified first, followed by the list of constituent fetures. This information facilitates further pathway analysis and provides valuable insights into the relationships between features.

The full .csv file of the list of metabolites to perform pathway analysis further can be downloaded at the bottom of the table.

![](https://github.com/user-attachments/assets/92fe0169-138b-4410-bdca-2d48f5719fa5)


Statistical analysis by Students t-test compares phenotypes chosen from a drop-down menu. The features of top correlated module, determined previously, are used as predictors. The user can also specify a significance threshold for the p-value, with the default set to 0.05. It returns a boxplot

* Class: Lists the two levels of the phenotypes being compared. 
If there are more than two levels, it compares one level against the others. 
Dots marking outliers and a legend describing the compared phenotype.
                                     
![](https://github.com/user-attachments/assets/9cad9a8f-cec8-4723-b871-fe99344e3ee0)
