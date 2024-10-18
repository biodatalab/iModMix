# Building a Prod-Ready, Robust Shiny Application.
#
# README: each step of the dev files is optional, and you don't have to
# fill every dev scripts before getting started.
# 01_start.R should be filled at start.
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
#
#
###################################
#### CURRENT FILE: DEV SCRIPT #####
###################################

# Engineering

## Dependencies ----
## Amend DESCRIPTION with dependencies read from package code parsing
usethis::use_package("glassoFast")
usethis::use_package("stringr")
usethis::use_package("WGCNA")
usethis::use_package("dplyr")
usethis::use_package("enrichR")
usethis::use_package("igraph")
usethis::use_package("stats")
usethis::use_package("tidyverse")
usethis::use_package("pROC")
usethis::use_package("randomForest")
usethis::use_package("class")
usethis::use_package("purrr")
usethis::use_package("impute")
usethis::use_package("gridExtra")
usethis::use_package("ComplexHeatmap")
usethis::use_package("devtools")
usethis::use_package("ggplot2")
usethis::use_package("ggfortify")
usethis::use_package("RColorBrewer")
usethis::use_package("cowplot")
usethis::use_package("grid")
usethis::use_package("plotly")
usethis::use_package("visNetwork")
usethis::use_package("shinyWidgets")
usethis::use_package("shinyBS")
usethis::use_package("viridis")
usethis::use_package("corrplot")
usethis::use_package("reshape2")
usethis::use_package("preprocessCore")
usethis::use_package("GO.db")
usethis::use_package("impute")

## install.packages('attachment') # if needed.
attachment::att_amend_desc()

## Add modules ----
## Create a module infrastructure in R/
golem::add_module(name = "module1") # Name of the module
golem::add_module(name = "name_of_module2", with_test = TRUE) # Name of the module

## Add helper functions ----
## Creates fct_* and utils_*
golem::add_fct("partial_cors", with_test = TRUE)
golem::add_fct("hierarchical_cluster", with_test = TRUE)
golem::add_fct("cluster_assignments_metabolites", with_test = TRUE)
golem::add_fct("cluster_assignments_genes", with_test = TRUE)
golem::add_fct("Assigment_genes_enrichr", with_test = TRUE)
golem::add_fct("Eigengenes", with_test = TRUE)
golem::add_fct("Modules_correlation", with_test = TRUE)
golem::add_fct("FeaturesAnnot_correlation", with_test = TRUE)
golem::add_fct("annotation_matrices_list", with_test = TRUE)
golem::add_fct("perform_classification", with_test = TRUE)
golem::add_fct("load_data", with_test = TRUE)
golem::add_utils("utils_load", with_test = TRUE)

## External resources
## Creates .js and .css files at inst/app/www
golem::add_js_file("script")
golem::add_js_handler("handlers")
golem::add_css_file("custom")
golem::add_sass_file("custom")

## Add internal datasets ----
## If you have data in your package
usethis::use_data_raw(name = "my_dataset", open = FALSE)

## Tests ----
## Add one line by test you want to create
usethis::use_test("app")

# Documentation

## Vignette ----
usethis::use_vignette("iModMix")
devtools::build_vignettes()

## Code Coverage----
## Set the code coverage service ("codecov" or "coveralls")
usethis::use_coverage()

# Create a summary readme for the testthat subdirectory
covrpage::covrpage()

## CI ----
## Use this part of the script if you need to set up a CI
## service for your application
##
## (You'll need GitHub there)
usethis::use_github()

# GitHub Actions
usethis::use_github_action()
# Chose one of the three
# See https://usethis.r-lib.org/reference/use_github_action.html
usethis::use_github_action_check_release()
usethis::use_github_action_check_standard()
usethis::use_github_action_check_full()
# Add action for PR
usethis::use_github_action_pr_commands()

# Travis CI
usethis::use_travis()
usethis::use_travis_badge()

# AppVeyor
usethis::use_appveyor()
usethis::use_appveyor_badge()

# Circle CI
usethis::use_circleci()
usethis::use_circleci_badge()

# Jenkins
usethis::use_jenkins()

# GitLab CI
usethis::use_gitlab_ci()

# You're now set! ----
# go to dev/03_deploy.R
rstudioapi::navigateToFile("dev/03_deploy.R")
