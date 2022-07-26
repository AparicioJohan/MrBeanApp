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
## Add one line by package you want to add as dependency
usethis::use_package( "SpATS" )
usethis::use_package( "bs4Dash" )
usethis::use_package( "ggplot2" )
usethis::use_package( "shinyjs" )
usethis::use_package( "rintrojs" )
usethis::use_package( "shinytoastr" )
usethis::use_package( "shinyalert" )
usethis::use_package( "shinyWidgets" )
usethis::use_package( "dplyr" )
usethis::use_package( "shinycssloaders" )
usethis::use_package( "plotly" )
usethis::use_package( "summarytools" )
usethis::use_package( "ggpubr" )
usethis::use_package( "lme4" )
usethis::use_package( "broom.mixed" )
usethis::use_package( "lmerTest" )
usethis::use_package( "magrittr" )
usethis::use_package( "data.table" )
usethis::use_package( "tidyr" )
usethis::use_package( "readxl" )
usethis::use_package( "tools" )
usethis::use_package( "echarts4r" )
usethis::use_package( "formattable" )
usethis::use_package( "kableExtra" )
usethis::use_package( "tibble" )
usethis::use_package( "Matrix" )
usethis::use_package( "reshape" )
usethis::use_package( "factoextra" )
usethis::use_package( "psych" )
usethis::use_package( "sever" )
usethis::use_package( "waiter" )
usethis::use_package( "ggrepel" )
usethis::use_package( "emmeans" )
usethis::use_package( "QBMS" )
usethis::use_dev_package("brapirv1", type = "Imports", remote = "mverouden/brapir-v1")

## Add modules ----
## Create a module infrastructure in R/
golem::add_module( name = "home_module1" ) # Name of the module
golem::add_module( name = "import_dt" ) # Name of the module
golem::add_module( name = "dataBMS" ) # Name of the module
golem::add_module( name = "descrip_scatter" ) # Name of the module
golem::add_module( name = "descrip_boxplot" ) # Name of the module
golem::add_module( name = "descrip_raw" ) # Name of the module
golem::add_module( name = "distribution" ) # Name of the module
golem::add_module( name = "spats_single" ) # Name of the module
golem::add_module( name = "info_spats" ) # Name of the module
golem::add_module( name = "effects_spats" ) # Name of the module
golem::add_module( name = "residuals_spats" ) # Name of the module
golem::add_module( name = "lme4_single" ) # Name of the module
golem::add_module( name = "residuals_lme4" ) # Name of the module
golem::add_module( name = "about" ) # Name of the module
golem::add_module( name = "MSA" ) # Name of the module
golem::add_module( name = "MSA_results" ) # Name of the module
golem::add_module( name = "M_traits") # Name of the module
golem::add_module( name = "aug_model" ) # Name of the module
golem::add_module( name = "aug_result" ) # Name of the module
golem::add_module( name = "spats_asreml" ) # Name of the module
golem::add_module( name = "spats_asreml_effects" ) # Name of the module
golem::add_module( name = "asreml_selector" ) # Name of the module
golem::add_module( name = "asreml_selector_effects" ) # Name of the module
golem::add_module( name = "MET" ) # Name of the module
golem::add_module( name = "MET_results" ) # Name of the module
golem::add_module( name = "MET_FA" ) # Name of the module
golem::add_module( name = "GBLUP" ) # Name of the module
golem::add_module( name = "GBLUP_results" ) # Name of the module



## Add helper functions ----
## Creates ftc_* and utils_*
golem::add_fct( "helpers" ) 
golem::add_utils( "helpers" )

## External resources
## Creates .js and .css files at inst/app/www
golem::add_js_file( "script" )
golem::add_js_handler( "handlers" )
golem::add_css_file( "custom" )

## Add internal datasets ----
## If you have data in your package
usethis::use_data_raw( name = "data", open = FALSE ) 

## Tests ----
## Add one line by test you want to create
usethis::use_test( "app" )

# Documentation

## Vignette ----
usethis::use_vignette("MrBean")
devtools::build_vignettes()

## Code coverage ----
## (You'll need GitHub there)
usethis::use_github()
usethis::use_travis()
usethis::use_appveyor()

# You're now set! ----
# go to dev/03_deploy.R
rstudioapi::navigateToFile("dev/03_deploy.R")

