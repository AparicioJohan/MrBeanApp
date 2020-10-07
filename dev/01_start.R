# Building a Prod-Ready, Robust Shiny Application.
# 
# README: each step of the dev files is optional, and you don't have to 
# fill every dev scripts before getting started. 
# 01_start.R should be filled at start. 
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
# 
# 
########################################
#### CURRENT FILE: ON START SCRIPT #####
########################################

## Fill the DESCRIPTION ----
## Add meta data about your application
golem::fill_desc(
  pkg_name = "MrBean", # The Name of the package containing the App 
  pkg_title = "Web application for analyzing field experiments", # The Title of the package containing the App 
  pkg_description = "Mr. Bean is an easy to use R-Shiny web-app that simplifies the analysis of large-scale plant breeding experimental analysis by using the power and versatility of Linear Mixed Models (LMM). This app combines the analytical robustness and speed of ASReml and SpATS with the visual power offered by R.  Mr. Bean provides a graphical workflow for importing data, identifying outliers, and fitting field data using LMM with or without spatial correction. The results are BLUPs/BLUEs predictions and heritabilities for single-environmental experiments or multiple-environmental trial (MET) analysis. In addition, Mr. Bean also provides a module for exploring results from METs using several graphical and multivariate techniques.", # The Description of the package containing the App 
  author_first_name = "Johan", # Your First Name
  author_last_name = "Aparicio", # Your Last Name
  author_email = "j.aparicio@cgiar.org", # Your Email
  repo_url = "https://github.com/AparicioJohan/MrBeanApp" # The URL of the GitHub Repo (optional) 
)     

## Set {golem} options ----
golem::set_golem_options()

## Create Common Files ----
## See ?usethis for more information
usethis::use_mit_license( name = "Johan Aparicio" )  # You can set another license here
usethis::use_readme_rmd( open = FALSE )
usethis::use_code_of_conduct()
usethis::use_lifecycle_badge( "Experimental" )
usethis::use_news_md( open = FALSE )

## Use git ----
usethis::use_git()

## Init Testing Infrastructure ----
## Create a template for tests
golem::use_recommended_tests()

## Use Recommended Packages ----
golem::use_recommended_deps()

## Favicon ----
# If you want to change the favicon (default is golem's one)
golem::remove_favicon()
golem::use_favicon() # path = "path/to/ico". Can be an online file. 

## Add helper functions ----
golem::use_utils_ui()
golem::use_utils_server()

# You're now set! ----

# go to dev/02_dev.R
rstudioapi::navigateToFile( "dev/02_dev.R" )

