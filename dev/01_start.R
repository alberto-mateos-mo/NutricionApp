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
  pkg_name = "NutricionApp", # The Name of the package containing the App 
  pkg_title = "NutricionApp", # The Title of the package containing the App 
  pkg_description = "Provides and app to calculate Total Parenteral Nutrition.", # The Description of the package containing the App 
  author_first_name = "David", # Your First Name
  author_last_name = "Mateos", # Your Last Name
  author_email = "davidmateosmo@gmail.com", # Your Email
  repo_url = "https://github.com/alberto-mateos-mo/NutricionApp" # The URL of the GitHub Repo (optional) 
)     

## Set {golem} options ----
golem::set_golem_options()

## Create Common Files ----
## See ?usethis for more information
usethis::use_ccby_license("David Mateos")
usethis::use_readme_rmd( open = FALSE )
usethis::use_code_of_conduct()
usethis::use_lifecycle_badge( "Experimental" )

## Favicon ----
# If you want to change the favicon (default is golem's one)
golem::remove_favicon()

## Add helper functions ----
golem::use_utils_ui()
golem::use_utils_server()

# You're now set! ----

# go to dev/02_dev.R
rstudioapi::navigateToFile( "dev/02_dev.R" )

