# File Author(s) (ordered by contribution): Katja Mathesius, Caleb Patterson
# Project Contributors (ordered alphabetically): Katja Mathesius, Caleb Patterson, Hming Zama
# Description: An analysis aiming to utilize publicly available information about grid demand and
#              commonly occurring risks to the grid in order to evaluate ideal locations to expand
#              resources into
# Active Maintenance Time Frame: Feb 2024-May 2024
# Github: https://github.com/katmathematics/STAT-190-Project-1

# Clear the environment
rm(list = ls())

# Install packages if not installed, then load packages
packages <- c("reticulate")
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}
invisible(lapply(packages, library, character.only = TRUE))


### Set to FALSE to run the entire process automatically. Set to true to be prompted 
### for which files you would like to run 
RUN_INTERACTIVELY = FALSE

### Default value for which files to run

# BPA Workflow: Scraping+Cleaning R script -> Analysis R script
run_BPA_Scraper = TRUE
run_BPA_Analysis = TRUE

# EIA Workflow: Data Manually Downloaded from EIA -> Cleaning R script -> Analysis R script
run_EIA_Cleaning = TRUE
run_EIA_Analysis = TRUE

# NCEI Workflow: Scraping Python script -> Cleaning R script -> Analysis R script
run_NCEI_Scraper = TRUE
run_NCEI_Cleaning = TRUE
run_NCEI_Analysis = TRUE

# Wildfire Workflow: Data Manually Downloaded from -> Cleaning R script -> Analysis R script
run_Wildfire_Cleaning = TRUE
run_Wildfire_Analysis = TRUE

# Merge Data Sources
run_Merge_Data_Sources = TRUE

# Complete Analaysis
run_Complete_Analysis = TRUE

# Aids in initializing whether or not certain files should be run based on command line input.
# Loops as long as a definitive yes/no response has not been provided
setup_runner <- function(prompt_text) {
  # Define a list of responses to check for
  positive_responses <- list("yes","y","ye","ja","да","oui","tak","true","t","1")
  negative_responses <- list("no","n","nah","nein","нет","non","nie","false","f","0")
  
  loop_here = TRUE
  while (loop_here == TRUE) {
    input = tolower(readline(prompt = prompt_text));
    if (input %in% positive_responses) {
      loop_here = FALSE
      return (TRUE)
    }
    else if(input %in% negative_responses){
      loop_here = FALSE
      return(FALSE)
    }
    else {
      print("Please respond \"Yes\" or \"No \" to each prompt.", quote = FALSE)
    }
  }
}

# Runs the interactive set up for which code files to run
if (RUN_INTERACTIVELY) {
  # Prints explanatory text to the console
  print("--------", quote = FALSE)
  print("Welcome to the runner for the grid demand/risk analysis code!", quote = FALSE)
  print("The following set up will walk you through running your desired sequence of code files. You will be given a series of prompts, and then the code will be run after all responses have been gathered. Please respond \"Yes\" or \"No \" to each prompt.", quote = FALSE)
  
  ### BPA Cleaning Set_UP
  prompt_text = "Would you like to run the code for DOWNLOADING/CLEANING power outage data from the Bonneville Power Administration website? (Yes/No): "
  run_BPA_Scraper = setup_runner(prompt_text)
  
  ### BPA Analysis Set_UP
  prompt_text = "Would you like to run the code for ANALYZING power outage data from the Bonneville Power Administration? (Yes/No): "
  run_BPA_Analysis = setup_runner(prompt_text)
  
  ### NCEI_Lightning Scraping Set_Up
  prompt_text = "Would you like to run the code for DOWNLOADING lightning data from the National Centers for Environmental Information website? (Yes/No): "
  run_NCEI_Scraper = setup_runner(prompt_text)
  
  ### NCEI_Lightning Cleaning Set_Up
  prompt_text = "Would you like to run the code for CLEANING lightning data from the National Centers for Environmental Information? (Yes/No): "
  run_NCEI_Cleaning = setup_runner(prompt_text)
  
  ### NCEI_Lightning Analysis Set_Up
  prompt_text = "Would you like to run the code for ANALYZING lightning data from the National Centers for Environmental Information? (Yes/No): "
  run_NCEI_Analysis = setup_runner(prompt_text)
  
  ### EIA_Interchange Cleaning Set Up
  prompt_text = "Would you like to run the code for CLEANING grid demand data from the U.S. Energy Information Administration? (Yes/No): "
  run_EIA_Cleaning = setup_runner(prompt_text)
  
  ### EIA Analysis Set Up
  prompt_text = "Would you like to run the code for ANALYZING grid demand data from the U.S. Energy Information Administration? (Yes/No): "
  run_EIA_Analysis = setup_runner(prompt_text)
  
  ### Wildfire Cleaning Set Up
  prompt_text = "Would you like to run the code for CLEANING wildfire data from the Fort Collins, CO: Forest Service Research Data Archive? (Yes/No): "
  run_Wildfire_Cleaning = setup_runner(prompt_text)
  
  ### Wildfire Analysis Set Up
  prompt_text = "Would you like to run the code for ANALYZING grid demand data from the Fort Collins, CO: Forest Service Research Data Archive? (Yes/No): "
  run_Wildfire_Analysis = setup_runner(prompt_text)
  
  ### Merge Data
  prompt_text = "Would you like to run the code for MERGING data sources? (Yes/No): "
  run_Merge_Data_Sources = setup_runner(prompt_text)
  
  ### Complete Analysis
  prompt_text = "Would you like to run the code for COMPLETE ANALYSIS? (Yes/No): "
  run_Complete_Analysis = setup_runner(prompt_text)
}


# Create all the directories we need
mainDir = getwd()
dir.create(file.path(mainDir, "data/compressed_raw_data"), showWarnings = FALSE)
dir.create(file.path(mainDir, "data/cleaned_data"), showWarnings = FALSE)
dir.create(file.path(mainDir, "data/model_data"), showWarnings = FALSE)
dir.create(file.path(mainDir, "data/prediction_data"), showWarnings = FALSE)
dir.create(file.path(mainDir, "data/web_data/ncei_data"), showWarnings = FALSE)
dir.create(file.path(mainDir, "data/web_data/bpa_data"), showWarnings = FALSE)

# Runs any files configured to run
if (run_BPA_Scraper) {
  source("R/BPAScrapingCleaning.R")
}

if (run_BPA_Analysis) {
  source("R/BPAAnalysis.R")
}

if (run_EIA_Cleaning) {
  source("R/EIACleaning.R")
}

if (run_EIA_Analysis) {
  source("R/EIAAnalysis.R")
}

if (run_NCEI_Scraper) {
  system('pip install -r Python/requirements.txt')
  system('python Python/GetData.py ncei data/web_data/ncei_data')
}

if (run_NCEI_Cleaning) {
  source("R/NCEILightningCleaning.R")
}  
  
if (run_Wildfire_Cleaning) {
  source("R/WildfireCleaning.R")
}  

if (run_Wildfire_Analysis) {
  source("R/WildfireAnalysis.R")
}

if (run_Merge_Data_Sources) {
  source("R/MergeDataSources.R")
}

if (run_Complete_Analysis) {
  source("R/CompleteAnalysis.R")
}
