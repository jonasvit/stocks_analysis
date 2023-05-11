################################################################################
# @Name               Jonas Vitkauskas
# @Role               ---
# @Project            Forecasting stocks
# @Description        Project aim is to forecast stocks in the future, create strategies and evaluate their performances
# @Data servers       .csv file provided
################################################################################


#################################################################
## Running all scripts
#################################################################

## set the directory to current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


source("./01_setup.R")
source("./02_functions.R")

# Extract data that was provided
instrument_data <- fread("../input/instrument_data.csv", header = T)

source("./03_strategies.R")

# If you want to check the best optimal MAs over some period of time, run scripts 01, 02,
# import instrument_data and run the line below - 04.
# source("./04_best_strategy.R")

# If you want to generate R markdown document, run ONLY this line:
# rmarkdown::render("./documentation_and_results.Rmd")
