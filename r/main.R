##############
## Main script
##############

data_folder <- "data/zenodo/processed_data/"
bassin <- "loire" # "loire" or "vilaine"
year_of_interest <- 1990:2024 # time period
spp_to_plot <- "ANG" # Anguilla
make_plots <- T

## Load packages and functions
#-----------------------------
source("r/source/packages_functions.R")


## Load data and join the different tables
#-----------------------------------------
source("r/source/load_data.R")


## Filter data (species, years)
#------------------------------
source("r/source/filter_data.R")


## Simplify the stream (if necessary)
#------------------------------------
## CAREFULL - NEED TO BE DOUBLE CHECKED
source("r/source/simplify_stream.R")


## Reverse the stream network so that it is upstream
#---------------------------------------------------
source("r/source/reverse_network.R")


## Shape data
#------------
source("r/source/shape_data.R")

## Fit model
#-----------
out = tinyVAST( data = Data,
  family = gaussian(),
  formula = Count ~ 1,
  spatial_domain = graph,
  space_column = c("X","Y"),
  variable_column = "var",
  time_column = "time",
  times = seq_times,
  distribution_column = "dist",
  space_term = "" ,
  time_term = "" )


## Plot predictions and parameters
#---------------------------------
source("r/source/plot_predictions_and_estimates.R")
