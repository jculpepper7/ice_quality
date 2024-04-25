# The objective of this script is to 
# 1. Read in ice quality data with geographic locations
# 2. Get max white ice thickness for the year of the ice blitz project for all lakes
# 3. Make a map of all lakes colored by their max white ice
# 4. Black circle around lakes with time series data? Something to designate it I think


# 1. Load Libraries -------------------------------------------------------

library(tidyverse)
library(rnaturalearth)
library(rnaturalearthdata)
library(rnaturalearthhires)
library(here)
library(janitor)

# 2. Import data ----------------------------------------------------------

fin_white_ice <- read_csv(here('data/finnish_white_ice.csv'))

fin_geo_loc <- read_csv(here('data/finnish_standard.csv'))

ntl_whice_ice <- read_csv(here('data/ntl_ice_quality.csv'))

ice_blitz <- read_delim(here('data/iceblitz_data.csv'), delim = ';')

hal <- read_csv(here('data/haliburton_field_data_2016_17.csv'))


