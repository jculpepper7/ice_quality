# The purpose of this script is to make a 
# stand-in figure for the under-ice ecology section
# We will use data from Shchapov et al., 2023
# Get doi: 


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(here)
library(janitor)
library(lubridate)

# 1. Read and clean data --------------------------------------------------

winter_zoops <- read_csv(here('data/winter_zoop_data.csv'))

head(winter_zoops)

winter_zoops_clean <- winter_zoops %>% 
  select(1:7) %>% 
  clean_names() %>% 
  mutate(
    date = dmy(date),
    white_ice = as.numeric(white_ice),
    lake = as.factor(lake),
    color_trophic_status = as.factor(color_trophic_status),
    white_ice_ratio = white_ice/ice_thickness_m
  ) %>% 
  #remove extreme outlier
  filter(
    tot_zoops_ind_per_l < 50
  )


# 2. Plot correlation between white ice and zoops -------------------------

ggplot(data = winter_zoops_clean)+
  geom_smooth(aes(y = tot_zoops_ind_per_l, x = white_ice_ratio), method = 'lm' )+
  geom_point(aes(x = white_ice, y = tot_zoops_ind_per_l))+
  theme_classic()+
  xlab('White Ice ratio')+
  ylab('Zooplankton Density (ind/L)')

#Really doesn't tell you much
#Would need a lot more data

# 3. Plot time series of ice and zoops ---------------------------------------

#Only going to use lakes with more than one observation
winter_zoops_abbr <- winter_zoops_clean[7:10,]

#time series plot
ggplot(data = winter_zoops_abbr)+
  geom_line(aes(x = date, y = tot_zoops_ind_per_l))+
  geom_line(aes(x = date, y = white_ice_ratio*10), linetype = 'dashed')+
  theme_classic()+
  ylab('Zooplankton Density (ind/L)\nWhite Ice Fraction (x10)')+
  xlab('')
  
