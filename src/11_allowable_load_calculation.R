library(tidyverse)
library(ncdf4)
library(here)
library(fields)

# 1. Get data -------------------------------------------------------------

ice_thick_data <- nc_open(here('data/iceANNmean_mod_ensmean.nc'))

#latitude and longitude
ilat_id <- ncvar_get(ice_thick_data, 'lat')
ilon_id <- ncvar_get(ice_thick_data, 'lon')

#Time variable (year)
itime_id <- ncvar_get(ice_thick_data, 'year') #between 1850 and 2099

#ice on and off data for calculations and mapping
ice_thk <- ncvar_get(ice_thick_data, 'LAKEICETHICK')

#convert ice thickness to cm (accounting for units of equation)
ice_thk_cm <- ice_thk*100

#Check units
image.plot(ice_thk_cm[,,1]) #looks correct

#Reduce the boundaries of the data to just the Northern Hemisphere
idx_lat <- ilat_id >= 20 & ilat_id <= 90
idx_lon <- ilon_id >= 0 & ilon_id <= 360

lat_nh <- ilat_id[idx_lat]
lon_nh <- ilon_id[idx_lon]

ice_thk_nh <- ice_thk_cm[idx_lon, idx_lat,]

# 2. Calculate allowable load --------------------------------------------

#Allowable load is calculated (from Weyhenmeyer et al., 2022, equation #2)

#Allowable load = P (in kg); 
#Bearing strength = A (between 3.5 kg per cm^2 (white ice) and 17.5 kg per cm^2 (black ice))
#Ice thickness = H (in cm);
#W = % white ice (value between 0 and 100)
#(Note: They do not provide a variable in the paper, but I find it easier to assign it a variable)

#Allowable load equation: P = (A*H^2)/2 * (1+ ((100-W)/100))

#Allowable Load equation 100% black ice
load_p <- function(H, W){
  A <- if_else(W == 100, 3.5,
        if_else(W == 0, 17.5,
          if_else(W == 50, 10.5, as.numeric(NA)) #Using midpoint because it is unclear if the relationship is linear. Presented as linear in the paper.
    )
  );
  P <- ((A*H^2)/2) * (1 + ((100 - W)/100));
  #print(P)
} 

#Test
x <- load_p(H = 10, W = 100)
y <- load_p(H = 10, W = 50)
z <- load_p(H = 10, W = 0)
zz <- load_p(H = 10, W = 25)
#Tests look good and give the same answers as Weyhenmeyer et al., 2022 for 0 and 100 % white ice

load_black_ice <- load_p(H = ice_thk_cm, W = 0)

image.plot(load_black_ice[,,1])

load_white_ice <- load_p(H = ice_thk_cm, W = 100)

image.plot(load_white_ice[,,1])

load_half <- load_p(H = ice_thk_cm, W = 50)

image.plot(load_half[,,1])

