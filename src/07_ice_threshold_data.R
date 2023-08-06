library(fields)
library(tidyverse)
library(ncdf4)
library(here)

# 1 Get data----
on_data <- nc_open(here('data/Iceon_ensmean.nc'))
off_data <- nc_open(here('data/Iceoff_ensmean.nc'))

#latitude and longitude
ilat_id <- ncvar_get(on_data, 'lat')
ilon_id <- ncvar_get(on_data, 'lon')

#Time variable (year)
itime_id <- ncvar_get(on_data, 'year') #between 1850 and 2099

#Threshold variable (ice thickness in inches)
threshold <- ncvar_get(on_data, 'threshold') #depth in inches (0,2,4,5,8,12,42) in cm, that's (0, 5.08, 10.16, 12.7, 20.32, 30.48, 106.68)

#ice on and off data for calculations and mapping
ice_on <- ncvar_get(on_data, 'iceon')
ice_off <- ncvar_get(off_data, 'iceoff')

#Quick look at the data
image.plot(ice_on[,,1,1]) #DOY since Jan for day of ice on (>365 is for next year)

#Reduce the boundaries of the data to just the Northern Hemisphere
idx_lat <- ilat_id >= 20 & ilat_id <= 90
idx_lon <- ilon_id >= 0 & ilon_id <= 360

lat_nh <- ilat_id[idx_lat]
lon_nh <- ilon_id[idx_lon]

#Reduce the boudaries of the data to just North America
#Note: This is for the maps showing the viability of snowmobile travel
# idx_lat2 = ilat_id >= 40 & ilat_id <= 77
# idx_lon2 = ilon_id >= 190 & ilon_id <= 330
# 
# lat_na <- ilat_id[idx_lat2]
# lon_na <- ilon_id[idx_lon2]

#Variable with data confined to Northern Hemisphere
ice_on_nh <- ice_on[idx_lon, idx_lat,,]
ice_off_nh <- ice_off[idx_lon, idx_lat,,]

#Variable with data confined to North America
# ice_on_na <- ice_on[idx_lon2, idx_lat2,,]
# ice_off_na <- ice_off[idx_lon2, idx_lat2,,]

#Generate historical average
idx_base <- itime_id >= 1851 & itime_id <= 1880

#Get the mean of the historic period
ice_on_clim <- apply(ice_on_nh[,,idx_base,], c(1,2,4), mean, na.rm = TRUE)
ice_on_clim2 <- replicate(length(itime_id), ice_on_clim)
ice_on_clim3 <- aperm(ice_on_clim2, c(1,2,4,3)) #Must permute the data array, because it is not in the same order as the ice_on_nh data
ice_on_anom <- ice_on_nh - ice_on_clim3 #Error: non-conformable arrays #Just need to swap 7 and 250 ----> aperm(c(1,2,3,....))


on_anom_test <- apply(ice_on_anom[,,,threshold == 12], 3, mean, na.rm = T)

str(on_anom_test)
plot(on_anom_test, type = 'l')

#Cannot subtract from full time series, so I am attempting individual segments
#associated with warming thresholds 
#Note: The below code works, but I'm uncertain it is entirely correct.
#      Check with Iestyn before proceeding with final plots

#alternative anomaly method----
#Isolate years associated with warming thresholds
idx_base1 <- itime_id >= 2009 & itime_id <= 2011 #1 degree warming
idx_base2 <- itime_id >= 2042 & itime_id <= 2044 #2 degrees warming
idx_base4 <- itime_id >= 2086 & itime_id <= 2088 #3 degrees warming

#Average for each time period
ice_on_clim_1deg <- apply(ice_on_nh[,,idx_base1,], c(1,2,4), mean, na.rm = TRUE)
ice_on_clim_2deg <- apply(ice_on_nh[,,idx_base2,], c(1,2,4), mean, na.rm = TRUE)
ice_on_clim_4deg <- apply(ice_on_nh[,,idx_base4,], c(1,2,4), mean, na.rm = TRUE)


ice_on_anom1 <- ice_on_clim - ice_on_clim_1deg 
ice_on_anom2 <- ice_on_clim - ice_on_clim_2deg 
ice_on_anom4 <- ice_on_clim - ice_on_clim_4deg 

# Anomaly maps for ice on----

#1 degree warming anomaly map
ice_on_map1 <- mapCDFtemp(lat_nh, lon_nh, ice_on_anom1[,,3], 
                        name_cmocean = 'matter', 
                        direction = 1, 
                        limits = c(-60,0),
                        grid_size = 2,
                        titletext = 'Ice On - 1C Warming')
ice_on_map1

#ggsave(here('results/plots/00_iceon_4cm_1deg.png'), dpi = 300, width = 9, height = 9, units = 'in')

#2 degrees warming anomaly map
ice_on_map2 <- mapCDFtemp(lat_nh, lon_nh, ice_on_anom2[,,3], 
                          name_cmocean = 'gray', direction = 1, 
                          limits = c(-60,0),
                          grid_size = 2,
                          titletext = 'Ice On - 2C Warming')
ice_on_map2

#ggsave(here('results/plots/00_iceon_4cm_2deg.png'), dpi = 300, width = 9, height = 9, units = 'in')

#4 degrees warming anomaly map
ice_on_map4 <- mapCDFtemp(lat_nh, lon_nh, ice_on_anom4[,,3], 
                          name_cmocean = 'gray', direction = 1, 
                          limits = c(-60,0),
                          grid_size = 2,
                          titletext = 'Ice On - 4C Warming')
ice_on_map4

#1 degree warming anomaly map
ice_on5_map1 <- mapCDFtemp(lat_nh, lon_nh, ice_on_anom1[,,4], 
                          name_cmocean = 'matter', 
                          direction = 1, 
                          limits = c(-60,0),
                          grid_size = 2,
                          titletext = '')
ice_on5_map1

#ggsave(here('results/plots/00_iceon_4cm_1deg.png'), dpi = 300, width = 9, height = 9, units = 'in')

#2 degrees warming anomaly map
ice_on5_map2 <- mapCDFtemp(lat_nh, lon_nh, ice_on_anom2[,,4], 
                          name_cmocean = 'gray', direction = 1, 
                          limits = c(-60,0),
                          grid_size = 2,
                          titletext = '')
ice_on5_map2

#ggsave(here('results/plots/00_iceon_4cm_2deg.png'), dpi = 300, width = 9, height = 9, units = 'in')

#4 degrees warming anomaly map
ice_on5_map4 <- mapCDFtemp(lat_nh, lon_nh, ice_on_anom4[,,4], 
                          name_cmocean = 'gray', direction = 1, 
                          limits = c(-60,0),
                          grid_size = 2,
                          titletext = '')
ice_on5_map4

figure <- ggarrange(ice_on_map1, ice_on_map2, ice_on_map4,
                    ice_on5_map1, ice_on5_map2, ice_on5_map4,
                    labels = c("a", "b", "c", "d", "e", "f"),
                    ncol = 3, nrow = 2)
figure

ggsave(here('results/plots/ice_safety_walking_maps.png'), dpi = 300, width = 12, height = 12, units = 'in')

# DOY Maps for ice on----

#Historic conditions
ice_on_doy_hist <- mapCDFtemp(lat_nh, lon_nh, ice_on_clim[,,2], 
                          name_cmocean = 'gray', direction = -1, 
                          limits = c(247,480), 
                          titletext = 'Ice On DOY - Historic')
ice_on_doy_hist

ggsave(here('results/plots/01_iceon_4cm_hist.png'), dpi = 300, width = 9, height = 9, units = 'in')

#1 degree warming
ice_on_doy_1deg <- mapCDFtemp(lat_nh, lon_nh, ice_on_clim_1deg[,,2], 
                              name_cmocean = 'gray', direction = -1, 
                              limits = c(247,480), 
                              titletext = 'Ice On DOY - 1C')
ice_on_doy_1deg

ggsave(here('results/plots/01_iceon_4cm_1deg.png'), dpi = 300, width = 9, height = 9, units = 'in')

#2 degrees warming
ice_on_doy_2deg <- mapCDFtemp(lat_nh, lon_nh, ice_on_clim_2deg[,,2], 
                              name_cmocean = 'gray', direction = -1, 
                              limits = c(247,480), 
                              titletext = 'Ice On DOY - 2C')
ice_on_doy_2deg

ggsave(here('results/plots/01_iceon_4cm_2deg.png'), dpi = 300, width = 9, height = 9, units = 'in')

#4 degree warming
ice_on_doy_4deg <- mapCDFtemp(lat_nh, lon_nh, ice_on_clim_4deg[,,2], 
                              name_cmocean = 'gray', direction = -1, 
                              limits = c(247,480), 
                              titletext = 'Ice On DOY - 4C')
ice_on_doy_4deg

ggsave(here('results/plots/01_iceon_4cm_4deg.png'), dpi = 300, width = 9, height = 9, units = 'in')

# 3. Ice off calculations and mapping----

#Get the mean of the historic period
ice_off_clim <- apply(ice_off_nh[,,idx_base,], c(1,2,4), mean, na.rm = TRUE)
ice_off_clim2 <- replicate(length(itime_id), ice_off_clim)
ice_off_anom <- ice_off_clim - ice_off_clim2 #Error: non-conformable arrays

#Cannot subtract from full time series, so I am attempting individual segments
#associated with warming thresholds 
#Note: The below code works, but I'm uncertain it is entirely correct.
#      Check with Iestyn before proceeding with final plots

#Isolate years associated with warming thresholds
idx_base1 <- itime_id >= 2009 & itime_id <= 2011 #1 degree warming
idx_base2 <- itime_id >= 2042 & itime_id <= 2044 #2 degrees warming
idx_base4 <- itime_id >= 2086 & itime_id <= 2088 #3 degrees warming

#Average for each time period
ice_off_clim_1deg <- apply(ice_off_nh[,,idx_base1,], c(1,2,4), mean, na.rm = TRUE)
ice_off_clim_2deg <- apply(ice_off_nh[,,idx_base2,], c(1,2,4), mean, na.rm = TRUE)
ice_off_clim_4deg <- apply(ice_off_nh[,,idx_base4,], c(1,2,4), mean, na.rm = TRUE)


ice_off_anom1 <- ice_off_clim - ice_off_clim_1deg 
ice_off_anom2 <- ice_off_clim - ice_off_clim_2deg 
ice_off_anom4 <- ice_off_clim - ice_off_clim_4deg 

mean(ice_off_anom1, na.rm = T)
# Anomaly maps for ice on----

#1 degree warming anomaly map
ice_off_map1 <- mapCDFtemp(lat_nh, lon_nh, ice_off_anom1[,,2], 
                          name_cmocean = 'gray', direction = -1, 
                          limits = c(0,80), 
                          titletext = 'Ice Off - 1C Warming')
ice_off_map1

#ggsave(here('results/plots/02_iceoff_4cm_1deg.png'), dpi = 300, width = 9, height = 9, units = 'in')

#2 degrees warming anomaly map
ice_off_map2 <- mapCDFtemp(lat_nh, lon_nh, ice_off_anom2[,,2], 
                          name_cmocean = 'gray', direction = -1, 
                          limits = c(0,80), 
                          titletext = 'Ice Off - 2C Warming')
ice_off_map2

#ggsave(here('results/plots/02_iceoff_4cm_2deg.png'), dpi = 300, width = 9, height = 9, units = 'in')

#3 degrees warming anomaly map
ice_on_map4 <- mapCDFtemp(lat_nh, lon_nh, ice_off_anom4[,,2], 
                          name_cmocean = 'gray', direction = -1, 
                          limits = c(0,80), 
                          titletext = 'Ice Off - 4C Warming')
ice_on_map4

#ggsave(here('results/plots/02_iceoff_4cm_4deg.png'), dpi = 300, width = 9, height = 9, units = 'in')

# DOY Maps for ice on----
#range(ice_off_clim_4deg, na.rm = T)
#Historic conditions
ice_off_doy_hist <- mapCDFtemp(lat_nh, lon_nh, ice_off_clim[,,2], 
                              name_cmocean = 'gray', direction = 1, 
                              limits = c(365,550), 
                              titletext = 'Ice Off DOY - Historic')
ice_off_doy_hist

#ggsave(here('results/plots/03_iceoff_4cm_hist.png'), dpi = 300, width = 9, height = 9, units = 'in')

#1 degree warming
ice_off_doy_1deg <- mapCDFtemp(lat_nh, lon_nh, ice_off_clim_1deg[,,2], 
                              name_cmocean = 'gray', direction = 1, 
                              limits = c(365,550), 
                              titletext = 'Ice Off DOY - 1C')
ice_off_doy_1deg

#ggsave(here('results/plots/03_iceoff_4cm_1deg.png'), dpi = 300, width = 9, height = 9, units = 'in')

#2 degrees warming
ice_off_doy_2deg <- mapCDFtemp(lat_nh, lon_nh, ice_off_clim_2deg[,,2], 
                              name_cmocean = 'gray', direction = 1, 
                              limits = c(365,550), 
                              titletext = 'Ice Off DOY - 2C')
ice_off_doy_2deg

#ggsave(here('results/plots/03_iceoff_4cm_2deg.png'), dpi = 300, width = 9, height = 9, units = 'in')

#4 degree warming
ice_off_doy_4deg <- mapCDFtemp(lat_nh, lon_nh, ice_off_clim_4deg[,,2], 
                              name_cmocean = 'gray', direction = 1, 
                              limits = c(365,550), 
                              titletext = 'Ice Off DOY - 4C')
ice_off_doy_4deg

#ggsave(here('results/plots/03_iceoff_4cm_4deg.png'), dpi = 300, width = 9, height = 9, units = 'in')

# 4. Get snowmobile accessibility maps------------------------------------------

#both 13 and 20 cm ice thickness
#Note: for the time being, this mimics the black ice and white ice scenarios
#until we can figure out the issue with the time series data
ice_on_clim_sm <- apply(ice_on_na[,,idx_base,], c(1,2,4), mean, na.rm = TRUE)

#Isolate years associated with warming thresholds
idx_base1 <- itime_id >= 2009 & itime_id <= 2011 #1 degree warming
idx_base2 <- itime_id >= 2042 & itime_id <= 2044 #2 degrees warming
idx_base4 <- itime_id >= 2086 & itime_id <= 2088 #3 degrees warming

#Average for each time period
ice_on_clim_1deg_sm <- apply(ice_on_na[,,idx_base1,], c(1,2,4), mean, na.rm = TRUE)
ice_on_clim_2deg_sm <- apply(ice_on_na[,,idx_base2,], c(1,2,4), mean, na.rm = TRUE)
ice_on_clim_4deg_sm <- apply(ice_on_na[,,idx_base4,], c(1,2,4), mean, na.rm = TRUE)

#Get anomalies for 13 and 20 cm

#13cm anomaly
ice_on_anom1_sm <- ice_on_clim_sm - ice_on_clim_1deg_sm 
ice_on_anom2_sm <- ice_on_clim_sm - ice_on_clim_2deg_sm 
ice_on_anom4_sm <- ice_on_clim_sm - ice_on_clim_4deg_sm 

#13cm maps

#1 Degree warming
ice_on_anom_1deg_13cm <- mapCDFtemp2(lat_na, lon_na, ice_on_anom1_sm[,,4], 
                               name_cmocean = 'gray', direction = 1, 
                               limits = c(-80,0), 
                               titletext = 'Ice On Anomaly (13cm) - 1C')
ice_on_anom_1deg_13cm

ggsave(here('results/plots/04_iceon_13cm_1deg.png'), dpi = 300, width = 9, height = 9, units = 'in')

#2 Degrees warming
ice_on_anom_2deg_13cm <- mapCDFtemp2(lat_na, lon_na, ice_on_anom2_sm[,,4], 
                                     name_cmocean = 'gray', direction = 1, 
                                     limits = c(-80,0), 
                                     titletext = 'Ice On Anomaly (13cm) - 2C')
ice_on_anom_2deg_13cm


ggsave(here('results/plots/04_iceon_13cm_2deg.png'), dpi = 300, width = 9, height = 9, units = 'in')

#4 Degrees warming
ice_on_anom_4deg_13cm <- mapCDFtemp2(lat_na, lon_na, ice_on_anom4_sm[,,4], 
                                     name_cmocean = 'gray', direction = 1, 
                                     limits = c(-80,0), 
                                     titletext = 'Ice On Anomaly (13cm) - 4C')
ice_on_anom_4deg_13cm

ggsave(here('results/plots/04_iceon_13cm_4deg.png'), dpi = 300, width = 9, height = 9, units = 'in')

#20cm maps

#1 Degree warming
ice_on_anom_1deg_20cm <- mapCDFtemp2(lat_na, lon_na, ice_on_anom1_sm[,,5], 
                                     name_cmocean = 'gray', direction = 1, 
                                     limits = c(-80,0), 
                                     titletext = 'Ice On Anomaly (13cm) - 1C')
ice_on_anom_1deg_20cm

ggsave(here('results/plots/04_iceon_20cm_1deg.png'), dpi = 300, width = 9, height = 9, units = 'in')

#2 Degrees warming
ice_on_anom_2deg_20cm <- mapCDFtemp2(lat_na, lon_na, ice_on_anom2_sm[,,4], 
                                     name_cmocean = 'gray', direction = 1, 
                                     limits = c(-80,0), 
                                     titletext = 'Ice On Anomaly (13cm) - 2C')
ice_on_anom_2deg_13cm


ggsave(here('results/plots/04_iceon_13cm_2deg.png'), dpi = 300, width = 9, height = 9, units = 'in')

#4 Degrees warming
ice_on_anom_4deg_13cm <- mapCDFtemp2(lat_na, lon_na, ice_on_anom4_sm[,,4], 
                                     name_cmocean = 'gray', direction = 1, 
                                     limits = c(-80,0), 
                                     titletext = 'Ice On Anomaly (13cm) - 4C')
ice_on_anom_4deg_13cm

ggsave(here('results/plots/04_iceon_13cm_4deg.png'), dpi = 300, width = 9, height = 9, units = 'in')
# 5. Get shoulder season length for ice on and ice off--------------------------

#Ice on

#13 cm at historical
ice_shldr_len_13cm_hist <- ice_on_clim[,,4] - ice_on_clim[,,1]
mean(ice_shldr_len_13cm_hist, na.rm = T)

#13 cm at 1 deg warming
ice_shldr_len_13cm1 <- ice_on_clim_1deg[,,4] - ice_on_clim_1deg[,,1]
mean(ice_shldr_len_13cm1, na.rm = T)

#13 cm at 2 deg warming
ice_shldr_len_13cm2 <- ice_on_clim_2deg[,,4] - ice_on_clim_2deg[,,1]
mean(ice_shldr_len_13cm2, na.rm = T)

#13 cm at 4 deg warming
ice_shldr_len_13cm4 <- ice_on_clim_4deg[,,4] - ice_on_clim_4deg[,,1]
mean(ice_shldr_len_13cm4, na.rm = T)

#20 cm at historical
ice_shldr_len_20cm_hist <- ice_on_clim[,,5] - ice_on_clim[,,1]
mean(ice_shldr_len_20cm_hist, na.rm = T)

#20 cm at 1 deg warming
ice_shldr_len_20cm1 <- ice_on_clim_1deg[,,5] - ice_on_clim_1deg[,,1]
mean(ice_shldr_len_20cm1, na.rm = T)

#20 cm at 2 deg warming
ice_shldr_len_20cm2 <- ice_on_clim_2deg[,,5] - ice_on_clim_2deg[,,1]
mean(ice_shldr_len_20cm2, na.rm = T)

#20 cm at 4 deg warming
ice_shldr_len_20cm4 <- ice_on_clim_4deg[,,5] - ice_on_clim_4deg[,,1]
mean(ice_shldr_len_20cm4, na.rm = T)

#Ice off

#13 cm at historical
ice_off_shldr_len_13cm_hist <-  ice_off_clim[,,1] - ice_off_clim[,,4]
mean(ice_off_shldr_len_13cm_hist, na.rm = T)

#13 cm at 1 deg warming
ice_off_shldr_len_13cm1 <-  ice_off_clim_1deg[,,1] - ice_off_clim_1deg[,,4]
mean(ice_off_shldr_len_13cm1, na.rm = T)

#13 cm at 2 deg warming
ice_off_shldr_len_13cm2 <-  ice_off_clim_2deg[,,1] - ice_off_clim_2deg[,,4]
mean(ice_off_shldr_len_13cm2, na.rm = T)

#13 cm at 4 deg warming
ice_off_shldr_len_13cm4 <-  ice_off_clim_4deg[,,1] - ice_off_clim_4deg[,,4]
mean(ice_off_shldr_len_13cm4, na.rm = T)

#20 cm at historical
ice_off_shldr_len_20cm_hist <- ice_off_clim[,,1] - ice_off_clim[,,5]
mean(ice_off_shldr_len_20cm_hist, na.rm = T)

#20 cm at 1 deg warming
ice_off_shldr_len_20cm1 <-  ice_off_clim_1deg[,,1] - ice_off_clim_1deg[,,5]
mean(ice_off_shldr_len_20cm1, na.rm = T)

#20 cm at 2 deg warming
ice_off_shldr_len_20cm2 <-  ice_off_clim_2deg[,,1] - ice_off_clim_2deg[,,5]
mean(ice_off_shldr_len_20cm2, na.rm = T)

#20 cm at 4 deg warming
ice_off_shldr_len_20cm4 <-  ice_off_clim_4deg[,,1] - ice_off_clim_4deg[,,5]
mean(ice_off_shldr_len_20cm4, na.rm = T)

#Ice on anomaly 13cm
#13 cm at 1 deg warming
ice_shldr_anom_len_13cm1 <- ice_on_clim_1deg[,,4] - ice_on_clim[,,4]
mean(ice_shldr_anom_len_13cm1, na.rm = T)

#13 cm at 2 deg warming
ice_shldr_anom_len_13cm2 <- ice_on_clim_2deg[,,4] - ice_on_clim[,,4]
mean(ice_shldr_anom_len_13cm2, na.rm = T)

#13 cm at 4 deg warming
ice_shldr_anom_len_13cm4 <- ice_on_clim_4deg[,,4] - ice_on_clim[,,4]
mean(ice_shldr_anom_len_13cm4, na.rm = T)

#Ice on anomaly 20cm
#20 cm at 1 deg warming
ice_shldr_anom_len_20cm1 <- ice_on_clim_1deg[,,5] - ice_on_clim[,,5]
mean(ice_shldr_anom_len_20cm1, na.rm = T)

#20 cm at 2 deg warming
ice_shldr_anom_len_20cm2 <- ice_on_clim_2deg[,,5] - ice_on_clim[,,5]
mean(ice_shldr_anom_len_20cm2, na.rm = T)

#20 cm at 4 deg warming
ice_shldr_anom_len_20cm4 <- ice_on_clim_4deg[,,5] - ice_on_clim[,,5]
mean(ice_shldr_anom_len_20cm4, na.rm = T)

#Ice off anomaly 13cm
#13 cm at 1 deg warming
ice_off_shldr_anom_len_13cm1 <- ice_off_clim[,,4] - ice_off_clim_1deg[,,4]
mean(ice_off_shldr_anom_len_13cm1, na.rm = T)

#13 cm at 2 deg warming
ice_off_shldr_anom_len_13cm2 <- ice_off_clim[,,4] - ice_off_clim_2deg[,,4] 
mean(ice_off_shldr_anom_len_13cm2, na.rm = T)

#13 cm at 4 deg warming
ice_off_shldr_anom_len_13cm4 <- ice_off_clim[,,4] - ice_off_clim_4deg[,,4] 
mean(ice_off_shldr_anom_len_13cm4, na.rm = T)

#Ice off anomaly 20cm
#20 cm at 1 deg warming
ice_off_shldr_anom_len_20cm1 <- ice_off_clim[,,5] - ice_off_clim_1deg[,,5]
mean(ice_off_shldr_anom_len_20cm1, na.rm = T)

#20 cm at 2 deg warming
ice_off_shldr_anom_len_20cm2 <- ice_off_clim[,,5] - ice_off_clim_2deg[,,5]
mean(ice_off_shldr_anom_len_20cm2, na.rm = T)

#20 cm at 4 deg warming
ice_off_shldr_anom_len_20cm4 <- ice_off_clim[,,5] - ice_off_clim_4deg[,,5]
mean(ice_off_shldr_anom_len_20cm4, na.rm = T)
