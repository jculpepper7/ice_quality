library(fields)
library(tidyverse)
library(ncdf4)
library(here)
library(patchwork)
library(ggpubr)

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


# 2. Generate historic average --------------------------------------------

# * 2.1 define historic period----
idx_base <- itime_id >= 1851 & itime_id <= 1880

# * 2.2 Ice on historic period----
#Get the mean of the historic period
ice_on_clim <- apply(ice_on_nh[,,idx_base,], c(1,2,4), mean, na.rm = TRUE)
ice_on_clim2 <- replicate(length(itime_id), ice_on_clim)

#Must permute the data array, because it is not in the same 
#order as the ice_on_nh data
ice_on_clim3 <- aperm(ice_on_clim2, c(1,2,4,3)) 

# * 2.3 Ice off historic period----
#Get the mean of the historic period
ice_off_clim <- apply(ice_off_nh[,,idx_base,], c(1,2,4), mean, na.rm = TRUE)
ice_off_clim2 <- replicate(length(itime_id), ice_off_clim)

#Must permute the data array, because it is not in the same 
#order as the ice_on_nh data
ice_off_clim3 <- aperm(ice_off_clim2, c(1,2,4,3)) 

# 3. Establish anomalies from historic data----

#Subtract from the original dataset to get the anomaly

# Ice on
ice_on_anom <- ice_on_nh - ice_on_clim3 

# Ice off
ice_off_anom <- ice_off_nh - ice_off_clim3 

#Did this work? Look at the ice on time series
on_anom_test <- apply(ice_on_anom[,,,threshold == 12], 3, mean, na.rm = T)
off_anom_test <- apply(ice_off_anom[,,,threshold == 12], 3, mean, na.rm = T)

plot(on_anom_test, type = 'l')
plot(off_anom_test, type = 'l')

#Look at an anomaly map
#image.plot(ice_on_anom[,, itime_id == 2088,threshold == 12])

# 4. Create Historic maps----


# * 4.1 Historic maps for ice on  ---------------------------------------------



#Ice on for 10 cm
ice_on_hist4 <- mapCDFtemp(lat_nh, lon_nh, ice_on_clim[,,threshold == 4], 
                          name_cmocean = 'matter', 
                          direction = 1, 
                          #color_breaks()
                          limits = c(min(ice_on_clim[,,threshold == 4], na.rm = T), max(ice_on_clim[,,threshold == 4], na.rm = T)),
                          grid_size = 2,
                          titletext = '',
                          plot_margin = unit(c(0,0,0,0),"cm"))
#ice_on_hist4

#ggsave(here('results/final_plots/historical_maps/ice_on_hist4.png'), dpi = 300, width = 20, height = 20, units = 'cm')


#Ice on for 13 cm
ice_on_hist5 <- mapCDFtemp(lat_nh, lon_nh, ice_on_clim[,,threshold == 5], 
                          name_cmocean = 'matter', 
                          direction = 1, 
                          limits = c(min(ice_on_clim[,,threshold == 4], na.rm = T), max(ice_on_clim[,,threshold == 4], na.rm = T)),
                          grid_size = 2,
                          titletext = '',
                          plot_margin = unit(c(-1,-1,-1,1),"cm"))
#ice_on_hist5

#Ice on for 20 cm
ice_on_hist8 <- mapCDFtemp(lat_nh, lon_nh, ice_on_clim[,,threshold == 8], 
                           name_cmocean = 'matter', 
                           direction = 1, 
                           limits = c(min(ice_on_clim[,,threshold == 4], na.rm = T), max(ice_on_clim[,,threshold == 4], na.rm = T)),
                           grid_size = 2,
                           titletext = '',
                           plot_margin = unit(c(0,0,0,0),"cm"))
#ice_on_hist8

#ggsave(here('results/final_plots/historical_maps/ice_on_hist8.png'), dpi = 300, width = 20, height = 20, units = 'cm')


#Ice on for 30 cm
ice_on_hist12 <- mapCDFtemp(lat_nh, lon_nh, ice_on_clim[,,threshold == 12], 
                           name_cmocean = 'matter', 
                           direction = 1, 
                           limits = c(min(ice_on_clim[,,threshold == 4], na.rm = T), max(ice_on_clim[,,threshold == 4], na.rm = T)),
                           grid_size = 2,
                           titletext = '',
                           plot_margin = unit(c(-1,-1,-1,-5),"cm"))
#ice_on_hist12

#Extract desired legend (will put plots together in inkscape)
#iceon_legend <- as_ggplot(get_legend(ice_on_hist4))
#ggsave(here('results/final_plots/historical_maps/iceon_legend.png'), dpi = 300)

# * 4.2 Historic maps for ice off  ---------------------------------------------

#Ice off for 10 cm
ice_off_hist4 <- mapCDFtemp(lat_nh, lon_nh, ice_off_clim[,,threshold == 4], 
                           name_cmocean = 'matter', 
                           direction = 1, 
                           limits = c(min(ice_off_clim[,,threshold == 4], na.rm = T), max(ice_off_clim[,,threshold == 4], na.rm = T)),
                           grid_size = 2,
                           titletext = '',
                           plot_margin = unit(c(0,0,0,0),"cm"))
#ice_off_hist4

#ggsave(here('results/final_plots/historical_maps/ice_off_hist4.png'), dpi = 300, width = 20, height = 20, units = 'cm')


#Ice off for 13 cm
ice_off_hist5 <- mapCDFtemp(lat_nh, lon_nh, ice_off_clim[,,threshold == 5], 
                           name_cmocean = 'matter', 
                           direction = 1, 
                           limits = c(min(ice_off_clim[,,threshold == 4], na.rm = T), max(ice_off_clim[,,threshold == 4], na.rm = T)),
                           grid_size = 2,
                           titletext = '',
                           plot_margin = unit(c(-1,-1,-1,-5),"cm"))
#ice_off_hist5

#Ice off for 20 cm
ice_off_hist8 <- mapCDFtemp(lat_nh, lon_nh, ice_off_clim[,,threshold == 8], 
                            name_cmocean = 'matter', 
                            direction = 1, 
                            limits = c(min(ice_off_clim[,,threshold == 4], na.rm = T), max(ice_off_clim[,,threshold == 4], na.rm = T)),
                            grid_size = 2,
                            titletext = '',
                            plot_margin = unit(c(0,0,0,0),"cm"))
#ice_off_hist8

#ggsave(here('results/final_plots/historical_maps/ice_off_hist8.png'), dpi = 300, width = 20, height = 20, units = 'cm')

#Ice off for 30 cm
ice_off_hist12 <- mapCDFtemp(lat_nh, lon_nh, ice_off_clim[,,threshold == 12], 
                            name_cmocean = 'matter', 
                            direction = 1, 
                            limits = c(min(ice_off_clim[,,threshold == 4], na.rm = T), max(ice_off_clim[,,threshold == 4], na.rm = T)),
                            grid_size = 2.5,
                            titletext = '',
                            plot_margin = unit(c(-1,-1,-1,-5),"cm"))
#ice_off_hist12

#iceoff_legend <- as_ggplot(get_legend(ice_off_hist4))
#ggsave(here('results/final_plots/historical_maps/iceoff_legend.png'), dpi = 300)












# Arrange plots
#The below arrangements look terrible, so I combined them in Inkscape
# hist_maps <- ggarrange(ice_on_hist4, ice_on_hist8, 
#                        ice_off_hist4, ice_off_hist8, 
#                        ncol=2, nrow=2, common.legend = TRUE, legend="right")
# hist_maps
# 
# patch_plt <- (ice_on_hist4 + ice_on_hist8)/ (ice_off_hist4 + ice_off_hist8)+
#   plot_layout(guides = 'collect')
# patch_plt 

# 5. Create anomaly maps --------------------------------------------------

# **Ice on----

#Ice on 1 degree warming - 10cm thickness
ice_on_anom1_10cm <- mapCDFtemp(lat_nh, lon_nh, ice_on_anom[,, itime_id == 2010,threshold == 4], 
                             name_cmocean = 'matter', 
                             direction = -1, 
                             limits = c(0, max(ice_on_anom[,, itime_id == 2088, threshold == 12], na.rm = T)),
                             grid_size = 2.5,
                             titletext = '',
                             plot_margin = unit(c(0,0,0,0),"cm"))
ice_on_anom1_10cm

#ggsave(here('results/final_plots/anomaly_maps/on_anom1_10cm.png'), dpi = 300)

#Ice on 2 degree warming - 10cm thickness
ice_on_anom2_10cm <- mapCDFtemp(lat_nh, lon_nh, ice_on_anom[,, itime_id == 2042,threshold == 4], 
                                name_cmocean = 'matter', 
                                direction = -1, 
                                limits = c(0, max(ice_on_anom[,, itime_id == 2088, threshold == 12], na.rm = T)),
                                grid_size = 2.5,
                                titletext = '',
                                plot_margin = unit(c(0,0,0,0),"cm"))
#ice_on_anom2_10cm

#ggsave(here('results/final_plots/anomaly_maps/on_anom2_10cm.png'), dpi = 300)

#Ice on 4 degree warming - 10cm thickness
ice_on_anom4_10cm <- mapCDFtemp(lat_nh, lon_nh, ice_on_anom[,, itime_id == 2088,threshold == 4], 
                                name_cmocean = 'matter', 
                                direction = -1, 
                                limits = c(0, max(ice_on_anom[,, itime_id == 2088, threshold == 12], na.rm = T)),
                                grid_size = 2.5,
                                titletext = '',
                                plot_margin = unit(c(0,0,0,0),"cm"))
#ice_on_anom4_10cm

#ggsave(here('results/final_plots/anomaly_maps/on_anom4_10cm.png'), dpi = 300)

#Ice on 1 degree warming - 13cm thickness
ice_on_anom1_13cm <- mapCDFtemp(lat_nh, lon_nh, ice_on_anom[,, itime_id == 2010,threshold == 5], 
                                name_cmocean = 'matter', 
                                direction = -1, 
                                limits = c(0, max(ice_on_anom[,, itime_id == 2088, threshold == 12], na.rm = T)),
                                grid_size = 2.5,
                                titletext = '',
                                plot_margin = unit(c(0,0,0,0),"cm"))
#ice_on_anom1_13cm

#ggsave(here('results/final_plots/anomaly_maps/on_anom1_13cm.png'), dpi = 300)

#Ice on 2 degree warming - 10cm thickness
ice_on_anom2_13cm <- mapCDFtemp(lat_nh, lon_nh, ice_on_anom[,, itime_id == 2042,threshold == 5], 
                                name_cmocean = 'matter', 
                                direction = -1, 
                                limits = c(0, max(ice_on_anom[,, itime_id == 2088, threshold == 12], na.rm = T)),
                                grid_size = 2.5,
                                titletext = '',
                                plot_margin = unit(c(0,0,0,0),"cm"))
#ice_on_anom2_13cm

#ggsave(here('results/final_plots/anomaly_maps/on_anom2_13cm.png'), dpi = 300)

#Ice on 4 degree warming - 10cm thickness
ice_on_anom4_13cm <- mapCDFtemp(lat_nh, lon_nh, ice_on_anom[,, itime_id == 2088,threshold == 5], 
                                name_cmocean = 'matter', 
                                direction = -1, 
                                limits = c(0, max(ice_on_anom[,, itime_id == 2088, threshold == 12], na.rm = T)),
                                grid_size = 2.5,
                                titletext = '',
                                plot_margin = unit(c(0,0,0,0),"cm"))
#ice_on_anom4_13cm

#ggsave(here('results/final_plots/anomaly_maps/on_anom4_13cm.png'), dpi = 300)

#Ice on 1 degree warming - 20cm thickness
ice_on_anom1_20cm <- mapCDFtemp(lat_nh, lon_nh, ice_on_anom[,, itime_id == 2010,threshold == 8], 
                                name_cmocean = 'matter', 
                                direction = -1, 
                                limits = c(0, max(ice_on_anom[,, itime_id == 2088, threshold == 12], na.rm = T)),
                                grid_size = 2.5,
                                titletext = '',
                                plot_margin = unit(c(0,0,0,0),"cm"))
#ice_on_anom1_20cm

#ggsave(here('results/final_plots/anomaly_maps/on_anom1_20cm.png'), dpi = 300)

#Ice on 2 degree warming - 20cm thickness
ice_on_anom2_20cm <- mapCDFtemp(lat_nh, lon_nh, ice_on_anom[,, itime_id == 2042,threshold == 8], 
                                name_cmocean = 'matter', 
                                direction = -1, 
                                limits = c(0, max(ice_on_anom[,, itime_id == 2088, threshold == 12], na.rm = T)),
                                grid_size = 2.5,
                                titletext = '',
                                plot_margin = unit(c(0,0,0,0),"cm"))
#ice_on_anom2_20cm

#ggsave(here('results/final_plots/anomaly_maps/on_anom2_20cm.png'), dpi = 300)

#Ice on 4 degree warming - 20cm thickness
ice_on_anom4_20cm <- mapCDFtemp(lat_nh, lon_nh, ice_on_anom[,, itime_id == 2088,threshold == 8], 
                                name_cmocean = 'matter', 
                                direction = -1, 
                                limits = c(0, max(ice_on_anom[,, itime_id == 2088, threshold == 12], na.rm = T)),
                                grid_size = 2.5,
                                titletext = '',
                                plot_margin = unit(c(0,0,0,0),"cm"))
#ice_on_anom4_20cm

#ggsave(here('results/final_plots/anomaly_maps/on_anom4_20cm.png'), dpi = 300)

#Ice on 1 degree warming - 13cm thickness
ice_on_anom1_30cm <- mapCDFtemp(lat_nh, lon_nh, ice_on_anom[,, itime_id == 2010,threshold == 12], 
                                name_cmocean = 'matter', 
                                direction = -1, 
                                limits = c(0, max(ice_on_anom[,, itime_id == 2088, threshold == 12], na.rm = T)),
                                grid_size = 2.5,
                                titletext = '',
                                plot_margin = unit(c(0,0,0,0),"cm"))
#ice_on_anom1_30cm

#ggsave(here('results/final_plots/anomaly_maps/on_anom1_30cm.png'), dpi = 300)

#Ice on 2 degree warming - 10cm thickness
ice_on_anom2_30cm <- mapCDFtemp(lat_nh, lon_nh, ice_on_anom[,, itime_id == 2042,threshold == 12], 
                                name_cmocean = 'matter', 
                                direction = -1, 
                                limits = c(0, max(ice_on_anom[,, itime_id == 2088, threshold == 12], na.rm = T)),
                                grid_size = 2.5,
                                titletext = '',
                                plot_margin = unit(c(0,0,0,0),"cm"))
#ice_on_anom2_30cm

#ggsave(here('results/final_plots/anomaly_maps/on_anom2_30cm.png'), dpi = 300)

#Ice on 4 degree warming - 10cm thickness
ice_on_anom4_30cm <- mapCDFtemp(lat_nh, lon_nh, ice_on_anom[,, itime_id == 2088,threshold == 12], 
                                name_cmocean = 'matter', 
                                direction = -1, 
                                limits = c(0, max(ice_on_anom[,, itime_id == 2088, threshold == 12], na.rm = T)),
                                grid_size = 2.5,
                                titletext = '',
                                plot_margin = unit(c(0,0,0,0),"cm"))
#ice_on_anom4_30cm

#ggsave(here('results/final_plots/anomaly_maps/on_anom4_30cm.png'), dpi = 300)

#########################################################
iceon_anom_legend <- as_ggplot(get_legend(ice_on_anom4_30cm))
#ggsave(here('results/final_plots/anomaly_maps/on_anom_generalSafety_legend.png'), dpi = 300)

# min(ice_on_anom[,, itime_id == 2088, threshold == 12], na.rm = T)
# max(ice_on_anom[,, itime_id == 2088, threshold == 12], na.rm = T)
#sum(ice_on_anom[,, itime_id == 2088, threshold == 4] < 0, na.rm = T)

#4inch -> c(-17, 41)
#5inch -> c(-17, 44)
#8inch -> c(-26, 57)
#12inch -> c(-30, 80)

# min(ice_on_anom[,, itime_id == 2088, threshold == 12], na.rm = T)
# max(ice_on_anom[,, itime_id == 2088, threshold == 12], na.rm = T)
#sum(ice_on_anom[,, itime_id == 2088, threshold == 4] < 0, na.rm = T)

#4inch -> c(-17, 41)
#5inch -> c(-17, 44)
#8inch -> c(-26, 57)
#12inch -> c(-30, 80)

# 6. Create shoulder season maps-------------------------------------------

# **Ice on----

shldr_10cm <- ice_on_clim[,, threshold == 4] - ice_on_clim[,, threshold == 0]
shldr_20cm <- ice_on_clim[,, threshold == 8] - ice_on_clim[,, threshold == 0]

#Ice on 1 degree warming - 10cm thickness
ice_on_shldr_10cm <- mapCDFtemp(lat_nh, lon_nh, shldr_10cm, 
                                name_cmocean = 'matter', 
                                direction = -1, 
                                limits = c(-5, 50), #max(shldr_10cm, na.rm = T))
                                color_breaks = seq(0,50,5),
                                grid_size = 2.5,
                                titletext = 'Shoulder season on 10cm',
                                plot_margin = unit(c(0,0,0,0),"cm"))
ice_on_shldr_10cm

ggsave(here('results/final_plots/alt_fig1/on_shldr_10cm.png'), dpi = 300,
       width = 10, height = 10, units = 'in')

#Ice on 2 degree warming - 10cm thickness
ice_on_shldr_20cm <- mapCDFtemp(lat_nh, lon_nh, shldr_20cm, 
                                name_cmocean = 'matter', 
                                direction = -1, 
                                limits = c(-5, 50),
                                color_breaks = seq(0,50,5),
                                grid_size = 2.5,
                                titletext = 'Shoulder season on 20cm',
                                plot_margin = unit(c(0,0,0,0),"cm"))
ice_on_shldr_20cm

ggsave(here('results/final_plots/alt_fig1/on_shldr_20cm.png'), dpi = 300,
       width = 10, height = 10, units = 'in')

# **Ice off----

shldr_off_10cm <- ice_off_clim[,, threshold == 0] - ice_off_clim[,, threshold == 4]
shldr_off_20cm <- ice_off_clim[,, threshold == 0] - ice_off_clim[,, threshold == 8]

#Ice on 1 degree warming - 10cm thickness
ice_off_shldr_10cm <- mapCDFtemp(lat_nh, lon_nh, shldr_off_10cm, 
                                name_cmocean = 'matter', 
                                direction = -1, 
                                limits = c(0, 30), #max(shldr_10cm, na.rm = T))
                                color_breaks = seq(0,30,3),
                                grid_size = 2.5,
                                titletext = 'Shoulder season off 10cm',
                                plot_margin = unit(c(0,0,0,0),"cm"))
ice_off_shldr_10cm

ggsave(here('results/final_plots/alt_fig1/off_shldr_10cm.png'), dpi = 300,
       width = 10, height = 10, units = 'in')

#Ice on 2 degree warming - 10cm thickness
ice_off_shldr_20cm <- mapCDFtemp(lat_nh, lon_nh, shldr_off_20cm, 
                                name_cmocean = 'matter', 
                                direction = -1, 
                                limits = c(0, 30),
                                color_breaks = seq(0,30,3),
                                grid_size = 2.5,
                                titletext = 'Shoulder season off 20cm',
                                plot_margin = unit(c(0,0,0,0),"cm"))
ice_off_shldr_20cm

ggsave(here('results/final_plots/alt_fig1/off_shldr_20cm.png'), dpi = 300,
       width = 10, height = 10, units = 'in')

# 7. Other visualizations -------------------------------------------------

#Recall how to get info
on_anom_test <- apply(ice_on_anom[,,,threshold == 12], c(1,2), mean, na.rm = T)
off_anom_test <- apply(ice_off_anom[,,,threshold == 12], 3, mean, na.rm = T)


on_anom_test_viz <- apply(ice_on_anom[,,,threshold == 12], c(1,2), mean, na.rm = T) 

#*Ice on violin plot----

on_anom <- expand.grid(lon_nh, lat_nh) %>%
  rename(lon_nh = Var1, lat_nh = Var2) %>%
  mutate(
    lon_nh = ifelse(lon_nh > 180, -(360 - lon_nh), lon_nh),
    on_anom1_10cm = as.vector(apply(ice_on_anom[,,itime_id == 2010,threshold == 4], c(1,2), mean, na.rm = T)),
    on_anom1_13cm = as.vector(apply(ice_on_anom[,,itime_id == 2010,threshold == 5], c(1,2), mean, na.rm = T)),
    on_anom1_20cm = as.vector(apply(ice_on_anom[,,itime_id == 2010,threshold == 8], c(1,2), mean, na.rm = T)),
    on_anom1_30cm = as.vector(apply(ice_on_anom[,,itime_id == 2010,threshold == 12], c(1,2), mean, na.rm = T)),
    on_anom2_10cm = as.vector(apply(ice_on_anom[,,itime_id == 2042,threshold == 4], c(1,2), mean, na.rm = T)),
    on_anom2_13cm = as.vector(apply(ice_on_anom[,,itime_id == 2042,threshold == 5], c(1,2), mean, na.rm = T)),
    on_anom2_20cm = as.vector(apply(ice_on_anom[,,itime_id == 2042,threshold == 8], c(1,2), mean, na.rm = T)),
    on_anom2_30cm = as.vector(apply(ice_on_anom[,,itime_id == 2042,threshold == 12], c(1,2), mean, na.rm = T)),
    on_anom4_10cm = as.vector(apply(ice_on_anom[,,itime_id == 2088,threshold == 4], c(1,2), mean, na.rm = T)),
    on_anom4_13cm = as.vector(apply(ice_on_anom[,,itime_id == 2088,threshold == 5], c(1,2), mean, na.rm = T)),
    on_anom4_20cm = as.vector(apply(ice_on_anom[,,itime_id == 2088,threshold == 8], c(1,2), mean, na.rm = T)),
    on_anom4_30cm = as.vector(apply(ice_on_anom[,,itime_id == 2088,threshold == 12], c(1,2), mean, na.rm = T))
    ) %>% 
  na.omit() %>% 
  pivot_longer(cols = starts_with('on'),
               names_to = 'iceon_anomaly', 
               values_to = 'days') %>% 
  mutate(
    thickness = as.factor(rep(c('10 cm','13 cm','20 cm','30 cm'), length.out = 33036)),
    warming = as.factor(rep(c(1,1,1,1,2,2,2,2,4,4,4,4), length.out = 33036))
  )

on_anom_violin <- ggplot(data = on_anom)+
  # geom_violin(mapping = aes(x = factor(iceon_anomaly), 
  #                           y = days), draw_quantiles = 0.5, linewidth = 1.5, lineend = "round")+
  geom_violin(aes(x = thickness, y = days, fill = warming), draw_quantiles = 0.5, linewidth = 0.5)+
  #geom_boxplot(aes(x = thickness, y = days, color = warming), geom = 'errorbar', width = 0.3)+
  theme_classic()+
  xlab('')+
  ylab('Ice On Anomaly (days)')+
  #scale_fill_viridis_d(labels = c(expression('1°C'), '2°C', '4°C'), begin = 0.3, end = 0.8)+
  #scale_fill_grey(labels = c('1°C', '2°C', '4°C'), start = 0.4, end = 0.8)+
  scale_fill_manual(values=c("#FEEDB0FF", "#F5A773FF", "#B32E5FFF"),labels = c('1°C', '2°C', '4°C'))+
  theme(
    axis.text = element_text(size = 18),
    axis.title.y = element_text(size = 20),
    legend.title = element_text(size = 15),
    legend.position = c(0.1,0.8),
    legend.text = element_text(size = 12),
    axis.text.x = element_blank()
    )+
  guides(fill=guide_legend(title="Warming\nScenario"))

on_anom_violin


#ggsave(here('results/final_plots/violin_plt.png'), dpi = 700, width = 7, height = 4)  
  
#*Ice off violin plot----  
  
off_anom <- expand.grid(lon_nh, lat_nh) %>%
  rename(lon_nh = Var1, lat_nh = Var2) %>%
  mutate(
    lon_nh = ifelse(lon_nh > 180, -(360 - lon_nh), lon_nh),
    off_anom1_10cm = as.vector(apply(ice_off_anom[,,itime_id == 2010,threshold == 4], c(1,2), mean, na.rm = T)),
    off_anom1_13cm = as.vector(apply(ice_off_anom[,,itime_id == 2010,threshold == 5], c(1,2), mean, na.rm = T)),
    off_anom1_20cm = as.vector(apply(ice_off_anom[,,itime_id == 2010,threshold == 8], c(1,2), mean, na.rm = T)),
    off_anom1_30cm = as.vector(apply(ice_off_anom[,,itime_id == 2010,threshold == 12], c(1,2), mean, na.rm = T)),
    off_anom2_10cm = as.vector(apply(ice_off_anom[,,itime_id == 2042,threshold == 4], c(1,2), mean, na.rm = T)),
    off_anom2_13cm = as.vector(apply(ice_off_anom[,,itime_id == 2042,threshold == 5], c(1,2), mean, na.rm = T)),
    off_anom2_20cm = as.vector(apply(ice_off_anom[,,itime_id == 2042,threshold == 8], c(1,2), mean, na.rm = T)),
    off_anom2_30cm = as.vector(apply(ice_off_anom[,,itime_id == 2042,threshold == 12], c(1,2), mean, na.rm = T)),
    off_anom4_10cm = as.vector(apply(ice_off_anom[,,itime_id == 2088,threshold == 4], c(1,2), mean, na.rm = T)),
    off_anom4_13cm = as.vector(apply(ice_off_anom[,,itime_id == 2088,threshold == 5], c(1,2), mean, na.rm = T)),
    off_anom4_20cm = as.vector(apply(ice_off_anom[,,itime_id == 2088,threshold == 8], c(1,2), mean, na.rm = T)),
    off_anom4_30cm = as.vector(apply(ice_off_anom[,,itime_id == 2088,threshold == 12], c(1,2), mean, na.rm = T))
  ) %>% 
  na.omit() %>% 
  pivot_longer(cols = starts_with('off'),
               names_to = 'iceoff_anomaly', 
               values_to = 'days') %>% 
  mutate(
    thickness = as.factor(rep(c('10 cm','13 cm','20 cm','30 cm'), length.out = 33036)),
    warming = as.factor(rep(c(1,1,1,1,2,2,2,2,4,4,4,4), length.out = 33036))
  )

off_anom_violin <- ggplot(data = off_anom)+
  # geom_violin(mapping = aes(x = factor(iceon_anomaly), 
  #                           y = days), draw_quantiles = 0.5, linewidth = 1.5, lineend = "round")+
  geom_violin(aes(x = thickness, y = days, fill = warming), draw_quantiles = 0.5, linewidth = 0.5)+
  #geom_boxplot(aes(x = thickness, y = days, color = warming), geom = 'errorbar', width = 0.3)+
  theme_classic()+
  xlab('')+
  ylab('Ice Off Anomaly (days)')+
  #scale_fill_viridis_d(labels = c(expression('1°C'), '2°C', '4°C'), begin = 0.3, end = 0.8)+
  #scale_fill_grey(labels = c('1°C', '2°C', '4°C'), start = 0.4, end = 0.8)+
  scale_fill_manual(values=c("#FEEDB0FF", "#F5A773FF", "#B32E5FFF"),labels = c('1°C', '2°C', '4°C'))+
  theme(
    axis.text = element_text(size = 18),
    axis.title.y = element_text(size = 20),
    legend.title = element_text(size = 15),
    legend.position = 'none',
    legend.text = element_text(size = 12)
  )+
  guides(fill=guide_legend(title="Warming\nScenario"))  

ggarrange(on_anom_violin, off_anom_violin,
          labels = c("a", "b"),
          hjust = -10,
          ncol = 1, nrow = 2)
  
#ggsave(here('results/final_plots/violin_plt_onoff_viridis.png'), dpi = 700, width = 12, height = 10)  
#ggsave(here('results/final_plots/violin_plt_onoff_greyscale.png'), dpi = 700, width = 12, height = 10)  
ggsave(here('results/final_plots/violin_plt_onoff_manualScale.png'), dpi = 700, width = 12, height = 10)  

##

























on_anom_lat <- ggplot(data = on_anom)+
  # geom_violin(mapping = aes(x = factor(iceon_anomaly), 
  #                           y = days), draw_quantiles = 0.5, linewidth = 1.5, lineend = "round")+
  geom_point(aes(x = lat_nh, y = days, color = warming))+
  #geom_boxplot(aes(x = thickness, y = days, color = warming), geom = 'errorbar', width = 0.3)+
  theme_classic()+
  xlab('')+
  ylab('Ice On Anomaly (days)')+
  scale_fill_viridis_d(labels = c(expression('1°C'), '2°C', '4°C'), begin = 0.3, end = 0.8)+
  #scale_fill_grey(labels = c('1°C', '2°C', '4°C'), start = 0.4, end = 0.8)+
  theme(
    axis.text = element_text(size = 18),
    axis.title.y = element_text(size = 20),
    legend.title = element_text(size = 15),
    #legend.position = c(0.1,0.8),
    legend.position = 'right',
    legend.text = element_text(size = 12),
    axis.text.x = element_blank()
  )+
  guides(fill=guide_legend(title="Warming\nScenario"))
