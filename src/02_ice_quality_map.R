library(fields)
library(tidyverse)
library(ncdf4)
library(here)
library(patchwork)
library(ggpubr)
library(svglite)
#library(plotly)
library(cmocean)


# 1. Get data -------------------------------------------------------------


ice_thick_data <- nc_open(here('data/iceANNmean_mod_ensmean.nc'))

#latitude and longitude
ilat_id <- ncvar_get(ice_thick_data, 'lat')
ilon_id <- ncvar_get(ice_thick_data, 'lon')

#Time variable (year)
itime_id <- ncvar_get(ice_thick_data, 'year') #between 1850 and 2099

#ice on and off data for calculations and mapping
ice_thk <- ncvar_get(ice_thick_data, 'LAKEICETHICK')

image.plot(ice_thk[,,1])

#Reduce the boundaries of the data to just the Northern Hemisphere
idx_lat <- ilat_id >= 20 & ilat_id <= 90
idx_lon <- ilon_id >= 0 & ilon_id <= 360

lat_nh <- ilat_id[idx_lat]
lon_nh <- ilon_id[idx_lon]

ice_thk_nh <- ice_thk[idx_lon, idx_lat,]


# 2. Generate historic average --------------------------------------------

# * 2.1 define historic period----
idx_base <- itime_id >= 1851 & itime_id <= 1880

# * 2.2 Ice on historic period----
#Get the mean of the historic period
ice_thk_clim <- apply(ice_thk_nh[,,idx_base], c(1,2), mean, na.rm = TRUE)
ice_thk_clim2 <- replicate(length(itime_id), ice_thk_clim)

# 3. Establish anomalies from historic data -------------------------------

#Subtract from the original dataset to get the anomaly

# Ice thickness
ice_thk_anom <- ice_thk_nh - ice_thk_clim2 

#Did this work? Look at a basic ice thickness map and time series
image.plot(ice_thk_anom[,,itime_id ==2088])

#Create time series 1
thk_anom_test <- apply(ice_thk_anom, 3, mean, na.rm = T)

#thickness anomaly at different times
#thickness at 1C (contemporary anomaly)
thk_anom_test[161] #-0.07m (7cm)
#thickness at 2C (warming by 2040s)
thk_anom_test[193] # -0.12m (12cm)
#thickness at 4C (warming by end of century)
thk_anom_test[239] # -0.21m (21cm)

#time series plot
#plot(thk_anom_test, type = 'l')


# 4. Map of ice thickness at 1C warming -----------------------------------

# 4a. Need to decompose the plot and add Gesa's data----
map_data <- expand.grid(lon_nh, lat_nh) %>%
  rename(lon = Var1, lat = Var2) %>%
  mutate(lon = ifelse(lon > 180, -(360 - lon), lon),
         idat = as.vector(ice_thk_anom[,,itime_id == 2010]))
  
  #Start plot, feeding in the previous dataframe from expand.grid
ice_thick_1C_map  <- ggplot()+
  
  #geom_point(data = map_data, aes(x = lon, y = lat, color = idat), size = 2.5, shape = 'square')+
  geom_tile(data = ice_thk_anom[,,itime_id == 2010], aes(x = lon, y = lat, fill = idat))+
  # scale_color_viridis(name = 'Ice Duration Anomaly', 
  #                     na.value = 'transparent', 
  #                     option = 'A', 
  #                     limits = c(min(ice_thk_anom[,,itime_id == 2042], na.rm = T), max(ice_thk_anom[,, itime_id == 2042], na.rm = T)), 
  #                     direction = 1)+#,

  scale_color_cmocean(name='ice', direction = 1, limits = c(min(ice_thk_anom[,,itime_id == 2042], na.rm = T), max(ice_thk_anom[,, itime_id == 2042], na.rm = T)), na.value = 'transparent')+
  
  #Add the map layer
  borders('world', colour = 'grey15', fill = 'NA')+
  #borders('world', colour = 'NA', fill = 'NA')+ #try this out to get rid of the country borders?
  
  
  #Basic theme settings for background color, text size etc. 
  theme_bw()+
  theme(
    title = element_text(size = 30),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.text = element_text(size = 20),
    legend.title = element_blank(),
    legend.position = 'none',
    legend.key.width = unit(0.5, 'cm'),
    legend.key.height = unit(1.5, 'cm'),
    #plot.margin=unit(c(0,0,0,0),"cm"))
  )+
  
  #Map settings, adjusting the lat/long and projection
  coord_map(xlim = c(-170, 170), ylim = c(39, 75), projection = 'orthographic')+ 
  #geom_point(data = ice_clean_2, aes(x = Longitude, y = Latitude), size = 3, shape = 'circle')+
    
  #Title text etc.
  ggtitle('')+
  xlab('')+
  ylab('')
ice_thick_1C_map

#ggsave(here('results/ice_thk_map_1C.png'), dpi = 300, units = 'in', height = 10, width = 10 )
#ggsave(here('results/figure_3a.pdf'), dpi = 300, units = 'in', height = 10, width = 10 )



# **4a REVISED MAP --------------------------------------------------------

#Test other maps

mapCDF_off <- function(lat, lon, idat) 
{
  #Add a map object
  world_map <- map_data("world")
  
  #Create a df to plot from netCDF data
  expand.grid(lon, lat) %>%
    dplyr::rename(lon = Var1, lat = Var2) %>%
    mutate(lon = ifelse(lon > 180, -(360 - lon), lon),
           idat = as.vector(idat)) %>% 
    
    #Start plot, feeding in the previous df from expand.grid
    ggplot()+
    geom_polygon(
      data = world_map, 
      aes(x = long, y = lat, group = group), 
      fill = "grey50", 
      colour = "grey50", 
      #alpha = 0.8
    ) +
    geom_tile(
      aes(x = lon, y = lat, fill = idat, color = idat), 
      linewidth = 1
    )+
    scale_fill_gradientn(
      name = 'Ice thickness change (m)',
      na.value = 'transparent',
      #colors = c('#00008B', '#8D8DFF', '#FFFFFF', '#FF0C0C', '#F10000', '#D80000', '#BE0000', '#A50000', '#8B0000', '#720000'),
      #colors = c('#00008B', '#8D8DFF', '#FFFFFF', '#DE8787', '#D35F5F', '#B53131', '#8D2626', '#651b1b', '#3D1010', '#290B0B'),
      colors = c( '#3D1010', '#651B1B', '#8D2626', '#B53131', '#D35F5F', '#DE8787','#FFFFFF', '#8D8DFF', '#00008B' ),
      limits = c(-0.3, 0.1
                 #min(ice_thk_anom[,,itime_id == 2010], na.rm = TRUE),  #min is -0.239
                 #max(ice_thk_anom[,,itime_id == 2010], na.rm = TRUE)   #max is 0.065 
      ),
      breaks = c(-0.3, -0.25, -0.2, -0.15, -0.1, -0.05, 0, 0.05, 0.1),
      labels = format(c(-0.3, -0.25, -0.2, -0.15, -0.1, -0.05, 0, 0.05, 0.1)),
      guide = guide_colorsteps()
    )+
    scale_color_gradientn(
      guide = 'none',
      na.value = 'transparent',
      #colors = c('#00008B', '#8D8DFF', '#FFFFFF', '#DE8787', '#D35F5F', '#B53131', '#8D2626', '#651b1b', '#3D1010'),
      colors = c( '#3D1010', '#651B1B', '#8D2626', '#B53131', '#D35F5F', '#DE8787','#FFFFFF', '#8D8DFF', '#00008B' ),
      limits = c(-0.3, 0.1
                 # min(rw_ice_on_1c_anom, na.rm = TRUE),  #min is -12
                 # max(rw_ice_on_1c_wht_anom, na.rm = TRUE)   #max is 93
      ),
    )+
    
    #Add the map layer
    coord_map("ortho", orientation = c(90, 0, 0)) +
    
    # Removes Axes and labels
    scale_x_continuous(breaks = NULL)+
    
    # Adds axes
    geom_hline(aes(yintercept = 180), linewidth = 0.5)  +
    
    #Basic theme settings
    theme_void()+
    
    # Change theme to remove axes and ticks
    theme(panel.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.ticks=element_blank(),
          legend.position = 'bottom',
          legend.title.position = 'bottom',
          legend.key.width = unit(1.25, 'in'),
          legend.title = element_text(hjust = 0.5, size = 20),
          legend.text = element_text(size = 20)
    )+
    xlab("") + 
    ylab("") 
  
}

y <- mapCDF_off(lat_nh, lon_nh, ice_thk_anom[,,itime_id == 2010])
#y <- mapCDF_off(lat_nh, lon_nh, ice_thk_anom[,,itime_id == 2042])

y

cairo_pdf(here('results/revised_maps/figure_3a.pdf'))
#cairo_pdf(here('results/revised_maps/figure_3b.pdf'))

print(y)
dev.off()

# **ALT REVISED MAP -------------------------------------------------------


mapCDF_off <- function(lat, lon, idat) 
{
  #Add a map object
  world_map <- map_data("world")
  
  #Create a df to plot from netCDF data
  expand.grid(lon, lat) %>%
    dplyr::rename(lon = Var1, lat = Var2) %>%
    mutate(lon = ifelse(lon > 180, -(360 - lon), lon),
           idat = as.vector(idat)) %>% 
    
    #Start plot, feeding in the previous df from expand.grid
    ggplot()+
    geom_polygon(
      data = world_map, 
      aes(x = long, y = lat, group = group), 
      fill = "grey50", 
      colour = "grey50", 
      #alpha = 0.8
    ) +
    geom_tile(
      aes(x = lon, y = lat, fill = idat, color = idat), 
      linewidth = 1
    )+
    scale_fill_viridis_b(
      na.value = 'transparent',
      limits = c(-0.3, 0.1),
      name = 'Ice thickness change (m)',
      breaks = c(-0.3, -0.25, -0.2, -0.15, -0.1, -0.05, 0, 0.05, 0.1),
      labels = format(c(-0.3, -0.25, -0.2, -0.15, -0.1, -0.05, 0, 0.05, 0.1)),
      guide = guide_colorsteps()
    )+
    scale_color_viridis_b(
      na.value = 'transparent',
      limits = c(-0.3, 0.1),
      name = 'Ice thickness change (m)',
      breaks = c(-0.3, -0.25, -0.2, -0.15, -0.1, -0.05, 0, 0.05, 0.1),
      labels = format(c(-0.3, -0.25, -0.2, -0.15, -0.1, -0.05, 0, 0.05, 0.1)),
      guide = guide_colorsteps()
    )+
    
    #Add the map layer
    coord_map("ortho", orientation = c(90, 0, 0)) +
    
    # Removes Axes and labels
    scale_x_continuous(breaks = NULL)+
    
    # Adds axes
    geom_hline(aes(yintercept = 180), linewidth = 0.5)  +
    
    #Basic theme settings
    theme_void()+
    
    # Change theme to remove axes and ticks
    theme(panel.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.ticks=element_blank(),
          legend.position = 'bottom',
          legend.title.position = 'bottom',
          legend.key.width = unit(1.25, 'in'),
          legend.title = element_text(hjust = 0.5, size = 20),
          legend.text = element_text(size = 20)
    )+
    xlab("") + 
    ylab("") 
  
}

#y <- mapCDF_off(lat_nh, lon_nh, ice_thk_anom[,,itime_id == 2010])
y <- mapCDF_off(lat_nh, lon_nh, ice_thk_anom[,,itime_id == 2042])

y

#cairo_pdf(here('results/revised_maps/figure_3a_alt.pdf'))
cairo_pdf(here('results/revised_maps/figure_3b_alt.pdf'))

print(y)
dev.off()


# 4b. Do the same as above but for 2C thickness----

map_data2 <- expand.grid(lon_nh, lat_nh) %>%
  rename(lon = Var1, lat = Var2) %>%
  mutate(lon = ifelse(lon > 180, -(360 - lon), lon),
         idat = as.vector(ice_thk_anom[,,itime_id == 2042]))

#Start plot, feeding in the previous dataframe from expand.grid
ice_thick_2C_map  <- ggplot()+
  geom_point(data = map_data2, aes(x = lon, y = lat, color = idat), size = 2.5, shape = 'square')+
  # scale_color_viridis(name = 'Ice Duration Anomaly', 
  #                     na.value = 'transparent', 
  #                     option = 'A', 
  #                     limits = c(min(ice_thk_anom[,,itime_id == 2042], na.rm = T), max(ice_thk_anom[,, itime_id == 2042], na.rm = T)), 
  #                     direction = 1)+#,
  scale_color_cmocean(name = 'ice', 
                      direction = 1, 
                      limits = c(min(ice_thk_anom[,,itime_id == 2042], na.rm = T), max(ice_thk_anom[,, itime_id == 2042], na.rm = T)), 
                      na.value = 'transparent')+
  
  #Add the map layer
  borders('world', colour = 'grey15', fill = 'NA')+
  #borders('world', colour = 'NA', fill = 'NA')+ #try this out to get rid of the country borders?
  
  
  #Basic theme settings for background color, text size etc. 
  theme_bw()+
  theme(
    title = element_text(size = 30),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.text = element_text(size = 20),
    legend.title = element_blank(),
    legend.position = 'right',
    legend.key.width = unit(0.5, 'cm'),
    legend.key.height = unit(1.5, 'cm')#,
    #plot.margin=unit(c(0,0,0,0),"cm"))
  )+
  
  #Map settings, adjusting the lat/long and projection
  coord_map(xlim = c(-170, 170), ylim = c(39, 75), projection = 'orthographic')+ 
  #geom_point(data = ice_clean_2, aes(x = Longitude, y = Latitude), size = 3, shape = 'circle')+
  
  #Title text etc.
  ggtitle('')+
  xlab('')+
  ylab('')
ice_thick_2C_map

#ggsave(here('results/ice_thk_map_2C_w_leg.png'), dpi = 300, units = 'in', height = 10, width = 10 )
ggsave(here('results/figure_3b.pdf'), dpi = 300, units = 'in', height = 10, width = 10 )


# 5. Make a time series plot of future ice thickness ----------------------


#Create time series 1
thk_anom_ts <- as.data.frame(apply(ice_thk_anom, 3, mean, na.rm = T)) %>% 
  rename(ice_thk = 1) %>% 
  mutate(
    year = as.numeric(seq(1850, 2099, 1)),
    sd_pos = ice_thk + sd(ice_thk),
    sd_neg = ice_thk - sd(ice_thk)
  ) %>% 
  select(2,1,3,4)

ice_ts_plt <- ggplot(data = thk_anom_ts)+
  geom_line(aes(x = year, y = ice_thk), linewidth = 1.2)+
  geom_ribbon(aes(x = year, y = ice_thk, ymin = sd_neg, ymax = sd_pos), fill = 'grey50', alpha = .2) +
  geom_point(aes(x = 2010, y = ice_thk[161], color = '1°C'), shape = 1, size = 5, stroke = 4)+ #ice thickness anomaly at 1C Warming = -0.0727m or -7.27cm
  geom_point(aes(x = 2042, y = ice_thk[193], color = '2°C'), shape = 1, size = 5, stroke = 4)+ #ice thickness anomaly at 2C Warming = -0.121m or -12.1cm
  scale_color_manual('', values = c("#E69F00", "#D55E00"))+
  xlab('') + 
  ylab('Ice Thickness Anomaly (m)')+
  theme_classic() +
  theme(
    text = element_text(size = 40),
    legend.position = c(0.8, 0.8)
  )
ice_ts_plt

#ggsave(here('results/ice_thk_ts.png'), dpi = 300, units = 'in', height = 10, width = 11)
ggsave(here('results/figure_3c.pdf'), dpi = 300, units = 'in', height = 10, width = 11)


#ggplotly(ice_ts_plt)


thk_anom_ts$year[193]

thk_anom_sd <- as.data.frame(apply(ice_thk_anom, 3, mean, na.rm = T)) %>% 
  rename(ice_thk = 1) %>% 
  mutate(
    year = as.numeric(seq(1850, 2099, 1)),
    sd_pos = sd(ice_thk),
    #sd_neg = ice_thk - sd(ice_thk)
  ) #%>% 
  #select(2,1,3,4)


