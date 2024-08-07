# The objective of this script is to 
# 1. Read in ice quality data with geographic locations
# 2. Get max white ice thickness for the year of the ice blitz project for all lakes
# 3. Make a map of all lakes colored by their max white ice
# 4. Black circle around lakes with time series data? Something to designate it I think


# 1. Load Libraries -------------------------------------------------------

library(tidyverse)
library(rnaturalearth)
library(rnaturalearthdata)
#library(rnaturalearthhires)
library(here)
library(janitor)
library(sf)

# 2. Import data ----------------------------------------------------------

fin_white_ice <- read_csv(here('data/finnish_white_ice.csv'))

fin_total_ice <- read_csv(here('data/finnish_total_ice.csv'))

fin_geo_loc <- read_csv(here('data/finnish_standard.csv'))

ntl_white_ice <- read_csv(here('data/ntl_ice_quality.csv'))

ice_blitz <- read_csv(here('data/ice_quality_data.csv'))

hal <- read_csv(here('data/haliburton_field_data_2016_17.csv'))

rus <- read_csv(here('data/ice_quality_zdorovennova_2021.csv'))

# 3. Clean data -----------------------------------------------------------

#**Clean Finnish data----

#Clean total ice data
fin_tot_ice <- fin_total_ice %>% 
  separate(lake_id, c('site', 'I')) %>%  #removing leading 0 and trailing "_I" to aggregate with tibble "finnish_standard.csv" obtained from Aman Basu
  mutate(
    site = as.numeric(site),
    site = as.factor(site)
  ) %>% 
  select(
    -I
  ) 

#Clean white ice data
fin_wht_ice <- fin_white_ice %>% 
  separate(lake_id, c('site', 'I')) %>%  #removing leading 0 and trailing "_I" to aggregate with tibble "finnish_standard.csv" obtained from Aman Basu
  mutate(
    site = as.numeric(site),
    site = as.factor(site)
  ) %>% 
  select(
    -I
  ) 

#Extract date of max ice thickness
fin_ice <- fin_tot_ice %>% 
  full_join(fin_wht_ice) %>% 
  mutate(
    water_year = if_else(
      month(date)>=10, year+1, year
    )
  ) %>% 
  # filter(
  #   water_year == '2021'
  # ) %>%
  group_by(site, water_year) %>% 
  filter(
    white_cm == max(white_cm, na.rm = T)
  ) %>% 
  group_by(site, water_year) %>% 
  slice(1) %>% 
  mutate(
    white_ice_perc = round((white_cm / total_thickness)*100) #round to the nearest %
  )

#Retain only geo location data for Finnish lakes
fin_std <- fin_geo_loc %>% 
  arrange(site) %>% 
# ggplot(data = fin_std, aes(x = date, y = white_cm))+
#   geom_point()+
#   facet_wrap(~site)
  select(site, lat, lon) %>% 
  mutate(
    site = as.factor(site)
  ) %>% 
  group_by(site) %>% 
  slice(1) 

#aggregate Finnish data

fin_clean <- fin_ice %>% 
  full_join(fin_std) %>% 
  select(
    site,
    year,
    white_ice_perc,
    lat, lon
  ) %>% 
  group_by(site) %>% 
  mutate(
    count = n()
  ) %>% 
  select(-water_year)
  

#**Clean NTL data----

ntl_clean <- ntl_white_ice %>% 
  dplyr::select(
    lakeid,
    year4, daynum, sampledate,
    lat, lon,
    totice, whiteice
  ) %>% 
  mutate(
    site = as.factor(lakeid),
    sampledate = mdy(sampledate),
    year = if_else(month(sampledate)>=10, year4+1, year4), #water year, but calling year to fit with Finnish dataframe
  ) %>% 
  group_by(site, year, lat, lon) %>%
  mutate(
    white_ice_perc = round((whiteice/totice)*100)
  ) %>% 
  filter(
    white_ice_perc == max(white_ice_perc, na.rm = TRUE)
  ) %>% 
  # summarise(
  #   white_ice_mean = mean(whiteice, na.rm = TRUE),
  #   white_ice_max = max(whiteice, na.rm = TRUE)
  # ) %>%
  group_by(site) %>% 
  mutate(
    count = n()
  ) %>% 
  dplyr::select(
    site,
    year,
    white_ice_perc,
    lat, lon,
    count
  )
  
#**Clean Ice Blitz----

ib_clean <- ice_blitz %>% 
  select(
    site = 1,
    lat = 3,
    lon = 4,
    date = 5,
    tot_thick = 8,
    white_ice = 9
  ) %>% 
  mutate(
    site = as.factor(site),
    date = mdy(date),
    year = if_else(month(date)>=10, year(date)+1, year(date))
  ) %>% 
  group_by(site, year, lat, lon) %>% 
  mutate(
    white_ice_perc = round((white_ice/tot_thick)*100)
  ) %>% 
  filter(
    white_ice_perc == max(white_ice_perc, na.rm = TRUE)
  ) %>% 
  # summarise(
  #   white_ice_mean = mean(white_ice, na.rm = TRUE),
  #   white_ice_max = max(white_ice, na.rm = TRUE)
  # ) %>% 
  group_by(site) %>% 
  slice(1) %>% 
  mutate(
    count = n()
  ) %>%
  select(
    site,
    year,
    white_ice_perc,
    lat, lon,
    count
  )

#**Clean Haliburton----

hal_clean <- hal %>%
  clean_names() %>% 
  dplyr::select(
    1:3, 5, 7, 10, 11
  ) %>% 
  # mutate(
  #   site = if_else(site_id == 'm1' | site_id == 'm4' | site_id == 'm5' | site_id == 'm6', 'm', 'c')
  # ) %>% 
  mutate(
    site = as.factor(site_id),
    white_ice_perc = round((white/total)*100),
    lon = lon*-1
  ) %>% 
  dplyr::select(-site_id) %>% 
  group_by(site, year) %>% 
  filter(
    white_ice_perc == max(white_ice_perc, na.rm = TRUE)
  ) %>% 
  # summarise(
  #   white_ice_mean = mean(white, na.rm = TRUE),
  #   white_ice_max = max(white, na.rm = TRUE)
  # ) %>% 
  group_by(site, year) %>%
  slice(1) %>% 
  mutate(
    count = n()
  ) %>% 
  dplyr::select(
    site,
    year,
    white_ice_perc,
    lat, lon,
    count
  )

#**Clean Lake Vendyurskoe data----

rus_clean <- rus %>% 
  select(
    1, 4, 5, 8, 9
  ) %>% 
  mutate(
    site = as.factor('vendyurskoe'),
    date = mdy(date),
    year = year(date)
  ) %>% 
  group_by(site, year, lat, lon) %>%
  mutate(
    white_ice_perc = round((white_ice_avg_cm/total_ice_avg_cm)*100)
  ) %>% 
  group_by(site, year) %>% 
  filter(
    white_ice_perc == max(white_ice_perc, na.rm = TRUE)
  ) %>% 
  # summarise(
  #   white_ice_mean = mean(white_ice_avg_cm, na.rm = TRUE),
  #   white_ice_max = max(white_ice_avg_cm, na.rm = TRUE)
  # ) %>% 
  group_by(site) %>% 
  mutate(
    count = n()
  ) %>% 
  group_by(site, year) %>% 
  slice(1) %>% 
  select(
    site, 
    year, 
    white_ice_perc, 
    lat, lon, 
    count
  )

# 4. Combine the dataframes -----------------------------------------------

white_ice_clean <- ntl_clean %>% 
  bind_rows(fin_clean, ib_clean, hal_clean, rus_clean) %>% 
  mutate(
    sample_2021 = if_else(
      year != '2021', 0, 1
    )
  ) %>%
  filter(
    year <= 2021
  ) %>% 
  group_by(site) %>% 
  slice_tail() %>% 
  mutate(
    shape = as.factor(if_else(
      count == 1, 'Single Year',
      if_else(count > 1 & count < 10, '<10 Years', '>10 Years'
    ))
  )
  )


# 5. Make a polar coord map -----------------------------------------------



library(rgdal)                                                                                                      
library(raster)
library(ggplot2)

# Defines the x axes required
x_lines <- seq(-120,180, by = 60)

ggplot() +
  borders('world', colour = 'grey50', fill = 'grey', ylim = c(45, 90), alpha = 0.5)+
  
  #Add points when sampled from year 2021
  geom_point(data = white_ice_clean %>%
               filter(sample_2021 == 1),
             aes(x = lon, y = lat, fill = white_ice_perc, 
                 #size = count
                 ), shape = 21, stroke = 0.2) +
  
  #Add points from most recent year if not 2021
  geom_point(data = white_ice_clean %>% 
               filter(sample_2021 == 0), 
             aes(x = lon, y = lat, fill = white_ice_perc, 
                 #size = count
                 ), shape = 22, stroke = 0.2) +
  
  # scale_size_continuous(
  #   breaks = seq(1,3,1), 
  #   range = c(1,2),
  #   name = 'Years Sampled',
  #   labels = c('1 year', '<10 years', '>10 years')
  #   )+
  scale_fill_gradient(
    limits = c(0, 100),
    breaks = c(20, 40, 60, 80, 100),
    labels = format(c(20, 40, 60, 80, 100), nsmall = 1),
    #guide = guide_colorsteps(),
    #name = "Max. White Ice (%)"
    guide = 'none'
  )+
  
  # Convert to polar coordinates
  coord_map("ortho", orientation = c(90, 0, 0)) +
  scale_y_continuous(breaks = seq(20, 80, by = 10), labels = NULL) +
  
  # Removes Axes and labels
  scale_x_continuous(breaks = NULL) +
  xlab("") + 
  ylab("") +
  
  # Adds labels
  geom_text(aes(x = 180, y = seq(40, 80, by = 20), hjust = -0.2, label = paste0(seq(40, 80, by = 20), "°N")), size = 8) +
  #geom_text(aes(x = x_lines, y = 35, label = c("120°W", "60°W", "0°", "60°E", "120°E", "180°W"))) +
  
  # Adds axes
  geom_hline(aes(yintercept = 20), size = 0.5)  +
  #geom_segment(aes(y = 20, yend = 90, x = x_lines, xend = x_lines), linetype = "dashed", size = 0.5, alpha = 0.6) +
  
  # Change theme to remove axes and ticks
  theme(panel.background = element_blank(),
        panel.grid.major = element_line(size = 0.25, linetype = 'dashed',
                                        colour = "black"),
        axis.ticks=element_blank(),
        text = element_text(size = 30))
  
#ggsave(here('results/quality_map_2024.05.03.pdf'), dpi = 300, width = 10, height = 10, units = 'in')



# 6. Make a N. American Map -----------------------------------------------

world <- ne_countries(scale = 'medium', returnclass = 'sf')
lakes <- rnaturalearth::ne_download(scale = 'medium', 
                                    type = 'lakes', 
                                    category = 'physical') %>% 
  sf::st_as_sf(lakes)

theme_set(theme_bw())

#Map of NA
ggplot() +
  geom_sf(data = world)+
  geom_sf(data = lakes %>% filter(name == 'Lake Michigan'),
          mapping = aes(geometry = geometry),
          color = "black",
          fill = "lightblue")+ 
  geom_sf(data = lakes %>% filter(name == 'Lake Huron'),
          mapping = aes(geometry = geometry),
          color = "black",
          fill = "lightblue")+
  geom_sf(data = lakes %>% filter(name == 'Lake Erie'),
          mapping = aes(geometry = geometry),
          color = "black",
          fill = "lightblue")+
  geom_sf(data = lakes %>% filter(name == 'Lake Ontario'),
          mapping = aes(geometry = geometry),
          color = "black",
          fill = "lightblue")+
  geom_sf(data = lakes %>% filter(name == 'Lake Superior'),
          mapping = aes(geometry = geometry),
          color = "black",
          fill = "lightblue")+
  coord_sf(
    xlim = c(-115, -70), 
    ylim = c(35,55), 
    expand = T
    )+
  #Add points sampled in 2021
  geom_point(
    data = white_ice_clean %>% filter(sample_2021 == 1), 
    aes(x = lon, y = lat, fill = white_ice_perc, shape = shape), 
    size = 5, stroke = 0.2, alpha = 0.7
    )+
  #Add points sampled most recently (if not 2021)
  geom_point(
    data = white_ice_clean %>% filter(sample_2021 == 0), 
    aes(x = lon, y = lat, fill = white_ice_perc, shape = shape), 
    size = 5, stroke = 0.2, alpha = 0.7, color = 'white'
  )+
  #Adjust display settings
  scale_shape_manual(
    values = c('Single Year' = 21, '<10 Years' = 22, '>10 Years' = 24), 
    guide = 'none'
    )+
  scale_fill_gradient(
    limits = c(0, 100),
    breaks = c(0, 20, 40, 60, 80, 100),
    labels = format(c(0, 20, 40, 60, 80, 100)),
    guide = guide_colorsteps()
    )+
  xlab('')+
  ylab('')+
  theme(
    legend.position = 'bottom',
    legend.title = element_blank(),
    legend.key.width = unit(1, 'in'),
    panel.grid.major = element_line( size=0.1, linetype = 2),
    text = element_text(size = 20)
  )

#ggsave(here('results/NA_map_revised.pdf'), dpi = 300, width = 10, height = 9, units = 'in')

# 7. Map a European Map ---------------------------------------------------
  
#Map of Europe
ggplot() +
  geom_sf(data = world)+
  coord_sf(
    xlim = c(0, 35), 
    ylim = c(45,70), 
    expand = T
  )+
  #Add points sampled in 2021
  geom_point(
    data = white_ice_clean %>% filter(sample_2021 == 1), 
    aes(x = lon, y = lat, fill = white_ice_perc, shape = shape), 
    size = 5, stroke = 0.2, alpha = 0.7 #shape = 21,
  )+
  #Add points sampled most recently (if not 2021)
  geom_point(
    data = white_ice_clean %>% filter(sample_2021 == 0), 
    aes(x = lon, y = lat, fill = white_ice_perc, shape = shape), 
    size = 5, stroke = 0.2, alpha = 0.7, color = 'white' #shape = 21,
  )+
  scale_shape_manual(
    values = c('Single Year' = 21, '<10 Years' = 22, '>10 Years' = 24), 
    guide = 'none'
  )+
  scale_fill_gradient(
    limits = c(0, 100),
    breaks = c(0, 20, 40, 60, 80, 100),
    labels = format(c(0, 20, 40, 60, 80, 100)),
    guide = guide_colorsteps()
  )+
  xlab('')+
  ylab('')+
  theme(
    legend.position = 'bottom',
    legend.title = element_blank(),
    legend.key.width = unit(1, 'in'),
    panel.grid.major = element_line( size=0.1, linetype = 2),
    text = element_text(size = 20)
  )


#ggsave(here('results/Euro_map_2024.05.03.pdf'), dpi = 300, width = 10, height = 9, units = 'in')



# 8. Make NTL inset plot --------------------------------------------------

#Without Trout Bog Lake

ntl_clean2 <- ntl_white_ice %>% 
  dplyr::select(
    lakeid,
    year4, daynum, sampledate,
    lat, lon,
    totice, whiteice, blueice
  ) %>% 
  mutate(
    site = as.factor(lakeid),
    sampledate = mdy(sampledate),
    year = if_else(month(sampledate)>=10, year4+1, year4), #water year, but calling year to fit with Finnish dataframe
  ) %>% 
  group_by(site) %>% 
  mutate(
    count = n()
  ) %>% 
  filter(
    site != 'TB'
  ) %>% 
  group_by(year) %>% 
  mutate(
    #total ice 
    mean_totice = mean(totice, na.rm = TRUE),
    sd_totice_high = mean_totice + sd(totice, na.rm = TRUE),
    sd_totice_low = mean_totice - sd(totice, na.rm = TRUE),
    sd_totice_low = if_else(
      sd_totice_low < 0, 0, sd_totice_low
    ),
    max_totice = max(totice, na.rm = TRUE),
    min_totice = min(totice, na.rm = TRUE),
    #white ice
    mean_whiteice = mean(whiteice, na.rm = TRUE),
    sd_whiteice_high = mean_whiteice + sd(whiteice, na.rm = TRUE),
    sd_whiteice_low = mean_whiteice - sd(whiteice, na.rm = TRUE),
    sd_whiteice_low = if_else(
      sd_whiteice_low < 0, 0, sd_whiteice_low
    ),
    max_whiteice = max(whiteice, na.rm = TRUE),
    min_whiteice = min(whiteice, na.rm = TRUE),
    #"blue" ice (i.e., black ice)
    mean_blackice = mean(blueice, na.rm = TRUE),
    sd_blackice_high = mean_blackice + sd(blueice, na.rm = TRUE),
    sd_blackice_low = mean_blackice - sd(blueice, na.rm = TRUE),
    sd_blackice = sd(blueice, na.rm = TRUE),
    sd_blackice_low = if_else(
      sd_blackice_low < 0, 0, sd_blackice_low
    ),
    max_blackice = max(blueice, na.rm = TRUE),
    min_blackice = min(blueice, na.rm = TRUE)
  )

#With Trout Bog Lake

ntl_clean3 <- ntl_white_ice %>% 
  dplyr::select(
    lakeid,
    year4, daynum, sampledate,
    lat, lon,
    totice, whiteice, blueice
  ) %>% 
  mutate(
    site = as.factor(lakeid),
    sampledate = mdy(sampledate),
    year = if_else(month(sampledate)>=10, year4+1, year4), #water year, but calling year to fit with Finnish dataframe
  ) %>% 
  group_by(site) %>% 
  mutate(
    count = n()
  ) %>% 
  group_by(year) %>% 
  mutate(
    #total ice 
    mean_totice = mean(totice, na.rm = TRUE),
    sd_totice_high = mean_totice + sd(totice, na.rm = TRUE),
    sd_totice_low = mean_totice - sd(totice, na.rm = TRUE),
    sd_totice_low = if_else(
      sd_totice_low < 0, 0, sd_totice_low
    ),
    max_totice = max(totice, na.rm = TRUE),
    min_totice = min(totice, na.rm = TRUE),
    #white ice
    mean_whiteice = mean(whiteice, na.rm = TRUE),
    sd_whiteice_high = mean_whiteice + sd(whiteice, na.rm = TRUE),
    sd_whiteice_low = mean_whiteice - sd(whiteice, na.rm = TRUE),
    sd_whiteice_low = if_else(
      sd_whiteice_low < 0, 0, sd_whiteice_low
    ),
    max_whiteice = max(whiteice, na.rm = TRUE),
    min_whiteice = min(whiteice, na.rm = TRUE),
    #"blue" ice (i.e., black ice)
    mean_blackice = mean(blueice, na.rm = TRUE),
    sd_blackice_high = mean_blackice + sd(blueice, na.rm = TRUE),
    sd_blackice_low = mean_blackice - sd(blueice, na.rm = TRUE),
    sd_blackice = sd(blueice, na.rm = TRUE),
    sd_blackice_low = if_else(
      sd_blackice_low < 0, 0, sd_blackice_low
    ),
    max_blackice = max(blueice, na.rm = TRUE),
    min_blackice = min(blueice, na.rm = TRUE)
  )

#**8a. Plot of NTL total ice thickness----
total_ice_plt <- ggplot(data = ntl_clean2)+
  geom_line(aes(x = sampledate, y = sd_totice_high), color = 'transparent')+ #linetype = 'dashed', linewidth = 1.25
  geom_line(aes(x = sampledate, y = sd_totice_low), color = 'transparent')+ #linetype = 'dashed', linewidth = 1.25
  geom_ribbon(aes(x = sampledate, ymin = sd_totice_low, ymax = sd_totice_high), fill = 'lightblue')+
  #geom_line(aes(x = sampledate, y = totice, color = site))+
  geom_line(aes(x = sampledate, y = mean_totice), linewidth = 0.5)+
  #scale_color_manual(values = c('grey50','grey50','grey50','grey50','grey50','grey50','grey50','grey50','grey50','grey50','grey50'))+
  #facet_wrap(~site)+
  theme_classic()+
  xlab('')+
  ylab('Total Ice Thickness (cm)')
  
total_ice_plt

ggsave(here('results/figure_4f_v1.pdf'), dpi = 300, width = 5, height = 3)

#**8b. Plot of NTL black ice thickness----
black_ice_plt <- ggplot(data = ntl_clean2)+
  geom_line(aes(x = sampledate, y = sd_blackice_high), color = 'transparent')+ 
  geom_line(aes(x = sampledate, y = sd_blackice_low), color = 'transparent')+
  #geom_line(aes(x = sampledate, y = min_blackice), color = 'transparent')+ 
  #geom_line(aes(x = sampledate, y = max_blackice), color = 'transparent')+ 
  geom_ribbon(aes(x = sampledate, ymin = sd_blackice_low, ymax = sd_blackice_high), fill = 'lightblue')+
  #geom_ribbon(aes(x = sampledate, ymin = min_blackice, ymax = max_blackice), fill = 'lightblue')+
  geom_line(data = ntl_clean3 %>% 
              filter(site == 'TB') %>% 
              group_by(year) %>% 
              mutate(blueice = mean(blueice)) %>% 
              filter(row_number()==1), 
             aes(x = sampledate, y = blueice), color = 'grey50')+
  geom_line(aes(x = sampledate, y = mean_blackice), linewidth = 0.5)+
  theme_classic()+
  xlab('')+
  ylab('Black Ice Thickness (cm)')+
  ylim(c(0,70))

black_ice_plt

ggsave(here('results/figure_4f_v4_wo_TBmean.pdf'), dpi = 300, width = 5, height = 3)

#**8c. Plot of NTL white ice thickness----
white_ice_plt <- ggplot(data = ntl_clean2)+
  geom_line(aes(x = sampledate, y = sd_whiteice_high), color = 'transparent')+ 
  geom_line(aes(x = sampledate, y = sd_whiteice_low), color = 'transparent')+
  #geom_line(aes(x = sampledate, y = min_whiteice), color = 'transparent')+ #linetype = 'dashed', linewidth = 1.25
  #geom_line(aes(x = sampledate, y = max_whiteice), color = 'transparent')+ #linetype = 'dashed', linewidth = 1.25
  geom_ribbon(aes(x = sampledate, ymin = sd_whiteice_low, ymax = sd_whiteice_high), fill = 'lightblue')+
  #geom_ribbon(aes(x = sampledate, ymin = min_whiteice, ymax = max_whiteice), fill = 'lightblue')+
  geom_line(data = ntl_clean3 %>% 
              filter(site == 'TB') %>% 
              group_by(year) %>% 
              mutate(whiteice = mean(whiteice)) %>% 
              filter(row_number()==1), 
            aes(x = sampledate, y = whiteice), color = 'grey50')+
  geom_line(aes(x = sampledate, y = mean_whiteice), linewidth = 0.5)+
  theme_classic()+
  xlab('')+
  ylab('White Ice Thickness (cm)')+
  ylim(c(0,70))

white_ice_plt

ggsave(here('results/figure_4f_v5_wo_TBmean.pdf'), dpi = 300, width = 5, height = 3)

# 4. Combine the dataframes -----------------------------------------------

white_ice_clean2 <- ntl_clean %>% 
  bind_rows(fin_clean, ib_clean, hal_clean, rus_clean) %>% 
  group_by(site) %>% 
  mutate(
    year_min = min(year, na.rm = TRUE),
    year_max = max(year, na.rm = TRUE)
  ) %>% 
  filter(
    row_number() == 1
  ) %>% 
  dplyr::select(
    -c(
      white_ice_perc,
      year
    )
  )
#write_csv(white_ice_clean2, here('data/combined_ice_data_request.csv'))
