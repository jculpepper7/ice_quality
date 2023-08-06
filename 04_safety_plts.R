# The purpose of this script is to look at 
# 1. Individual months for 1, 2, and 4 C warming
#    during the winter and spring months for two aspects
# 2. Ice thickness between December and May
# 3. Winter drownings during this time period
# 4. Make a plot (problaly box plot) to convey the median and quantile change
#    between months at varying months


# 1. Libraries ------------------------------------------------------------

library(tidyverse)
library(here)
library(lubridate)
library(janitor)
library(ncdf4)
library(cmocean)

# 2. Import netCDF data ---------------------------------------------------

seasonal_ice_depth <- nc_open(here('data/icethick_daily_targetyears_ensmean.nc'))

# 2b. Extract variables of interest---------------------------------------------

#Extract ice duration data
#Key id = ice depth (to differentiate from snow depth below)
ice_thickness <- ncvar_get(seasonal_ice_depth, 'LAKEICETHICK')
ilat_id <- ncvar_get(seasonal_ice_depth, 'lat')
ilon_id <- ncvar_get(seasonal_ice_depth, 'lon')

nh_lat <- ilat_id >= 20 #northern hemisphere latitudes
ilat_nh <- ilat_id[nh_lat]
# 2c. Reorient time variable----------------------------------------------------
tt_id <- ncvar_get(seasonal_ice_depth, 'time')

#Convert time from days since 1850-01-01 to a vector of dates
itime_id <- as.Date(tt_id, origin = "1850-01-01")
#itime_id_df <- tibble(itime_id)

#Extract the year
iyear_id <- year(itime_id)

#Extract seasonal cycle for specific years
idx_year1 <-  iyear_id >= 1851 & iyear_id <= 1851
idx_year2 <-  iyear_id >= 1899 & iyear_id <= 1901
idx_year3 <-  iyear_id >= 2009 & iyear_id <= 2011
idx_year4 <-  iyear_id >= 2042 & iyear_id <= 2044
idx_year5 <-  iyear_id >= 2086 & iyear_id <= 2088

#Extact seasonal cycle for specific years
# idx_year1 <- itime_id >= as.Date('1900-09-01') & itime_id <= as.Date('1901-08-31')
# idx_year2 <- itime_id >= as.Date('1850-09-01') & itime_id <= as.Date('1851-08-31')
# idx_year3 <- itime_id >= as.Date('2010-09-01') & itime_id <= as.Date('2011-08-31')
# idx_year4 <- itime_id >= as.Date('2043-09-01') & itime_id <= as.Date('2044-08-31')
# idx_year5 <- itime_id >= as.Date('2087-09-01') & itime_id <= as.Date('2088-08-31')

#Get the day of year for each seasonal cycle

#index the time variable by the year of interest and extract the day of year
#These variables will be the x-axis of the time series plots
iyear1 <- itime_id[idx_year1] 
iyear2 <- itime_id[idx_year2]
iyear3 <- itime_id[idx_year3]
iyear4 <- itime_id[idx_year4]
iyear5 <- itime_id[idx_year5]

#Index period of interest after initial index to attempt to fix error with time series

# iyear2_idx <-  iyear2 >= '1900-09-01' & iyear2 <= '1901-08-31'
# 
# ts_hist <- 

#Must revise the day of year so that the days are sequential and do not repeat.
idoy1 <- iyear1 - iyear1[1] + 1  
idoy2 <- iyear2 - iyear2[1] + 1  
idoy3 <- iyear3 - iyear3[1] + 1  
idoy4 <- iyear4 - iyear4[1] + 1  
idoy5 <- iyear5 - iyear5[1] + 1  


#Check starting times
iyear2[1]  #Appear to be out of sync
iyear3[1]  
iyear4[1] 
iyear5[1]

# 2d. Extract lat and long of interest from global data-------------------------

#North America
# idx_lat_id2 = ilat_id >= 40 & ilat_id <= 77
# idx_lon_id2 = ilon_id >= 190 & ilon_id <= 330

#Northern Hemisphere
idx_lat_id2 = ilat_id >= 20 & ilat_id <= 90
idx_lon_id2 = ilon_id >= 0 & ilon_id <= 360

# Filter ice thickness time series to lat and lon of interest

seasonal_ice_thick = ice_thickness[idx_lon_id2,idx_lat_id2,]

# 2e. Create time series based on temperature threshold-------------------------

seasonal_ice_thick1 = seasonal_ice_thick[,,idx_year1]*100 #convert to cm
seasonal_ice_thick2 = seasonal_ice_thick[,,idx_year2]*100
seasonal_ice_thick3 = seasonal_ice_thick[,,idx_year3]*100
seasonal_ice_thick4 = seasonal_ice_thick[,,idx_year4]*100
seasonal_ice_thick5 = seasonal_ice_thick[,,idx_year5]*100

#Functio to extract first DOY (starting in Sept 1) when ice is above 13cm in thickness
my_ice_on_func <- function(y){
  
  y2 <- NA
  if(all(!is.na(y))){
    y2 <- -1 #Eliminate lakes where not all time series has a value
    if(any(y>13)){
      y2 <- min(which(y>13))
    }
    if(y2==1){
      y2 <- -2 #eliminate pixels where lake is is 13cm on day 1
    }
  }
  
  return(y2)
  
}

#CAUTION: anomalies will now have an issue due to negative values (-1, -2)

# Use above ice on function to extract day of year (DOY) that   
x2 <- apply(seasonal_ice_thick2, c(1,2), my_ice_on_func) 
x3 <- apply(seasonal_ice_thick3, c(1,2), my_ice_on_func) 
x4 <- apply(seasonal_ice_thick4, c(1,2), my_ice_on_func) 
x5 <- apply(seasonal_ice_thick5, c(1,2), my_ice_on_func) 




#Remove -2 and -1 values for the anomalies
x2b <- x2

x3b <- x3

x4b <- x4

x5b <- x5

# Historic condition
x2b[x2b==-2] <- NA #for anomaly calculations to eliminate the negative values
x2b[x2b==-1] <- NA

# 1 degree warming
x3b[x3b==-2] <- NA #for anomaly calculations to eliminate the negative values
x3b[x3b==-1] <- NA

# 2 degrees warming
x4b[x4b==-2] <- NA #for anomaly calculations to eliminate the negative values
x4b[x4b==-1] <- NA

# 4 degrees warming
x5b[x5b==-2] <- NA #for anomaly calculations to eliminate the negative values
x5b[x5b==-1] <- NA

# Anomalies
x3_anom <- x3b - x2b #max = 53, min = 3, median = 33, mean = 33, n = 2756
x4_anom <- x2b - x4b #max = 59, min = -6, median = 36, mean = 35, n = 2691
x5_anom <- x2b - x5b #max = 66, min = -8, median = 35, mean = 34, n = 2523

#range(x5_anom, na.rm = T)

#Need script 00 mapping function to run this code chunk
mapCDFtemp(lat = ilat_nh, 
           lon = ilon_id, 
           idat = x5,
           grid_size = 1.25,
           limits = c(min(x5, na.rm = T), max(x5, na.rm = T)), #c(0,165) #These numbers are the maximum day of year of formation for the 4 degree warming scenario
           name_cmocean = 'balance', 
           direction = 1, 
           titletext = '4 Degree Warming (13 cm)'
)

#ggsave(here('results/plots/DOY_13cm_4deg.png'), dpi = 300, width = 9, height = 9, units = 'in')


#The below code is to access the time series and plot it
test_hist <- apply(seasonal_ice_thick2, 3, mean, na.rm = TRUE) 
test_1deg <- apply(seasonal_ice_thick3, 3, mean, na.rm = TRUE) 
test_2deg <- apply(seasonal_ice_thick4, 3, mean, na.rm = TRUE) 
test_4deg <- apply(seasonal_ice_thick5, 3, mean, na.rm = TRUE) 

test_hist2 <- as.data.frame(as.numeric(test_hist))
test_1deg2 <- as.data.frame(as.numeric(test_1deg))
test_2deg2 <- as.data.frame(as.numeric(test_2deg)) 
test_4deg2 <- as.data.frame(as.numeric(test_4deg)) 
idoy2 <- as.data.frame(as.numeric(idoy2))

ice_thick_df <- idoy2 %>% 
  bind_cols(test_hist2, test_1deg2, test_2deg2, test_4deg2) %>% 
  rename(
    idoy = 1, 
    historic = 2,
    warming_1_degC = 3,
    warming_2_degC = 4,
    warming_4_degC = 5
  ) %>% 
  mutate(
    historic = round(historic, 1),
    warming_1_degC = round(warming_1_degC, 1),
    warming_2_degC = round(warming_2_degC, 1),
    warming_4_degC = round(warming_4_degC, 1),
    # date_1C = as_date(idoy, origin = '2009-12-31'),
    # date_2C = as_date(idoy, origin = '2041-12-31'),
    # date_4C = as_date(idoy, origin = '2088-12-31'),
    # month = month(date_1C)
  ) %>% 
  slice(334:515)

#Need to break the df apart to get the individual dates, so that I
#can bring the df back together with stacked dates in the correct order

#Historical conditions
ice_thick_hist <- ice_thick_df %>% 
  select(1,2) %>% 
  mutate(
    date = as_date(idoy, origin = '1851-01-01'),
    scenario = as.factor('Historic (1851-1880)')
  ) %>% 
  rename(
    ice_thickness = 'historic'
  )

#1 degree C warming
ice_thick_1C <- ice_thick_df %>% 
  select(1,3) %>% 
  mutate(
    date = as_date(idoy, origin = '2010-01-01'),
    scenario = as.factor('1°C')
  ) %>% 
  rename(
    ice_thickness = 'warming_1_degC'
  )

#2 degree C warming
ice_thick_2C <- ice_thick_df %>% 
  select(1,4) %>% 
  mutate(
    date = as_date(idoy, origin = '2042-01-01'),
    scenario = as.factor('2°C')
  ) %>% 
  rename(
    ice_thickness = 'warming_2_degC'
  )

#4 degree C warming
ice_thick_4C <- ice_thick_df %>% 
  select(1,5) %>% 
  mutate(
    date = as_date(idoy, origin = '2089-01-01'),
    scenario = as.factor('4°C')
  ) %>% 
  rename(
    ice_thickness = 'warming_4_degC'
  )

#Join the separate dfs back together in rows

ice_thick_df2 <- ice_thick_hist %>% 
  bind_rows(ice_thick_1C, ice_thick_2C, ice_thick_4C) %>% 
  select(-1) %>% 
  select(2,3,1) %>% 
  mutate(
    month = month(date),
    year = year(date)
  ) %>% 
  filter(
    month != 6,
    month != 7, 
    month != 8,
    month != 9,
    month != 10,
    month != 11
  ) %>% 
  mutate(
    season = as.factor(if_else(month < 3 | month == 12, 'winter', 'spring'))
  )

ice_thick_winter <- ice_thick_df2 %>% 
  filter(
    season == 'winter'
  )

ggplot(data = ice_thick_df2)+
  geom_boxplot(aes(x = season, y = ice_thickness, fill = scenario))+
  theme_classic()+
  scale_fill_cmocean(name = 'ice', discrete = TRUE, direction = -1, start = 0.3, end = 0.8)+
  scale_x_discrete(labels = c('Spring', 'Winter'))+
  theme(
    legend.position = 'bottom',
    text = element_text(size = 25)
  )+
  xlab('')+
  ylab('Ice Thickness (cm)')+
  guides(fill = guide_legend(title = ' '))

