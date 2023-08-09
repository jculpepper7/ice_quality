# The goal of this script is to 
# 1. Clean the existing ice quality data from Weyhenmeyer et al., 2022
# 2. Use the cleaned data to create figures for the NREE manuscript


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(janitor)
library(lubridate)
library(here)
library(wql)


# 1. Import data ----------------------------------------------------------

ice_raw <- read_csv(here('data/ice_quality_data.csv'))


# 2. Clean data frame -----------------------------------------------------

ice_clean <- ice_raw %>%
  clean_names() %>%
  rename(
    date = date_of_sampling,
    snow_height_cm = height_of_snow_on_ice_in_cm,
    slush_height_cm = height_of_slush_on_ice_in_cm,
    ice_total_cm = total_ice_thickness_without_the_snow_slush_on_ice_cm,
    white_ice_cm = white_ice_thickness_i_e_the_upper_ice_layer_in_cm,
    black_ice_cm = black_ice_thickness_i_e_the_lower_ice_layer_in_cm
  ) #%>%
  # select(-contact_person) %>% 
  # mutate(
  #   date = mdy(date),
  #   snow_height_cm = grep(',', '.')
  # )

ice_clean_2 <- ice_raw %>% 
  select(3,4)

# 3. Visualize global ice quality data ------------------------------------

#PUT CODE HERE ONCE YOU FIGURE OUT THE grep() issue


# 4. Time series data  ----------------------------------------------------

#Data for ice quality time series come from Zdorovennova et al., 2021
#Supplemental Table 3
#DOI: https://www.mdpi.com/2073-4441/13/17/2435

# Import data

iq_ts <- read_csv(here('data/ice_quality_zdorovennova_2021.csv')) #iq_ts, as in ice quality time series

iq_ts2 <- iq_ts %>% 
  mutate(
    date = mdy(date),
    white_ice_ratio = white_ice_avg_cm/total_ice_avg_cm
  )

#head(iq_ts2)

#Plot total, black, and white ice thickness
ggplot(data = iq_ts2)+
  geom_smooth(aes(x = date, y = total_ice_avg_cm), method = 'lm', color = 'grey25', se = F)+
  geom_smooth(aes(x = date, y = black_ice_avg_cm), method = 'lm', color = 'grey50', se = F)+
  geom_smooth(aes(x = date, y = white_ice_avg_cm), method = 'lm', color = 'grey75', se = F, linetype = 'dashed')+
  geom_point(aes(x = date, y = total_ice_avg_cm, shape = 'Total Ice'), color = 'grey25', size = 3)+
  geom_point(aes(x = date, y = black_ice_avg_cm, shape = 'Black Ice'), color = 'grey50', size = 3)+
  geom_point(aes(x = date, y = white_ice_avg_cm, shape = "White Ice"), color = 'grey75', size = 3)+
  ylab('Ice Thickness (cm)')+
  xlab('')+
  theme_classic()+
  #scale_shape_discrete('', breaks = c('Total Ice', 'Black Ice', 'White Ice'))+
  theme(
    axis.title = element_text(size = 25),
    axis.text = element_text(size = 22),
    legend.text = element_text(size = 22)
  )

#Save image 
ggsave(here('results/iq_ts_draft.png'), dpi = 300, units = 'in', width = 11, height = 10)

#What if we average by year because only two measurements per year 
#does not say much about the winter/spring progression of ice quality?

#Get yearly averages

iq_ts3 <- iq_ts2 %>% 
  mutate(
    year = year(date)
  ) %>%   
  group_by(year) %>% 
  summarise(
    total_mean = mean(total_ice_avg_cm),
    black_mean = mean(black_ice_avg_cm),
    white_mean = mean(white_ice_avg_cm),
    white_ice_ratio = mean(white_ice_ratio)
  )

#Visualize summarised thickness data
ggplot(data = iq_ts3)+
  geom_smooth(aes(x = year, y = total_mean), method = 'lm', color = 'grey25', se = F)+
  geom_smooth(aes(x = year, y = black_mean), method = 'lm', color = 'grey50', se = F)+
  geom_smooth(aes(x = year, y = white_mean), method = 'lm', color = 'grey75', se = F, linetype = 'dashed')+
  geom_point(aes(x = year, y = total_mean), shape = 'square', color = 'grey25', size = 3)+
  geom_point(aes(x = year, y = black_mean), shape = 'triangle', color = 'grey50', size = 3)+
  geom_point(aes(x = year, y = white_mean), shape = 'circle', color = 'grey75', size = 3)+
  ylab('')+
  xlab('')+
  theme_classic()+
  theme(
    axis.title = element_text(size = 25),
    axis.text = element_text(size = 22)
  )

ggsave(here('results/iq_ts_final.png'), dpi = 300, units = 'in', height = 10, width = 11)

# Visualize white ice ratio
ggplot(data = iq_ts3)+
  geom_smooth(aes(x = year, y = white_ice_ratio), method = 'lm', color = 'grey25', se = T)+
  geom_line(aes(x = year, y = white_ice_ratio), shape = 'square', color = 'grey25', size = 3)+
  ylab('White Ice Ratio')+
  xlab('Year')+
  theme_classic()+
  theme(
    axis.title = element_text(size = 25),
    axis.text = element_text(size = 22)
  )


# 5. Mann-Kendall test on Zdorovennova 2021 data --------------------------

mk_total_ice <- mannKen(iq_ts2$total_ice_avg_cm)
mk_total_ice #p value: 0.0005, significant trend, sen = -0.5 cm/yr

pett(iq_ts2$total_ice_avg_cm)

mk_black_ice <- mannKen(iq_ts2$black_ice_avg_cm)
mk_black_ice #p value: 0.012, significant trend, sen = -0.28 cm/yr

mk_white_ice <- mannKen(iq_ts2$white_ice_avg_cm)
mk_white_ice #p value: 0.286, null hypothesis, not changing, sen = -0.19

mk_white_ratio <- mannKen(iq_ts2$white_ice_ratio)
mk_white_ratio #p value: 0.286, null hypothesis

mk_total_ice_mean <- mannKen(iq_ts3$total_mean)
mk_total_ice_mean #p value: 0.013, significant trend, sen = -0.71 cm/yr

mk_black_ice_mean <- mannKen(iq_ts3$black_mean)
mk_black_ice_mean #p value: 0.039, significant trend, sen = -0.52 cm/yr

mk_white_ice_mean <- mannKen(iq_ts3$white_mean)
mk_white_ice_mean #p value: 0.67, null hypothesis, sen = -0.18

mk_white_ice_ratio_mean <- mannKen(iq_ts3$white_ice_ratio)
mk_white_ice_ratio_mean

