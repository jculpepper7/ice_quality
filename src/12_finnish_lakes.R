# Goal of this script is to aggregate the Finnish lake ice data and plot trends

#Libraries

library(tidyverse)
library(here)
library(lubridate)
library(janitor)
library(Tides) #Finding gaps in data
library(TrackReconstruction) #Finding gaps in data
library(wql)

# 1. Import data ---------------------------------------------------------

ice_thick_1 <- read_csv(here('data/jaculp_yorku_ca_total_thickness_1.csv'))
ice_thick_2 <- read_csv(here('data/jaculp_yorku_ca_total_thickness_2.csv'))
ice_thick_3 <- read_csv(here('data/jaculp_yorku_ca_total_thickness_3.csv'))
ice_thick_4 <- read_csv(here('data/jaculp_yorku_ca_total_thickness_4.csv'))
ice_thick_5 <- read_csv(here('data/jaculp_yorku_ca_total_thickness_5.csv'))

black_ice_1 <- read_csv(here('data/jaculp_yorku_ca_steel_ice_1.csv'))
black_ice_2 <- read_csv(here('data/jaculp_yorku_ca_steel_ice_2.csv'))
black_ice_3 <- read_csv(here('data/jaculp_yorku_ca_steel_ice_3.csv'))
black_ice_4 <- read_csv(here('data/jaculp_yorku_ca_steel_ice_4.csv'))
black_ice_5 <- read_csv(here('data/jaculp_yorku_ca_steel_ice_5.csv'))

white_ice_1 <- read_csv(here('data/jaculp_yorku_ca_white_ice_1.csv'))
white_ice_2 <- read_csv(here('data/jaculp_yorku_ca_white_ice_2.csv'))
white_ice_3 <- read_csv(here('data/jaculp_yorku_ca_white_ice_3.csv'))
white_ice_4 <- read_csv(here('data/jaculp_yorku_ca_white_ice_4.csv'))
white_ice_5 <- read_csv(here('data/jaculp_yorku_ca_white_ice_5.csv'))

snow_depth_1 <- read_csv(here('data/jaculp_yorku_ca_snow_depth_1.csv'))
snow_depth_2 <- read_csv(here('data/jaculp_yorku_ca_snow_depth_2.csv'))
snow_depth_3 <- read_csv(here('data/jaculp_yorku_ca_snow_depth_3.csv'))
snow_depth_4 <- read_csv(here('data/jaculp_yorku_ca_snow_depth_4.csv'))
snow_depth_5 <- read_csv(here('data/jaculp_yorku_ca_snow_depth_5.csv'))

ntl <- read_csv(here('data/ntl_ice_quality.csv'))

# 2. Aggregate data -------------------------------------------------------

# **2a. Aggregate total thickness data ------------------------------------

#Total thickness data
total_thickness_long_1 <- ice_thick_1 %>% 
  pivot_longer(!date, names_to = 'lake_id', values_to = 'total_thickness') %>% 
  arrange(lake_id) %>% 
  mutate(
    date = dmy(date)
  )

total_thickness_long_2 <- ice_thick_2 %>% 
  pivot_longer(!date, names_to = 'lake_id', values_to = 'total_thickness') %>% 
  arrange(lake_id) %>% 
  mutate(
    date = dmy(date)
  )

total_thickness_long_3 <- ice_thick_3 %>% 
  pivot_longer(!date, names_to = 'lake_id', values_to = 'total_thickness') %>% 
  arrange(lake_id) %>% 
  mutate(
    date = dmy(date)
  )

total_thickness_long_4 <- ice_thick_4 %>% 
  pivot_longer(!date, names_to = 'lake_id', values_to = 'total_thickness') %>% 
  arrange(lake_id) %>% 
  mutate(
    date = dmy(date)
  )

total_thickness_long_5 <- ice_thick_5 %>% 
  pivot_longer(!date, names_to = 'lake_id', values_to = 'total_thickness') %>% 
  arrange(lake_id) %>% 
  mutate(
    date = dmy(date)
  )

#Combine ice thickness data

total_thickness_combined <- total_thickness_long_1 %>% 
  bind_rows(total_thickness_long_2, 
            total_thickness_long_3, 
            total_thickness_long_4, 
            total_thickness_long_5) %>% 
  mutate(
    year = year(date),
    water_year = if_else(
      month(date)>=10, year(date)+1, year(date)
    ),
    lake_id = as.factor(lake_id)
  )

tot_thick_summary <- total_thickness_combined %>% 
  group_by(lake_id, water_year) %>% 
  summarise(
    ice_mean = mean(total_thickness, na.rm = TRUE),
    ice_sd = sd(total_thickness, na.rm = TRUE),
    ice_median = median(total_thickness, na.rm = TRUE),
    ice_max = max(total_thickness, na.rm = TRUE)
  ) %>% 
  drop_na(ice_mean)

#write_csv(tot_thick_summary, 'data/finnish_total_ice.csv')
#write_csv(total_thickness_combined, 'data/finnish_total_ice.csv')



ggplot()+
  geom_point(data = tot_thick_summary, aes(x = water_year, y = ice_max))+
  geom_smooth(data = tot_thick_summary, aes(x = water_year, y = ice_max, color = lake_id), method = 'lm', se = F)+
  facet_wrap(~lake_id)

# **2b. Aggregate black ice data ------------------------------------------


black_ice_long_1 <- black_ice_1 %>% 
  pivot_longer(!date, names_to = 'lake_id', values_to = 'black_cm') %>% 
  arrange(lake_id) %>% 
  mutate(
    date = dmy(date),
    lake_id = as.factor(lake_id)
  )

black_ice_long_2 <- black_ice_2 %>% 
  pivot_longer(!date, names_to = 'lake_id', values_to = 'black_cm') %>% 
  arrange(lake_id) %>% 
  mutate(
    date = dmy(date),
    lake_id = as.factor(lake_id)
  )

black_ice_long_3 <- black_ice_3 %>% 
  pivot_longer(!date, names_to = 'lake_id', values_to = 'black_cm') %>% 
  arrange(lake_id) %>% 
  mutate(
    date = dmy(date),
    lake_id = as.factor(lake_id)
  )

black_ice_long_4 <- black_ice_4 %>% 
  pivot_longer(!date, names_to = 'lake_id', values_to = 'black_cm') %>% 
  arrange(lake_id) %>% 
  mutate(
    date = dmy(date),
    lake_id = as.factor(lake_id)
  )

black_ice_long_5 <- black_ice_5 %>% 
  pivot_longer(!date, names_to = 'lake_id', values_to = 'black_cm') %>% 
  arrange(lake_id) %>% 
  mutate(
    date = dmy(date),
    lake_id = as.factor(lake_id)
  )

#Black ice combined

#Combine ice thickness data

black_ice_combined <- black_ice_long_1 %>% 
  bind_rows(black_ice_long_2, 
            black_ice_long_3, 
            black_ice_long_4, 
            black_ice_long_5) %>% 
  mutate(
    year = year(date),
    water_year = if_else(
      month(date)>=10, year(date)+1, year(date)
    )
  )

black_ice_summary <- black_ice_combined %>% 
  group_by(lake_id, water_year) %>% 
  summarise(
    black_ice_mean = mean(black_cm, na.rm = TRUE),
    black_ice_sd = sd(black_cm, na.rm = TRUE),
    black_ice_median = median(black_cm, na.rm = TRUE),
    black_ice_max = max(black_cm, na.rm = TRUE)
  ) %>% 
  drop_na(black_ice_mean)

ggplot()+
  geom_point(data = black_ice_summary, aes(x = water_year, y = black_ice_mean))+
  geom_smooth(data = black_ice_summary, aes(x = water_year, y = black_ice_mean, color = lake_id), method = 'lm', se = F)+
  facet_wrap(~lake_id)

# **2c. Aggregate white ice data ------------------------------------------

#These white ice data are measured using the "Kohvasauva" method.
#In Finnish this is the snow ice measuring method.
#Rather than measuring from an augur hole ("Kohva"), a measuring stick
#is frozen into black ice. The white ice is then read from the measuring stick.
#According to the hydrologist I spoke with at the Finnish Environmental Institute
#this is a more accurate way to measure white ice than through an augur hole.

white_ice_long_1 <- white_ice_1 %>% 
  pivot_longer(!date, names_to = 'lake_id', values_to = 'white_cm') %>% 
  arrange(lake_id) %>% 
  mutate(
    date = dmy(date),
    lake_id = as.factor(lake_id)
  )

white_ice_long_2 <- white_ice_2 %>% 
  pivot_longer(!date, names_to = 'lake_id', values_to = 'white_cm') %>% 
  arrange(lake_id) %>% 
  mutate(
    date = dmy(date),
    lake_id = as.factor(lake_id)
  )

white_ice_long_3 <- white_ice_3 %>% 
  pivot_longer(!date, names_to = 'lake_id', values_to = 'white_cm') %>% 
  arrange(lake_id) %>% 
  mutate(
    date = dmy(date),
    lake_id = as.factor(lake_id)
  )

white_ice_long_4 <- white_ice_4 %>% 
  pivot_longer(!date, names_to = 'lake_id', values_to = 'white_cm') %>% 
  arrange(lake_id) %>% 
  mutate(
    date = dmy(date),
    lake_id = as.factor(lake_id)
  )

white_ice_long_5 <- white_ice_5 %>% 
  pivot_longer(!date, names_to = 'lake_id', values_to = 'white_cm') %>% 
  arrange(lake_id) %>% 
  mutate(
    date = dmy(date),
    lake_id = as.factor(lake_id)
  )

#White ice combined

#Combine ice thickness data

white_ice_combined <- white_ice_long_1 %>% 
  bind_rows(white_ice_long_2, 
            white_ice_long_3, 
            white_ice_long_4, 
            white_ice_long_5) %>% 
  mutate(
    year = year(date),
    water_year = if_else(
      month(date)>=10, year(date)+1, year(date)
    )
  )

white_ice_summary <- white_ice_combined %>% 
  group_by(lake_id, water_year) %>% 
  summarise(
    white_ice_mean = mean(white_cm, na.rm = TRUE),
    white_ice_sd = sd(white_cm, na.rm = TRUE),
    white_ice_median = median(white_cm, na.rm = TRUE),
    white_ice_max = max(white_cm, na.rm = TRUE)
  ) %>% 
  drop_na(white_ice_mean)

#write_csv(white_ice_summary, 'data/finnish_white_ice.csv')
#write_csv(white_ice_combined, 'data/finnish_white_ice.csv')


ggplot()+
  geom_point(data = white_ice_summary, aes(x = water_year, y = white_ice_mean))+
  geom_smooth(data = white_ice_summary, aes(x = water_year, y = white_ice_mean, color = lake_id), method = 'lm', se = F)+
  facet_wrap(~lake_id)

white_ice_summary %>% group_by(lake_id) %>%  filter(n()>30)
# **2d. Aggregate snow depth data ------------------------------------------


# snow_depth_long_1 <- snow_depth_1 %>% 
#   pivot_longer(!date, names_to = 'lake_id', values_to = 'snow_cm') %>% 
#   arrange(lake_id) %>% 
#   mutate(
#     date = dmy(date),
#     lake_id = as.factor(lake_id)
#   )
# 
# snow_depth_long_2 <- snow_depth_2 %>% 
#   pivot_longer(!date, names_to = 'lake_id', values_to = 'snow_cm') %>%  
#   arrange(lake_id) %>% 
#   mutate(
#     date = dmy(date),
#     lake_id = as.factor(lake_id)
#   )
# 
# snow_depth_long_3 <- snow_depth_3 %>% 
#   pivot_longer(!date, names_to = 'lake_id', values_to = 'snow_cm') %>% 
#   arrange(lake_id) %>% 
#   mutate(
#     date = dmy(date),
#     lake_id = as.factor(lake_id)
#   )
# 
# snow_depth_long_4 <- snow_depth_4 %>% 
#   pivot_longer(!date, names_to = 'lake_id', values_to = 'snow_cm') %>% 
#   arrange(lake_id) %>% 
#   mutate(
#     date = dmy(date),
#     lake_id = as.factor(lake_id)
#   )
# 
# snow_depth_long_5 <- snow_depth_5 %>% 
#   pivot_longer(!date, names_to = 'lake_id', values_to = 'snow_cm') %>%  
#   arrange(lake_id) %>% 
#   mutate(
#     date = dmy(date),
#     lake_id = as.factor(lake_id)
#   )

#Snow depth combined

# snow_depth_combined <- snow_depth_long_1 %>% 
#   bind_rows(snow_depth_long_2, 
#             snow_depth_long_3, 
#             snow_depth_long_4, 
#             snow_depth_long_5) %>% 
#   mutate(
#     year = year(date)
#   )
# 
# snow_depth_summary <- snow_depth_combined %>% 
#   group_by(lake_id, year) %>% 
#   summarise(
#     snow_depth_mean = mean(snow_cm, na.rm = TRUE),
#     snow_depth_sd = sd(snow_cm, na.rm = TRUE),
#     snow_depth_median = median(snow_cm, na.rm = TRUE),
#     snow_depth_max = max(snow_cm, na.rm = TRUE)
#   ) %>% 
#   drop_na(snow_depth_mean)

# ggplot()+
#   geom_point(data = snow_depth_summary, aes(x = year, y = snow_depth_mean))+
#   geom_smooth(data = snow_depth_summary, aes(x = year, y = snow_depth_mean, color = lake_id), method = 'lm', se = F)+
#   facet_wrap(~lake_id)


# **2e. Aggregate NTL data ------------------------------------------------

ntl_clean <- ntl %>% 
  dplyr::select(
    1, #lakeid 
    4, #sampledate  
    10, #totice = total ice
    12, #whiteice
    13 #blueice = black ice
  ) %>% 
  mutate(
    lake_id = as.factor(lakeid),
    date = mdy(sampledate),
    water_year = if_else(
      month(date)>=10, year(date)+1, year(date)
    )
  ) %>% 
  dplyr::select(
    lake_id,
    date,
    water_year,
    total_thickness = totice,
    white_cm = whiteice,
    black_cm = blueice
  )

ntl_ice_summary <- ntl_clean %>% 
  group_by(lake_id, water_year) %>% 
  summarise(
    ice_mean = mean(total_thickness, na.rm = TRUE),
    ice_max = max(total_thickness, na.rm = TRUE),
    black_ice_mean = mean(black_cm, na.rm = TRUE),
    black_ice_max = max(black_cm, na.rm = TRUE),
    white_ice_mean = mean(white_cm, na.rm = TRUE),
    white_ice_max = max(white_cm, na.rm = TRUE)
  ) #%>% 
  #drop_na(white_ice_mean)
# 
# ggplot()+
#   geom_point(data = ntl_ice_summary, aes(x = water_year, y = black_ice_mean))+
#   theme_bw()+
#   facet_wrap(~lake_id)+
#   geom_smooth(data = ntl_ice_summary, aes(x = water_year, y = black_ice_mean), se = F)

# 3. Time series analysis -------------------------------------------------

#Total thickness

tot_thick_trends <- tot_thick_summary %>% 
  group_by(lake_id) %>% 
  filter(n()>=30) %>% 
  summarise(
    mk_mean = mannKen(ice_mean)$p.value,
    sen_mean = mannKen(ice_mean)$sen.slope,
    mk_max = mannKen(ice_max)$p.value,
    sen_max = mannKen(ice_max)$sen.slope,
  )

tot_thick_trends %>% 
  filter(mk_mean<0.05) #%>% 
  #filter(sen_mean>0)

summary(tot_thick_trends %>% filter(mk_mean<0.05) ) 
#ADDING THE WATER YEAR ACTUALLY INCREASED THE MEAN TREND TO -0.285 (cm/yr)

#Black ice thickness

black_ice_trends <- black_ice_summary %>% 
  group_by(lake_id) %>% 
  #filter(n()>=30) %>% 
  summarise(
    mk_mean = mannKen(black_ice_mean)$p.value,
    sen_mean = mannKen(black_ice_mean)$sen.slope,
    mk_max = mannKen(black_ice_max)$p.value,
    sen_max = mannKen(black_ice_max)$sen.slope,
  )

black_ice_trends %>% 
  filter(mk_max<0.05) #%>% 
  #filter(sen_mean>0)

#Take a look at the only black ice measurement with a significant trend
# ggplot(data = black_ice_summary %>% filter(lake_id == '61301_I'))+
#   geom_point(aes(x = year, y = black_ice_mean))
# 
# ggplot(data = tot_thick_summary %>% filter(lake_id == '61301_I'))+
#   geom_point(aes(x = year, y = ice_mean))

#white ice thickness

white_ice_trends <- white_ice_summary %>% 
  group_by(lake_id) %>% 
  filter(n()>=30) %>% 
  summarise(
    mk_wht_mean = mannKen(white_ice_mean)$p.value,
    sen_wht_mean = mannKen(white_ice_mean)$sen.slope,
    mk_wht_max = mannKen(white_ice_max)$p.value,
    sen_wht_max = mannKen(white_ice_max)$sen.slope,
  )


white_ice_trends %>% 
  filter(mk_wht_mean<0.05) #%>% 
  #filter(sen_wht_max>0)

summary(white_ice_trends %>% filter(mk_wht_max<0.05) %>% filter(sen_wht_max>0)) 


# **NTL trends ------------------------------------------------------------

ntl_trends <- ntl_ice_summary %>% 
  group_by(lake_id) %>% 
  filter(n()>=30) %>% 
  summarise(
    mk_mean = mannKen(ice_mean)$p.value,
    sen_mean = mannKen(ice_mean)$sen.slope,
    mk_max = mannKen(ice_max)$p.value,
    sen_max = mannKen(ice_max)$sen.slope,
    mk_blk_mean = mannKen(black_ice_mean)$p.value,
    sen_blk_mean = mannKen(black_ice_mean)$sen.slope,
    mk_blk_max = mannKen(black_ice_max)$p.value,
    sen_blk_max = mannKen(black_ice_max)$sen.slope,
    mk_wht_mean = mannKen(white_ice_mean)$p.value,
    sen_wht_mean = mannKen(white_ice_mean)$sen.slope,
    mk_wht_max = mannKen(white_ice_max)$p.value,
    sen_wht_max = mannKen(white_ice_max)$sen.slope,
  )

#Sig. thickness trends? - Mean: NO ; Max: NO
summary(ntl_trends %>% filter(mk_mean<0.05) ) 

#Sig. black trends? - Mean:Yes , 1 - TB, dec. -0.207 ; Max:Yes , 3 - CR, SP, TB, -0.33
summary(ntl_trends %>% filter(mk_blk_mean<0.05) ) #%>% filter(sen_wht_max>0)

#Sig. white trends? - Mean: Yes, 1 - TB,  0.04848; Max: NO 
summary(ntl_trends %>% filter(mk_wht_mean<0.05))  # %>% filter(sen_wht_max>0)

# 4. Finnish lakes plot ---------------------------------------------------

x <- white_ice_trends %>% 
  left_join(tot_thick_trends) %>% 
  filter(
    mk_mean <= 0.05,
    mk_wht_mean >0.05
  ) %>%
  dplyr::select(lake_id, sen_wht_mean, sen_wht_max, sen_mean, sen_max) %>% 
  pivot_longer(
    !lake_id,
    names_to = 'type',
    values_to = 'trend'
  )

y <- white_ice_trends %>% 
  left_join(tot_thick_trends) %>% 
  filter(
    mk_wht_mean <= 0.05,
  ) %>%
  dplyr::select(lake_id, sen_wht_mean, sen_wht_max, sen_mean, sen_max) %>% 
  pivot_longer(
    !lake_id,
    names_to = 'type',
    values_to = 'trend'
  )

ggplot()+
  geom_boxplot(data = x, aes(x = type, y = trend), width = 0.4, outlier.shape = NA)+
  geom_jitter(data = x,aes(x = type, y = trend), alpha=0.5)+
  geom_jitter(
    data = y,
    aes(x = type, y = trend),
    color = 'blue',
    alpha=0.5)+
  geom_hline(yintercept = 0, linetype = 'dashed', alpha = 0.5)+
  labs(x = '', y = 'Trend (cm/yr)')+
  theme_classic()+
  scale_x_discrete(labels=c("Total Ice\nMaximum","Total Ice\nMean", 
                            "White Ice\nMaximum", 'White Ice\nMean'))+
  theme(
    text = element_text(size = 25)
  )

ggsave(here('results/ice_trend_boxplot_alt.pdf'), dpi = 300, width = 11, height = 10, units = 'in')  


