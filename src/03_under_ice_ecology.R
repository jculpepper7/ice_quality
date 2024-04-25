# The purpose of this script is to make a 
# stand-in figure for the under-ice ecology section
# We will use data from Shchapov et al., 2023
# Get doi: 


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(here)
library(janitor)
library(lubridate)
library(cmocean)

# 1. Read and clean data --------------------------------------------------

# winter_zoops <- read_csv(here('data/winter_zoop_data.csv'))
# 
# head(winter_zoops)
# 
# winter_zoops_clean <- winter_zoops %>% 
#   select(1:7) %>% 
#   clean_names() %>% 
#   mutate(
#     date = dmy(date),
#     white_ice = as.numeric(white_ice),
#     lake = as.factor(lake),
#     color_trophic_status = as.factor(color_trophic_status),
#     white_ice_ratio = white_ice/ice_thickness_m
#   ) %>% 
#   #remove extreme outlier
#   filter(
#     tot_zoops_ind_per_l < 50
#   )
# 
# 
# # 2. Plot correlation between white ice and zoops -------------------------
# 
# ggplot(data = winter_zoops_clean)+
#   geom_smooth(aes(y = tot_zoops_ind_per_l, x = white_ice_ratio), method = 'lm' )+
#   geom_point(aes(x = white_ice, y = tot_zoops_ind_per_l))+
#   theme_classic()+
#   xlab('White Ice ratio')+
#   ylab('Zooplankton Density (ind/L)')
# 
# #Really doesn't tell you much
# #Would need a lot more data
# 
# # 3. Plot time series of ice and zoops ---------------------------------------
# 
# #Only going to use lakes with more than one observation
# winter_zoops_abbr <- winter_zoops_clean[7:10,]
# 
# #time series plot
# ggplot(data = winter_zoops_abbr)+
#   geom_line(aes(x = date, y = tot_zoops_ind_per_l))+
#   geom_line(aes(x = date, y = white_ice_ratio*10), linetype = 'dashed')+
#   theme_classic()+
#   ylab('Zooplankton Density (ind/L)\nWhite Ice Fraction (x10)')+
#   xlab('')
#   
# 4. Phytoplankton data ---------------------------------------------------

# Read data

bramburger_npp <- read_csv(here('data/ecology/Bramburger_2023_npp.csv')) #listed values "<0.1" as NA
knoll_chla <- read_csv(here('data/ecology/Knoll_2023_NDS_chl.csv')) 
socha_chla <- read_csv(here('data/ecology/Socha_2023_SSB_chlorophyll.csv'))

# Clean data

bramburger_npp_clean <- bramburger_npp %>% 
  clean_names() %>% 
  mutate(
    color = as.factor(color),
    lake = as.factor(lake),
    ice_snow_perc_trans = as.numeric(ice_snow_perc_trans),
    summer_npp = as.numeric(summer_npp)
  ) %>% 
  select(color, lake, ice_npp, ice_snow_npp, summer_npp) %>% 
  pivot_longer(cols = c(3,4,5), names_to = 'treatment', values_to = 'npp') %>% 
  mutate(
    treatment = as.factor(treatment)
  ) %>% 
  arrange(lake) %>% 
  filter(color != "Green") %>% 
  filter(treatment!='summer_npp') %>% 
  mutate(
    condition = if_else(treatment == 'ice_snow_npp', as.factor('snow'), as.factor('snow_removed'))
  )

knoll_chla_clean <- knoll_chla %>% 
  clean_names() %>% 
  mutate(
    end_date = mdy(end_date),
    nutrient_treatment = as.factor(nutrient_treatment),
    snow_treatment = as.factor(snow_treatment),
    condition = if_else(snow_treatment == 'dark', as.factor('snow'), as.factor('snow_removed'))
  ) %>% 
  filter(nutrient_treatment == 'Control',
         length == 21)

# socha_phyto_clean <- socha_phyto %>% 
#   clean_names() %>% 
#   mutate(
#     lakeid = as.factor(lakeid),
#     year = as.factor(year4)
#   ) %>% 
#   select(-year4)

socha_chla_clean <- socha_chla %>% 
  clean_names() %>% 
  mutate(
    year = as.factor(year(sample_date)),
    condition = if_else(year == 2019, as.factor('snow'),
                        if_else(year == 2020, as.factor('late_snow_removed'), as.factor('snow_removed')))
  )
  

# 5. Phyto plots ----------------------------------------------------------

ggplot(bramburger_npp_clean %>% arrange(condition))+
  geom_boxplot(aes(x = condition, y = log(npp), fill = condition), width = 0.2)+
  geom_jitter(aes(x = condition, y = log(npp)), size = 2, width = 0.1, color = 'grey30')+
  theme_classic()+
  #scale_x_discrete(labels=c("Ice","Ice + Snow")) +
  labs(x = '', y = 'log( NPP )')+
  scale_x_discrete(labels = c('Snow', 'Snow\nRemoved'))+
  theme(
    text = element_text(size = 20),
    legend.position = 'none',
    legend.title = element_blank()
  )+
  scale_fill_manual(values = c('blue4', 'lightblue3'))
  


#ggsave(here('results/bramburger_data.png'), dpi = 300, width = 5, height = 3.5, units = 'in')
ggsave(here('results/figure_6a.pdf'), dpi = 300, width = 5, height = 3.5, units = 'in')

ggplot(knoll_chla_clean)+
  geom_boxplot(aes(x = condition, y = log(chl), fill = condition), width = 0.2)+
  geom_jitter(aes(x = condition, y = log(chl)), size = 2, width = 0.1, color = 'grey30')+
  theme_classic()+
  scale_x_discrete(labels = c('Snow', 'Snow\nRemoved'))+
  labs(x = '', y = 'log( Chlorophyll-a )')+
  theme(
    text = element_text(size = 20),
    legend.position = 'none',
    legend.title = element_blank()
  )+
  scale_fill_manual(values = c('blue4', 'lightblue'))


#ggsave(here('results/knoll_data.png'), dpi = 300, width = 5, height = 3.5, units = 'in')
ggsave(here('results/figure_6b.pdf'), dpi = 300, width = 5, height = 3.5, units = 'in')

ggplot(socha_chla_clean)+
  geom_boxplot(aes(x = condition, y = log(chlorophyll_ug_l), fill = condition), width = 0.2)+
  geom_jitter(aes(x = condition, y = log(chlorophyll_ug_l)), 
              size = 1, width = 0.1, color = 'grey30', alpha = 0.7)+
  theme_classic()+
  scale_x_discrete(labels=c("Snow","Snow\nRemoved\nLate", 'Snow\nRemoved')) +
  labs(x = '', y = 'log( Chlorophyll-a )')+
  theme(
    text = element_text(size = 20),
    legend.position = 'none',
    legend.title = element_blank()
  )+
  scale_fill_manual(values = c('blue4', 'blue4', 'lightblue'))

#ggsave(here('results/socha_data.png'), dpi = 300, width = 5, height = 3.5, units = 'in')
ggsave(here('results/figure_6c.pdf'), dpi = 300, width = 5, height = 3.5, units = 'in')

