#Libraries----
library(tidyverse)
library(ncdf4)
library(here)
library(fields)
library(ggpubr)

# 1. Get data----
on_data <- nc_open(here('data/Iceon_ensmean.nc'))
off_data <- nc_open(here('data/Iceoff_ensmean.nc'))

# 2. Extract variables----
iceon <- ncvar_get(on_data, 'iceon')
iceoff <- ncvar_get(off_data, 'iceoff')
ithresh <- ncvar_get(on_data, 'threshold')
year <- ncvar_get(on_data, 'year') %>% 
  as.data.frame()

# 3. Extract ice on thresholds----

#Walking on ice for black ice conditions
iceon_4_inch <- iceon[,,,ithresh == 4]

#Walking on ice for white ice conditions
iceon_5_inch <- iceon[,,,ithresh == 5]

#Hunting scenario on ice for black ice conditions
#Weight estimate of 1350kg for hunters, equipment, and game 
iceon_8_inch <- iceon[,,,ithresh == 8]

#Hunting scenario on ice for black ice conditions
iceon_12_inch <- iceon[,,,ithresh == 12]

# 4. Extract ice off thresholds----

#Walking on ice for black ice conditions
iceoff_4_inch <- iceoff[,,,ithresh == 4]

#Walking on ice for white ice conditions
iceoff_5_inch <- iceoff[,,,ithresh == 5]

#Hunting scenario on ice for black ice conditions
#Weight estimate of 1350kg for hunters, equipment, and game 
iceoff_8_inch <- iceoff[,,,ithresh == 8]

#Hunting scenario on ice for black ice conditions
iceoff_12_inch <- iceoff[,,,ithresh == 12]


# 5. Create time series for each thickness---- 

#Ice on scenarios

#4-inch scenario
av_iceon_4_inch <- apply(iceon_4_inch, 3, mean, na.rm = TRUE) %>% 
  as.data.frame() %>% 
  #bind_cols(year) %>% 
  rename(iceon_doy_4 = 1) %>% 
  mutate(
    std_dev_4on = sd(iceon_doy_4),
    sd_high_4on = iceon_doy_4 + std_dev_4on,
    sd_low_4on = iceon_doy_4 - std_dev_4on
  )
  
av_iceon_5_inch <- apply(iceon_5_inch, 3, mean, na.rm = TRUE) %>% 
  as.data.frame() %>% 
  #bind_cols(year) %>% 
  rename(iceon_doy_5 = 1) %>% 
  mutate(
    std_dev_5on = sd(iceon_doy_5),
    sd_high_5on = iceon_doy_5 + std_dev_5on,
    sd_low_5on = iceon_doy_5 - std_dev_5on
  )

av_iceon_8_inch <- apply(iceon_8_inch, 3, mean, na.rm = TRUE) %>% 
  as.data.frame() %>% 
  #bind_cols(year) %>% 
  rename(iceon_doy_8 = 1) %>% 
  mutate(
    std_dev_8on = sd(iceon_doy_8),
    sd_high_8on = iceon_doy_8 + std_dev_8on,
    sd_low_8on = iceon_doy_8 - std_dev_8on
  )

av_iceon_12_inch <- apply(iceon_12_inch, 3, mean, na.rm = TRUE) %>% 
  as.data.frame() %>% 
  #bind_cols(year) %>% 
  rename(iceon_doy_12 = 1) %>% 
  mutate(
    std_dev_12on = sd(iceon_doy_12),
    sd_high_12on = iceon_doy_12 + std_dev_12on,
    sd_low_12on = iceon_doy_12 - std_dev_12on
  )

iceon_safety_data <- bind_cols(year, av_iceon_4_inch, av_iceon_5_inch, av_iceon_8_inch, av_iceon_12_inch) %>% 
  rename(year = 1)

#Ice off scenarios
av_iceoff_4_inch <- apply(iceoff_4_inch, 3, mean, na.rm = TRUE) %>% 
  as.data.frame() %>% 
  #bind_cols(year) %>% 
  rename(iceoff_doy_4 = 1) %>% 
  mutate(
    std_dev_4off = sd(iceoff_doy_4),
    sd_high_4off = iceoff_doy_4 + std_dev_4off,
    sd_low_4off = iceoff_doy_4 - std_dev_4off
  )

av_iceoff_5_inch <- apply(iceoff_5_inch, 3, mean, na.rm = TRUE) %>% 
  as.data.frame() %>% 
  #bind_cols(year) %>% 
  rename(iceoff_doy_5 = 1) %>% 
  mutate(
    std_dev_5off = sd(iceoff_doy_5),
    sd_high_5off = iceoff_doy_5 + std_dev_5off,
    sd_low_5off = iceoff_doy_5 - std_dev_5off
  )

av_iceoff_8_inch <- apply(iceoff_8_inch, 3, mean, na.rm = TRUE) %>% 
  as.data.frame() %>% 
  #bind_cols(year) %>% 
  rename(iceoff_doy_8 = 1) %>% 
  mutate(
    std_dev_8off = sd(iceoff_doy_8),
    sd_high_8off = iceoff_doy_8 + std_dev_8off,
    sd_low_8off = iceoff_doy_8 - std_dev_8off
  )

av_iceoff_12_inch <- apply(iceoff_12_inch, 3, mean, na.rm = TRUE) %>% 
  as.data.frame() %>% 
  #bind_cols(year) %>% 
  rename(iceoff_doy_12 = 1) %>% 
  mutate(
    std_dev_12off = sd(iceoff_doy_12),
    sd_high_12off = iceoff_doy_12 + std_dev_12off,
    sd_low_12off = iceoff_doy_12 - std_dev_12off
  )

# 6. Combine ice safety data----

ice_safety_data_all <- iceon_safety_data %>% 
  bind_cols(av_iceoff_4_inch, av_iceoff_5_inch, av_iceoff_8_inch, av_iceoff_12_inch)

# 7. Create time series plots----

# Ice on plots
iceon_walk_plt <- ggplot(data = ice_safety_data_all)+
  # geom_ribbon(data=av_iceon_4_inch, 
  #             aes(x = ice_safety_data2$year, y = ice_safety_data2$iceon_doy_4, 
  #                 ymin=sd_neg, ymax=sd_pos), fill="grey", alpha=0.5)+
  # geom_ribbon(aes(x = year, y = iceon_doy_4, 
  #                 ymin=sd_low_4on, ymax=sd_high_4on), fill="grey50", alpha=0.8)+
  # geom_ribbon(aes(x = year, y = iceon_doy_5, 
  #                 ymin=sd_low_5on, ymax=sd_high_5on), fill="grey80", alpha=0.5)+
  # geom_line(mapping = aes(x = year, y = iceon_doy_5, color = 'White Ice'), linewidth = 1.4)+
  # geom_line(mapping = aes(x = year, y = iceon_doy_4, color = 'Black Ice'), linewidth = 1.4)+
  # scale_color_manual(values = c("black", "grey50"))+
  geom_ribbon(aes(x = year, y = iceon_doy_4, 
                  ymin=sd_low_4on, ymax=sd_high_4on), fill="#B32E5FFF", alpha=0.8)+
  geom_ribbon(aes(x = year, y = iceon_doy_5, 
                  ymin=sd_low_5on, ymax=sd_high_5on), fill="#FEEDB0FF", alpha=0.5)+
  geom_line(mapping = aes(x = year, y = iceon_doy_5, color = 'White Ice'), linewidth = 1.4)+
  geom_line(mapping = aes(x = year, y = iceon_doy_4, color = 'Black Ice'), linewidth = 1.4)+
  scale_color_manual(values = c("#2F0F3EFF", "#F9BE85FF"))+
  ylab('Safe Ice Period Start')+
  xlab('')+
  ylim(307, 382)+
  theme_classic()+
  theme(
    text = element_text(size = 20),
    axis.text.x=element_blank(),
    legend.position = c(0.2,0.9),
    legend.title = element_blank()
  )
iceon_walk_plt

iceon_hunt_plt <- ggplot(data = ice_safety_data_all)+
  geom_ribbon(aes(x = year, y = iceon_doy_8, 
                  ymin=sd_low_8on, ymax=sd_high_8on), fill="#B32E5FFF", alpha=0.8)+
  geom_ribbon(aes(x = year, y = iceon_doy_12, 
                  ymin=sd_low_12on, ymax=sd_high_12on), fill="#FEEDB0FF", alpha=0.5)+
  geom_line(mapping = aes(x = year, y = iceon_doy_12), color = '#F9BE85FF', linewidth = 1.4)+
  geom_line(mapping = aes(x = year, y = iceon_doy_8), color = '#2F0F3EFF', linewidth = 1.4)+
  ylab('')+
  xlab('')+
  ylim(307, 382)+
  theme_classic()+
  theme(
    text = element_text(size = 20),
    axis.text.x=element_blank(),
    axis.text.y=element_blank()
  )
iceon_hunt_plt

# Ice off plots
iceoff_walk_plt <- ggplot(data = ice_safety_data_all)+
  geom_ribbon(aes(x = year, y = iceon_doy_4, 
                  ymin=sd_low_4off, ymax=sd_high_4off), fill="#B32E5FFF", alpha=0.8)+
  geom_ribbon(aes(x = year, y = iceoff_doy_5, 
                  ymin=sd_low_5off, ymax=sd_high_5off), fill="#FEEDB0FF", alpha=0.5)+
  geom_line(mapping = aes(x = year, y = iceoff_doy_5), color = '#F9BE85FF', linewidth = 1.4)+
  geom_line(mapping = aes(x = year, y = iceoff_doy_4), color = '#2F0F3EFF', linewidth = 1.4)+
  ylab('Safe Ice Period End')+
  xlab('')+
  ylim(c(454, 492))+
  theme_classic()+
  theme(
    text = element_text(size = 20)
  )
iceoff_walk_plt

iceoff_hunt_plt <- ggplot(data = ice_safety_data_all)+
  geom_ribbon(aes(x = year, y = iceon_doy_8, 
                  ymin=sd_low_8off, ymax=sd_high_8off), fill="#B32E5FFF", alpha=0.8)+
  geom_ribbon(aes(x = year, y = iceoff_doy_12, 
                  ymin=sd_low_12off, ymax=sd_high_12off), fill="#FEEDB0FF", alpha=0.5)+
  geom_line(mapping = aes(x = year, y = iceoff_doy_12), color = '#F9BE85FF', linewidth = 1.4)+
  geom_line(mapping = aes(x = year, y = iceoff_doy_8), color = '#2F0F3EFF', linewidth = 1.4)+
  ylab('')+
  xlab('')+
  ylim(c(454, 492))+
  theme_classic()+
  theme(
    text = element_text(size = 20),
    axis.text.y = element_blank()
  )
iceoff_hunt_plt

# 8. Combine figures----
figure <- ggarrange(iceon_walk_plt, iceon_hunt_plt, 
                    iceoff_walk_plt, iceoff_hunt_plt,
                    labels = c("a", "b", "c", "d"),
                    ncol = 2, nrow = 2,
                    widths = c(1,0.95))
figure

#ggsave(here('results/final_plots/time_series_plt_2023.05.29.png'), dpi = 300, height = 10, width = 14, units = 'in')


#Code from Iestyn
# iceon <- ncvar_get(on_data, 'iceon')
# ithresh <- ncvar_get(on_data, 'threshold')
# iceon_4_inch <- iceon[,,,ithresh == 4]
# av_iceon_4_inch <- apply(iceon_4_inch, 3, mean, na.rm = TRUE)
# plot(av_iceon_4_inch, type = 'l')