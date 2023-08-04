library(tidyverse)

# Create plotting function and save plots

#below code taken from https://www.youtube.com/watch?v=OqcYTdSKNYg&t=495s
#The below code adjusts the lat and lon to fit on a map that uses negative
#values as west of 0 (i.e., -180 to 180 coordinates)

# 1. Map 1----------------------------------------------------------------------

mapCDFtemp <- function(lat, lon, idat, grid_size, limits, color_breaks, name_cmocean, direction, titletext, plot_margin) 
{
  
  #Create a dataframe to plot
  expand.grid(lon, lat) %>%
    rename(lon = Var1, lat = Var2) %>%
    mutate(lon = ifelse(lon > 180, -(360 - lon), lon),
           idat = as.vector(idat)) %>% 
    
    #Start plot, feeding in the previous dataframe from expand.grid
    ggplot()+
    geom_point(aes(x = lon, y = lat, color = idat), size = grid_size, shape = 'square')+
    scale_color_viridis(name = 'Ice Duration Anomaly', 
                        na.value = 'transparent', 
                        option = 'A', 
                        limits = limits, 
                        direction = direction)+#,
                        #values = c(0, scales::rescale(color_breaks, from = range(idat, na.rm = T)), 1))+
    #scale_color_cmocean(name=name_cmocean, direction = direction, limits = limits, na.value = 'transparent')+
    
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
      legend.key.height = unit(1.5, 'cm'),
      plot.margin=plot_margin
    )+
    
    #Map settings, adjusting the lat/long and projection
    coord_map(xlim = c(-170, 170), ylim = c(39, 75), projection = 'orthographic')+ 
    #coord_sf(xlim = c(-100, 100), ylim = c(40, 85))+
    
    #Title text etc.
    ggtitle(titletext)+
    xlab('')+
    ylab('')

}

# 2. Map 2----------------------------------------------------------------------

mapCDFtemp2 <- function(lat, lon, idat, grid_size, limits, name_cmocean, direction) 
{
  x_lines <- seq(-120,180, by = 60)
  world_map <- map_data("world")
  
  expand.grid(lon, lat) %>%
    rename(lon = Var1, lat = Var2) %>%
    mutate(lon = ifelse(lon > 180, -(360 - lon), lon),
           idat = as.vector(idat)) %>% 
    
    ggplot()+
    #Add map base layer
    geom_polygon(data = world_map, aes(x = long, y = lat, group = group), 
                 fill = "grey", colour = "black", alpha = 0.8) +
    
    #Add data
    geom_point(aes(x = lon, y = lat, color = idat), size = grid_size, shape = 'square')+
    scale_color_viridis(name = 'Ice Duration Anomaly', na.value = 'transparent', 
                        option = 'A', limits = limits, direction = direction)+
    
    # Convert to polar coordinates
    coord_map("ortho", orientation = c(90, 0, 0)) +
    scale_y_continuous(breaks = seq(45, 180, by = 5), labels = NULL) +
    
    # Removes Axes and labels
    scale_x_continuous(breaks = NULL) +
    
    # Change theme to remove axes and ticks
    theme(panel.background = element_blank(),
          panel.grid.major = element_line(size = 0.25, linetype = 'dashed',
                                          colour = "black"),
          axis.ticks=element_blank())+
    xlab("") + 
    ylab("") +
    
    # Adds axes
    # geom_hline(aes(yintercept = 180), linewidth = 1)  +
    # geom_segment(aes(y = 45, yend = 180, x = x_lines, xend = x_lines), linetype = "dashed")+
    
    # Adds labels
    # geom_label(aes(x = 180, y = seq(40, 85, by = 10), 
    #                label = paste0(seq(40, 85, by = 10), "°N")),size = 2) +
    # geom_label(aes(x = x_lines, y = 20, 
    #                label = c("120°W", "60°W", "0°", "60°E", "120°E", "180°W"))
    #                ,size = 2, color = "white",fill = "black")+
    #guides(colour = guide_legend(override.aes = list(size=5)))+
    # labs(color='DOY', caption ="Year:2099", 
    #      title = "Climate Scenario", subtitle = "SSP1-2.6" )
    
  #Change text elements
    theme(
      title = element_text(size = 30),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      legend.text = element_text(size = 20),
      #legend.title = element_text(size = 20),
      
      legend.title = element_blank(),
      
      #legend.direction = 'horizontal', 
      legend.position = 'bottom',
      #legend.justification = 'left',
      legend.key.width = unit(1.5, 'cm'),
      legend.key.height = unit(0.5, 'cm')
    )
}



# mapCDFtemp2 <- function(lat, lon, idat, limits, name_cmocean, direction, titletext) 
# {
#   #titletext <- ""
#   
#   expand.grid(lon, lat) %>%
#     rename(lon = Var1, lat = Var2) %>%
#     mutate(lon = ifelse(lon > 180, -(360 - lon), lon),
#            idat = as.vector(idat)) %>% 
#     
#     ggplot()+
#     geom_point(aes(x = lon, y = lat, color = idat), size = 5, shape = 'square')+
#     scale_color_viridis(name = 'Ice Duration Anomaly', na.value = 'transparent', option = 'A', limits = limits, direction = direction)+
#     # scale_color_gradient2(low="darkred", mid="white", high="darkgreen", #colors in the scale
#     #   midpoint=0,    #same midpoint for plots (mean of the range)
#     #   breaks=seq(-55,10,10), #breaks in the scale bar
#     #   limits=c(min(idat, na.rm = TRUE), max(idat, na.rm = TRUE)))+ #same limits for plots
#     #scale_color_cmocean(name=name_cmocean, direction = direction, limits = limits)+
#     # geom_point(aes(x = lon, y = lat, color = idat2), size = 2.5, shape = 'square')+
#     # scale_color_manual(values = 'black')+
#     #geom_point(aes(x = lon, y = lat, color = idat3), size = 2.5, shape = 'square')+
#     borders('world', colour = 'black', fill = 'NA')+ #xlim = c(-180, 180), ylim = c(40, 85), 
#     
#     
#     theme(
#       title = element_text(size = 30),
#       axis.text = element_blank(),
#       axis.ticks = element_blank(),
#       legend.text = element_text(size = 20),
#       #legend.title = element_text(size = 20),
#       
#       legend.title = element_blank(),
#       
#       #legend.direction = 'horizontal', 
#       legend.position = 'bottom',
#       #legend.justification = 'left',
#       legend.key.width = unit(1.5, 'cm'),
#       legend.key.height = unit(0.5, 'cm')
#     )+
#     #coord_quickmap(xlim = c(-170, 170), ylim = c(39, 75))+
#     coord_map(xlim = c(-170, -40), ylim = c(39, 75), projection = 'gilbert')+ 
#     #coord_sf(xlim = c(-100, 100), ylim = c(40, 85))+
#     ggtitle(titletext)+
#     xlab('')+
#     ylab('')
# }
# 
