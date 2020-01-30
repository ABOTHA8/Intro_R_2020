# Ggplot
# Mapping on Day 3
# Aamirah Botha
# 30 January 2020

# lOading packagaes

 library(tidyverse)  # Activating the packages
 library(boot)       # Library Func. activates the packages

 # Plotting with boot 
 urine <- boot::urine
 view (urine)

 ggplot(data = urine, aes(x = osmo, y = ph)) +
   geom_point(aes(colour = cond))+
   labs(x = "Osmoregulation", y = "pH")

 #### Mapping in R 
 
 # Loading in libraries
 library(tidyverse)
library(ggpubr) 

 # Load data
 load("data/south_africa_coast.RData")
 load("data/sa_provinces.RData")
 load("data/rast_annual.RData")
# load("data/MUR.RData")
 load("data/MUR_low_res.RData")
 sst <- MUR_low_res
 
 # The colour pallette we will use for ocean temperature
 cols11 <- c("#004dcd", "#0068db", "#007ddb", "#008dcf", "#009bbc",
             "#00a7a9", "#1bb298", "#6cba8f", "#9ac290", "#bec99a")

 ggplot(data = south_africa_coast, aes(x = lon, y = lat)) +
   geom_point() # to create a ont graph version of a map.

 ggplot(data = south_africa_coast, aes(x = lon, y = lat)) +
   geom_polygon(colour = "black", fill = "red", aes(group = group)) # The land mask
 #geom_polygon is used to vreate a polygon of an area. Looks more like a map.
 
 ggplot(data = south_africa_coast, aes(x = lon, y = lat)) +
   geom_polygon(colour = "black", fill = "grey70", aes(group = group)) +
   geom_path(data = sa_provinces, aes(group = group)) # The province borders
 
 ggplot(data = south_africa_coast, aes(x = lon, y = lat)) +
   geom_polygon(colour = "black", fill = "grey70", aes(group = group)) +
   geom_path(data = sa_provinces, aes(group = group)) + 
   coord_equal(xlim = c(15, 34), ylim = c(-36, -26), expand =0) # Force lon/lat extent

 ggplot(data = south_africa_coast, aes(x = lon, y = lat)) +
   geom_raster(data = sst, aes(fill = bins)) + # The ocean temperatures
   geom_polygon(colour = "black", fill = "grey70", aes(group = group)) +
   geom_path(data = sa_provinces, aes(group = group)) +
   coord_equal(xlim = c(15, 34), ylim = c(-36, -26), expand = 0)
 
 ggplot(data = south_africa_coast, aes(x = lon, y = lat)) +
   geom_raster(data = sst, aes(fill = bins)) +
   geom_polygon(colour = "black", fill = "grey70", aes(group = group)) +
   geom_path(data = sa_provinces, aes(group = group)) +
   scale_fill_manual("Temp. (°C)", values = cols11) + # Set the colour palette
   coord_equal(xlim = c(15, 34), ylim = c(-36, -26), expand = 0)
 
 final_map <-ggplot(data = south_africa_coast, aes(x = lon, y = lat)) +
   geom_raster(data = sst, aes(fill = bins)) +
   geom_polygon(colour = "black", fill = "grey70", aes(group = group)) +
   geom_path(data = sa_provinces, aes(group = group))  +
   geom_tile(data = rast_annual, aes(x = lon, y = lat, fill = bins), 
             colour = "white", size = 0.1) +
   scale_fill_manual("Temp. (°C)", values = cols11) +
   coord_equal(xlim = c(15, 34), ylim = c(-36, -26), expand = 0) +
   scale_x_continuous(position = "top") + # Put x axis labels on top of figure
   theme(axis.title = element_blank(), # Remove the axis labels
         legend.text = element_text(size = 7), # Change text size in legend
         legend.title = element_text(size = 7), # Change legend title text size
         legend.key.height = unit(0.3, "cm"), # Change size of legend
         legend.background = element_rect(colour = "white"), # Add legend background
         legend.justification = c(1, 0), # Change position of legend
         legend.position = c(0.55, 0.4) # Fine tune position of legend
   )
 final_map

 # Load libraries
 library(tidyverse)
 library(scales)
 library(ggsn)
 library(maps)
 
 
 # Load Africa map
 load("data/africa_map.RData")
 
 ggplot() +
   borders() + # The global shape file
   coord_equal(xlim = c(0, 50), ylim = c(0, -50)) # Equal sizing for lon/lat

 sa_1 <- ggplot() +
   borders(fill = "grey70", colour = "black") +
   coord_equal(xlim = c(12, 36), ylim = c(-38, -22), expand = 0) # Force lon/lat extent
 sa_1

 sa_2 <- sa_1 +
   annotate("text", label = "Atlantic\nOcean", 
            x = 15.1, y = -32.0, 
            size = 5.0, 
            angle = 30, 
            colour = "navy") +
   annotate("text", label = "Indian\nOcean", 
            x = 33.2, y = -34.2, 
            size = 5.0, 
            angle = 330, 
            colour = "springgreen")
 sa_2               

 sa_3 <- sa_2 +
   scalebar (x.min = 22, x.max = 26, y.min = -36, y.max = -35, # Set location of bar
           dist = 200, height = 1, st.dist = 0.8, st.size = 4, # Set particulars
          transform = TRUE, model = "WGS84") + # Set appearance
   north(x.min = 22.5, x.max = 25.5, y.min = -33, y.max = -31, # Set location of symbol
         scale = 1.2, symbol = 16)
 sa_3 