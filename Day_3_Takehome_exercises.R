# Day 2 Exercise
# Manipulate Graphs through legends, Keys, Titles etc.
# Aamirah Botha
# 29 jANUARY 2020

# Load packages
library(tidyverse)
library(ggpubr)

box_1 <- ggplot(data = mean_laminaria_1, aes(x = site, y = sd_stipe_length)) +
  geom_boxplot(aes(fill = sd_stipe_length)) +
  geom_smooth(method = "lm", size = 1.2)+
  labs(x = "Site", y = "Stipe length (cm)")

box_1

# Plotting a laminaria histogram on total length
histogram_2 <- ggplot(data = laminaria, aes( x= total_length))+
  geom_histogram(aes(fill= total_length, col= 5), position= "dodge", binwidth = 100)+
  labs(x="Total length", y= "Number",title ="Total length vs number")+
  
histogram_2

# Plotting a point graph on laminaria total length vs site

Point_1 <- ggplot( data = laminaria, aes( x= site, y = total_length))+
  geom_point(shape = 22, colour = "pink", fill = "salmon")+
  geom_line(aes(group=site))+
  geom_smooth(method = "lm")+ # Tried adding a line of best fit, did'nt seem to work?
 labs(x = "Site", y= "Total length per sample",title = "Total length per site samples")+
  theme_linedraw()
 
Point_1

# Summarising Laminaria dataset into mean, sd, variant and median

laminaria <- read_csv("data/laminaria.csv")

laminaria %>% #Chose the dataframe
  summarise((avg_stipe_length= mean(stipe_length))) #Calculate the mean stipe length
      

mean_laminaria_1<- laminaria %>% # Tell R that we want to use the 'laminaria' dataframe
  group_by(site) %>% # dont forget to add the piping to connect the functions
  summarise(avg_stipe_length= mean(mean(stipe_length)), # Create a summary of the mean of the total lengths
            sd_stipe_length = sd(stipe_length), # Create a summary of the sd of the total lengths
            var_stipe_length= var(stipe_length), 
            med_stipe_length= median(stipe_length))
mean_laminaria_1

## Plotting - Function ggplot

POint_2 <- ggplot(data = mean_laminaria_1, aes(x = site, y = avg_stipe_length)) +
  geom_point( shape = 6, colour = "purple", fill = "purple") +# geom_pont, line etc. for the type of graph
  geom_smooth(method = "lm", size = 1.2)+
  labs(x = "Sites", y = "Stipe length (cm)")+ # labs = Labels
scale_colour_gradientn(cols33) # tried adding colour palette,did not work.
POint_2

Col_1 <- ggplot(data = mean_laminaria_1, aes(x = site, y = avg_stipe_length)) +
  geom_col( binwidth = 50, colour = "purple", fill = "purple") +# geom_pont, line etc. for the type of graph
  geom_smooth(method = "lm", size = 1.2)+
  labs(x = "Sites", y = "Stipe length (cm)", title = "Average stipe length per site")
Col_1

# Faceting
library(tidyverse)
library(ggpubr)

Facet_1 <- ggarrange(box_1, Col_1, histogram_2, POint_2, 
          ncol = 2, nrow = 2, # Set number of rows and columns
          labels = c("A", "B", "C", "D"), # Label each figure
          common.legend = TRUE) # Create common legend
Facet_1

# Mapping- Cutting map 

# Load libraries
library(tidyverse)
library(scales)
library(ggsn)
library(maps)

# Load Africa map
load("data/africa_map.RData")
 South_America <- ggplot() +
  borders() + # The global shape file
  coord_equal(xlim = c(-75, -25), ylim = c(-60, 15), expand = 0) + # Equal sizing for lon/lat
  borders(fill = "blue", colour = "black")+
  annotate ("text", label = "South America", 
           x = -50, y = -20, colour = "green", size = 7)
   South_America       
        
        #creating a colour palette
        cols33 <- c("#5CB6C9","#77B6D7","#96B5E1","#B5B2E5","3D4AEE3,F1AADC")
            