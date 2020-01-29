# Day 2 Exercise
# Manipulate Graphs through legends, Keys, Titles etc.
# Aamirah Botha
# 29 jANUARY 2020

# Load packages
library(tidyverse)
library(ggpubr)

box_1 <- ggplot(data = ChickLast, aes(x = Diet, y = weight)) +
  geom_boxplot(aes(fill = Diet)) +
  geom_smooth(method = "lm", size = 1.2)+
  labs(x = "Diet", y = "Final Mass(g)")

box_1

# Plotting a laminaria histogram on total length
histogram_2 <- ggplot(data = laminaria, aes( x= total_length))+
  geom_histogram(aes(fill= total_length), position= "dodge", binwidth = 100)+
  labs(x="Total length", y= "Number")
histogram_2

# Plotting a point graph on laminaria toal length vs site

Point_1 <- ggplot( data = laminaria, aes( x= site, y = total_length))+
  geom_point(shape = 22, colour = "pink", fill = "salmon")+
  geom_line(aes(group=site))+
  geom_smooth(method = "lm")+ # Tried adding a line of best fit, did'nt seem to work?
  title(main = list("Total length per site samples", cex = 1.5, col= "red", font = 3))+#Tried adding a title, not sure why it did not work?
 labs(x = "Site", y= "Total length per sample")+
  theme_linedraw()
 
Point_1


      

