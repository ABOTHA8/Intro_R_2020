# Summary stats
# 29th January 2020
# Aamirah Botha
##################

#Loading packages - always add headings
library(tidyverse)

#Loading in the data

laminaria <- read_csv("data/laminaria.csv")

laminaria %>% #Chose the dataframe
  summarise((avg_bld_wdt= mean(blade_length))) #Calculate the mean blade length

laminaria %>% # Tell R that we want to use the 'laminaria' dataframe
  summarise(avg_stp_ln = mean(total_length), # Create a summary of the mean of the total lengths
            sd_stp_ln = sd(total_length)) # Create a summary of the sd of the total lengths

laminaria %>% # Tell R that we want to use the 'laminaria' dataframe
  group_by(site) %>% # dont forget to add the piping to connect the functions
  summarise(avg_stp_ln = mean(total_length), # Create a summary of the mean of the total lengths
            sd_stp_ln = sd(total_length), # Create a summary of the sd of the total lengths
            var_stp_ln= var(total_length), 
            med_stp_ln= median(total_length))

## Plotting - Function ggplot

ggplot(data = laminaria, aes(x = stipe_mass, y = stipe_length)) +
  geom_point(shape = 5, colour = "blue", fill = "white") + # geom_pont, line etc. for the type of graph
  labs(x = "Stipe mass (kg)", y = "Stipe length (cm)") # labs = Labels

# Plotting

ChickWeight <- datasets:: ChickWeight #no need to load dataset again as it was loaded earlier

# Create a basic figure
ggplot(data = ChickWeight, aes(x = Time, y = weight)) + # inoutting chickweight data
  geom_point(shape = 2, colour = "pink", fill = "pink") + # Creating point graoh (scatter)
  geom_line(aes(group = Chick))+ # links scatter plot with line per chick
  labs(x= "Time (hours)", y = "Weight (kgs)") # Plotting the Chicks diet by weight and Time

ggplot(data = ChickWeight, aes(x = Time, y = weight, colour = Diet)) + # Colour = diet creates a key for the different diets
  geom_point() + # can add anything her to personalise it
  geom_line(aes(group = Chick))

ggplot(data = ChickWeight, aes(x = Time, y = weight, colour = Diet)) +
  geom_point() +
  geom_smooth(method = "lm")            
theme_bw()

ggplot(data = ChickWeight, aes(x = Time, y = weight, colour = Diet)) +
  geom_point(aes(size = weight)) + # creates a key and makes points size relative to weight
  geom_smooth(method = "lm", size = 1.2)+ # size = thickness of line
  theme(legend.position = "bottom") # Change the legend position

# Faceting
library(tidyverse)
library(ggpubr)

# Create faceted figure
ggplot(data = ChickWeight, aes(x = Time, y = weight, colour = Diet)) +
  geom_point() +
  geom_smooth(method = "lm") + # Note the `+` sign here
  facet_wrap(~Diet, ncol = 2) + # This is the line that creates the facets
  labs(x = "Days", y = "Mass (g)")
       

ChickLast <- ChickWeight %>% # Changes the name ChickWeight to ChickLast
  filter(Time == 21) # doube equal to specify one value within the time column (makes data smaller and easier to plot)


line_1 <- ggplot(data = ChickWeight, aes(x = Time, y = weight, colour = Diet)) +
  geom_point() +
  geom_line(aes(group = Chick)) +
  labs(x = "Days", y = "Mass (g)")
line_1

lm_1 <- ggplot(data = ChickWeight, aes(x = Time, y = weight, colour = Diet)) +
  geom_point() +
  geom_smooth(method = "gam") +
  labs(x = "Days", y = "Mass (g)") # Creating object to save in environment
lm_1 # control enter to view graph, cannot view if not in environment


# Note that we are using 'ChickLast', not 'ChickWeight'
histogram_1 <- ggplot(data = ChickLast, aes(x = weight)) +
  geom_histogram(aes(fill = Diet), position = "dodge", binwidth = 100) +
  labs(x = "Final Mass (g)", y = "Count")
histogram_1

box_1 <- ggplot(data = ChickLast, aes(x = Diet, y = weight)) +
  geom_boxplot(aes(fill = Diet)) +
  labs(x = "Diet", y = "Final Mass (g)")
box_1

ggarrange(line_1, lm_1, histogram_1, box_1, 
          ncol = 2, nrow = 2, # Set number of rows and columns
          labels = c("A", "B", "C", "D"), # Label each figure
          common.legend = TRUE) # Create common legend
