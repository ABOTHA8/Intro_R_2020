# Biostats Day 1
# Aamirah Botha
# 19 February 2020

# Load library packages

library(tidyverse)

chicks <- as_tibble(ChickWeight)
head(chicks) # shows the first 6 rows in the datset
tail(chicks) # shows the last 6 rows in the dataset

tail(chicks, n=2) # specifying that we want to only look at 2 rows

colnames(chicks) # shows the names of the columns or variables in the dataset

summary(chicks) # gives a summmary of the data through showing the minimum,median and mean of the variable

dat1 <- c(23,45,23,66,13) # creating new values
mean(dat1) # calculating mean o =f the new dataset created previously

chicks %>% 
  summarise(mean_wt = mean(weight)) 

chicks %>%  summarise(mean_wt= round (mean(weight),1))

dat1 <- c(NA, 12, 76,34,23)

# Omitting the missing data

mean(dat1, na.rm = TRUE) # removes NA values

chicks %>%
  filter(Time == 21) %>% 
  ggplot(aes(x = Diet, y = weight)) +
  geom_boxplot(fill = "salmon") +
  geom_jitter(width = 0.05, fill = "white", col = "blue", shape = 21)
