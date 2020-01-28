#Exercise 1
#Aamirah Botha
#Arranging data, summarising, median and mutating

library(readr)
laminaria <- read_delim("data/laminaria.csv", 
                        ";", escape_double = FALSE, trim_ws = TRUE)


head(laminaria)
tail(laminaria)
glimpse(laminaria)


laminaria %>%
  group_by(site) %>%
  summarise(max_stipe_mass=max(stipe_mass))
summarise(min(blade_length))
