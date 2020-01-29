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


# Very neat scripts
# Its always good to do more then just that done in class in order to explore the code a bit more
# Always add comments at the end of each line so that you and the collaborator understands the code
#Overall mark for day 1: 7/10
