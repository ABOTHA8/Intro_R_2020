# Homework Assignment due 10th February
# Aamirah Botha 3440074

#Question 1: d
#Question 2: b,c,d,e



library(tidyverse)

# Opening Datasets
data(BOD)
data( BJsales )
data(EuStockMarkets)
data(DNase)
data(Formaldehyde)
data(Orange)
data(UCBAdmissions)

#Loading libraries and murder data
library(dplyr) 
library(dslabs) 
data(murders)
 library(tidyverse) 
library(ggpubr)
# Question 3:
head(murders)
tail(murders)
glimpse(murders)

# The murders data set had 51 observations with 5 variables. It shows the  population number of 51 states in America and the total deaths that have been recorded per state.
#California is the state with the highest popuation of 37253956 people and the highest murders of 1257 people. Wisconsin has the lowest number in population size but not
# the lowest murders number.

murders %>% 
  summarise(max_population = max(population))

murders %>% 
  summarise(min_population = min(population))

murders <- mutate(murders_population_in_millions = population / 10^6 
murders

# Removing Florida from the data set 
murders %>% 
  filter(state!= "Florida")

# Creating new datset with no South region data, we are then left with 34 states from 51
no_south <- murders %>% 
  filter(region!="South")

# Creating a new datset displaying only the New York nd Texas data
selected_sites <- c("New York", "Texas")
NY_TX_murders<- murders %>% filter(state %in% selected_sites)

# Calculate the total population size of the South and west regions.
selected_regions <- c("South", "West")

total_population <- murders %>%  filter( region%in% selected_regions)
total_population %>% 
  summarise(sum(population))


# Creating data frame with the Northest region population

Northeast_population <- c("Northeast")
Northeast_population <- murders %>%  filter(region== "Northeast")%>% 
group_by(region) %>% select(population)


# Creating two plots of dataframes created above



murder_1<- ggplot(data = murders, aes(x = region, y = population, colour = population)) +
  geom_point() +
  geom_smooth(method = "lm") + 
  labs(x = "Regions", y = "Population size")
       
murder_1

murder_2<- ggplot(data = murders, aes(x = region, y = total, colour = "Total Deaths")) +
  geom_point() +
  geom_smooth(method = bw)
  labs(x = "Regions", y = "Total Deaths")

  murder_2
# Load library for ggarrange
library(ggpubr)
ggarrange(murder_1, murder_2,  
          ncol = 2, nrow = 1, 
          labels = c("A", "B"), 
          common.legend = FALSE) 

# Explain visible trends: the trend between the population size and the Total deaths are the same. I might have done something wrong?
# Compare with population size of the South and the West: The west has a higher population than the South,

murders_1a <-  
  filter( murders, total>20,<100)

murders_1a

murder1b <- murders %>%
 slice(10:24, 26)

murder1b

murders_tibble<- murders %>% 
  as_tibble(murders)

murders_tibble

murders_tibble %>% 
  group_by(region)



#Section 3
library(dplyr) 
library(dslabs)
data(heights) 



# Describe heights dataset: 500 males and females were taken and their heights recorded. The dataset shows height from highest to lowest.

glimpse(heights)
head(heights)
tail (heights) 


male_heights<- heights %>% 
 filter(sex == "Male")
 male_heights %>% summarise(average_heights = mean(height)) # 69.31475
 male_heights %>% summarise(std_heights= sd(height)) # 3.611023
 male_heights %>% summarise(min_height= min(height), median_height = median(height), max_height = max(height)) # min = 50, median =69, max=82.67717
 
female_heights<- heights %>% 
  filter(sex== "Female") 
 female_heights %>%  summarise(sd_height = sd(height), (average_height = mean(height))) # SD= 3.760656 Average= 64.93942
 female_heights %>% summarise(min_height= min(height), median_height = median(height), max_height = max(height)) # min = 51, median = 64.98031, max= 79

 # Section 4 
 
 x <‐ c( 1, 6, 21, 19 , NA, 73, NA) 
 y <‐ c(NA, NA, 3, NA, 13, 24, NA) 
 # a) 2 missing in X and 4 missing in Y
 # b)

 geo.mean <- function(x)(
  
 prod.x <- prod(x)
 n<- length(x)
gm<- prod.x ^ (1/n)
 return(gm)
  
  )

 y < data.frame(toothaches= c(NA, NA, 3, NA, 13, 24, NA)) 
 
 x
 
 # Section 5
 
 Seasonal_data <- data.frame(year = c(2015, 2016, 2017, 2018),           
                             winter = c(41, 39, 47, 40),            
                             spring = c(41, 46, 57, 45),           
                             summer = c(75, 52, 85, 66),           
                             Autumn = c(57, 66, 52, 56)) 
# Hypothesis: The species number is highest in summer and lowest in winter
 # Create two plots
 
 Winter_plot <- ggplot(data = Seasonal_data, aes(x = year, y = winter)) +
   geom_point() +
   geom_line() +
   labs(x = "Years", y = "Numbers")
 Winter_plot
 
 Summer_plot <- ggplot(data = Seasonal_data, aes(x = year, y = summer)) +
   geom_point() +
   geom_line() +
   labs(x = "Years", y = "Numbers")
 Summer_plot
 ggarrange(Winter_plot, Summer_plot, 
           ncol = 2, nrow = 1, # Set number of rows and columns
           labels = c("Winter", "Summer"), # Label each figure
           common.legend = TRUE) # Create common legend
  
  # Findings: The Winter plot shows that between the years 2015- 2018 the highest species number was recorded in 2017
 # thhe same appliesfor the Summer plot. The overall average is higher in summer than winter by almost 40 species more than in Winter 2017.
 
 cats_data<- tibble(cats = c("A", "B", "C"),              
                    position = c("1-2-3", "3-1-2", "2-3-1"),             
                    minutes = c(3, 3, 3), 
                    seconds = c(12, 44, 15))  
 cats_data
cats_2 <- cats_data %>% 
  separate (col = position, into = c("first_place", "second_place", "third_place"),sep = "/ ")

cats_2

cats_3 <- cats_data %>%   
  unite(minutes, seconds, col = "total_time", sep = "-") 
cats_3

# Section 6

data(breslow)

breslow_tidy1 <- breslow %>%
  gather(n,ns, key= n, value = "Person years")
#  Gathering the person years and smokers years into one column named person years, the smokers years and people years are the same.


breslow_tidy2 <- breslow_tidy1 %>% 
  spread(key = 'n', value = X1)

breslow_tidy2
# The spread fuction allows me to add a column that makes the data make more sense. in this case we separated the column to non smokers and smokers per age group.


breslow_tidy3<- breslow_tidy2 %>% 
separate(col = age, into = c("40-50", "70-80")),
mutate(40-50 = lubridate::"40-50"(age),
       70-80 = lubridate::"70:80"(age))

# separate column age into age groups 40-50 and 70-80
 breslow_tidy4 <- breslow_tidy3 %>% 
   unite( "40-50", "70-80", col = "Age_groups")
breslow_tidy4 
# Uniting the two columns previously made into one named Age_groups

breslow_tidy4 %>% 
  arrange(Age_groups, ns)
# arranging the database based on the variables

breslow_tidy5<- breslow_tidy4 %>% 
  select(Age_groups, smoke)
# selecting age groups and smoke as the main columns 

breslow_tidy5
