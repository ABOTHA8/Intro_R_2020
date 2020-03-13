# Aamirah Botha 3440074
# IntroRWorkshop:Exercise2


library(tidyverse)
library(lubridate)
library(ggpubr)
library(dplyr)


# Question 1 --------------------------------------------------------------


#load data SACTNMONTHLY_V4.0


SACTN_PLOT <- SACTNmonthly_v4.0


SACTNmonthly_tidy <- SACTN_PLOT %>%
  mutate(year= year(date)) %>%
  group_by(site, year) %>%
  filter(src == "KZNSB") %>% 
  summarise(mean_temp=mean(temp))



KZNSB_plot<- ggplot(data = SACTNmonthly_tidy, aes(x = year, y = mean_temp)) +
  geom_line(aes(group = site, colour = "pink")) +
  facet_wrap(~site, ncol = 5) +
  labs(x = "Year", y = "Temperature (C)", title="KZNSB: series of annual means") +
  scale_y_continuous(breaks = seq(20, 24, 2)) +
  scale_x_discrete(breaks = seq(1980, 2014, 20))

KZNSB_plot



# Question 2 --------------------------------------------------------------


#load data laminaria

falsebay<- laminaria
falsebay_1 <- falsebay%>% 
  filter(site %in% c("A-Frame", "Baboon Rock", "Batsata Rock", "Betty's Bay", "Bordjiestif North", "Buffels", "Buffels South","Miller's Point", "Roman Rock"))

plot_1 <- ggplot(falsebay_1, aes(x = blade_length, y = blade_weight)) 
  geom_line(aes(colour = site), size = 1) +
  geom_point(aes(colour = site), size = 2) +
  facet_wrap(~site, ncol = 3) +
  labs(x = "Blade length (cm)", y = "Blade mass (kg)") +
  scale_colour_brewer(palette = "Accent") +#colour palette not enough colours for sites.
  ggtitle("A crazy graph of some data for False Bay sites")

plot2 <- ggplot(falsebay_1, aes(x = blade_length, y = blade_weight)) +
  geom_line(aes(colour = site), size = 1) +
  geom_point(aes(colour = site), size = 2) +
  facet_wrap(~site, ncol = 3) +
  labs(x = "Blade length (cm)", y = "Blade mass (kg)") +
  scale_colour_brewer(palette = "Set1") +
  ggtitle("A crazy graph of some data for False Bay sites")

False_Bay_plot<- ggarrange(plot_1,plot2, 
          ncol = 2, nrow = 2, 
          labels = c("A", "B"), 
          common.legend = FALSE,
          legend = "none")


# Question 3 --------------------------------------------------------------


tooth <- datasets::ToothGrowth
datasets::ToothGrowth#loaded dataset toothgrowth

Mean_sd_tooth <- ToothGrowth%>% 
  group_by(supp, dose) %>%
  summarise(mean_length = mean(len),#summarised mean length and standard deviation length
            sd_length = sd(len))


teeth_1 <- ToothGrowth

teeth_1 <- ggplot(Mean_sd_tooth, aes(x = dose, y = mean_length, fill = supp)) +
  geom_col(aes(fill = supp), position = "dodge", colour = "black") + 
  geom_errorbar(aes(ymin = mean_length - sd_length,
                    ymax = mean_length + sd_length), 
                position = "dodge") +
  labs(x = "Dose (mg/d)", y = "Tooth length (mm)") +
  ggtitle("Dosage of supplements in relation to tooth growth")
