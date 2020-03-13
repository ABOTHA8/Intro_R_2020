# Tidy Data
# 10th February 2020
# Aamirah Botha

# Libraries
library(tidyverse)

load("data/SACTN_mangled.RData")
SACTN2_tidy <- SACTN2 %>%
  gather(DEA, KZNSB, SAWS, key = "src", value = "temp") # Key names the column
