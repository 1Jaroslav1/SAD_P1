library(tidyverse)
library(anytime)

eu_deposits <- read.csv("data/EU_Inflation_HICP_data.csv")
eu_deposits <- na.omit(eu_deposits)

# correct datatypes
eu_deposits <- eu_deposits %>%
  rename(
    Period = Period.Unit,
    EU = EU..changingcomposition.
  ) %>%
  mutate(
    Period = anytime::anydate(paste(Period, 1))
  ) %>%
  mutate(across(!Period, as.double))

deposits_tidy <- pivot_longer(eu_deposits, -Period, names_to = "Country", values_to = "Deposits")