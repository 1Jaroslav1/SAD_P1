library(tidyverse)
library(anytime)

eu_deposits <- read.csv("data/EU_Inflation_HICP_data.csv")
eu_deposits <- na.omit(eu_deposits)

# correct datatypes
eu_deposits <- eu_deposits %>%
  rename(
    Date = Period.Unit,
    EU = EU..changingcomposition.
  ) %>%
  mutate(
    Date = anytime::anydate(paste(Date, 1))
  ) %>%
  mutate(across(!Date, as.double))

