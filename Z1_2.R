source("Functions.R")
source("InflationDataPreparing.R")
source("DepositDataPreparation.R")

country <- "Poland"
inflation_data <- getDataFrameByCountry(eu_inflation_tidy, country) %>%
  rename(
    Date = date,
    Metric = Inflation
  ) %>%
  select(Date, Metric) %>% mutate(
    series = "Inflation"
)

deposits_data <- eu_deposits %>%
  select(Date, all_of(country)) %>%
  rename(
    Metric = country
  ) %>% mutate(
    series = "Deposits"
)
combined <- union_all(inflation_data, deposits_data)


drawMultipleLines(combined, "Date")

a <- "a"