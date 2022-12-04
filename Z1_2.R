source("Utils.R")
source("DepositDataPreparation.R")
source("Functions.R")

# helper functions
inflation_vs_deposits_plot <- function(country_name, deposits, inflation, filter_range, plot_title) {
  inflation_for_country <- inflation %>%
    get_for_country_in_time_range(filter_range, country_name) %>%
    mutate(
      Metric = "Inflation"
    )

  deposits_for_country <- deposits %>% get_for_country_in_time_range(filter_range, country_name)
  mutate(
    Metric = "Deposits"
  )
  combined <- union_all(inflation_for_country, deposits_for_country)
  draw_stacked_metrics(combined, paste(country_name, ": ", plot_title))
}

statistics_for_all_countries <- function(df, filter_range) {
  df %>%
    filterByTime(filter_range) %>%
    pivot_longer(-Date, names_to = "Country", values_to = "Values") %>%
    na.omit() %>%
    group_by(Country) %>%
    summarise(
      mean = mean(Values),
      var = var(Values),
      median = median(Values)
    )
}

aggregated_deposit_and_inflation_barplot <- function(deposits, inflation, filter_range) {
  deposit_stats <- statistics_for_all_countries(deposits, filter_range)
  inflation_stats <- statistics_for_all_countries(inflation, filter_range)

  combined_mean <- union_all(filter_by_metric(inflation_stats, "mean", "Inflation"),
                             filter_by_metric(deposit_stats, "mean", "Deposits"))
  draw_aggregated_barplots(combined_mean,
                           paste("For time range ", filter_range[1], "-", filter_range[2]),
                           "Mean")
}


eu_inflation <- read_eu_inflation() %>% mutate(EU = NULL)
eu_deposits <- read_eu_depositis()
countries <- c("Poland", "Germany", "France", "Greece", "Czech.Republic")
filter_ranges <- list(c(analysis_start, pandemic_start, "Przed pandemi1"),
                      c(pandemic_start, during_pandemic, "Szczyt pandemii"),
                      c(during_pandemic, war_start, "Tuz przed wojna"),
                      c(war_start, now, "Od poczatku wojny"))

for (filter_range in filter_ranges) {
  title <- filter_range[3]
  for (country in countries) {
    inflation_vs_deposits_plot(country, eu_deposits, eu_inflation, head(filter_range, 2), title)
  }
  aggregated_deposit_and_inflation_barplot(eu_deposits, eu_inflation, filter_range)
}