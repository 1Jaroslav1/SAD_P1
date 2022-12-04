source("Utils.R")
source("DepositDataPreparation.R")
source("Functions.R")


eu_inflation <- read_eu_inflation()
eu_deposits <- read_eu_depositis()

draw_metrics <- function(df, title) {
  ggplot(df, aes(x = Date, y = Value, group = Metric,
                 color = Metric)) +
    geom_point() +
    geom_line(aes(color = Metric)) +
    scale_x_date(date_labels = "%Y-%m", date_minor_breaks = "1 month") +
    guides(fill = guide_legend(title = NULL)) +
    xlab("Date") +
    ylab("Value [%]") +
    ggtitle(title) +
    theme(
      plot.title = element_text(size = 18)
    )
}
analyzeCountry <- function(country_name, deposits, inflation, filter_range, plot_title) {
  inflation_for_country <- inflation %>%
    select(Date, all_of(country_name)) %>%
    filterByTime(filter_range) %>%
    rename(
      Value = all_of(country_name)
    ) %>%
    mutate(
      Metric = "Inflation"
    )

  deposits_for_country <- deposits %>%
    select(Date, all_of(country_name)) %>%
    filterByTime(filter_range) %>%
    rename(
      Value = all_of(country_name)
    ) %>%
    mutate(
      Metric = "Deposits"
    ) %>% na.omit()
  combined <- union_all(inflation_for_country, deposits_for_country)
  draw_metrics(combined,paste(country_name, ": ", plot_title))
}

filter_range <- c(analysis_start, pandemic_start)
title <- "Przed pandemia"
analyzeCountry("Poland", eu_deposits, eu_inflation, filter_range, title)
analyzeCountry("Germany", eu_deposits, eu_inflation, filter_range, title)
analyzeCountry("Czech.Republic", eu_deposits, eu_inflation, filter_range, title)

filter_range <- c(pandemic_start, during_pandemic)
title <- "Szczyt pandemii"
analyzeCountry("Poland", eu_deposits, eu_inflation, filter_range, title)
analyzeCountry("Germany", eu_deposits, eu_inflation, filter_range, title)
analyzeCountry("Czech.Republic", eu_deposits, eu_inflation, filter_range, title)

filter_range <- c(during_pandemic, war_start)
title <- "Tuż przed wojną"
analyzeCountry("Poland", eu_deposits, eu_inflation, filter_range, title)
analyzeCountry("Germany", eu_deposits, eu_inflation, filter_range, title)
analyzeCountry("Czech.Republic", eu_deposits, eu_inflation, filter_range, title)

filter_range <- c(war_start, now)
title <- "Od początku wojny"
analyzeCountry("Poland", eu_deposits, eu_inflation, filter_range, title)
analyzeCountry("Germany", eu_deposits, eu_inflation, filter_range, title)
analyzeCountry("Czech.Republic", eu_deposits, eu_inflation, filter_range, title)