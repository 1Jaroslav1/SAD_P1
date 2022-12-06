source("Functions.R")
source("Utils.R")
library(dplyr)
library(forecast)

draw_barplots <- function(df, title, column_name, shape1, shape2) {
  ggplot(df, aes(x = Date, y = get(column_name))) +
    geom_bar(stat = "identity", position = "dodge") +
    xlab("Country") +
    ylab("Delta Infl") +
    ggtitle(title) +
    theme(
      plot.title = element_text(size = 18)
    ) +
    theme_minimal() +
    stat_function(fun = function(x) dbeta(as.numeric(x - date_min) / as.numeric(date_diff), shape1, shape2), col = 'red')
}

countries <- c("Poland", "France", "Romania")
time_range <- c("2018-01-01", "2022-01-01")
inflation_delta <- read_eu_inflation() %>%
  filter_by_time(time_range) %>%
  select(
    Date, all_of(countries)
  ) %>%
  arrange(Date) %>%
  mutate(
    PolandMov = Poland - lag(Poland, 1),
    FranceMov = France - lag(France, 1),
    RomaniaMov = Romania - lag(Romania, 1)
  ) %>%
  na.omit

date_min <- min(inflation_delta$Date)
date_max <- max(inflation_delta$Date)

date_diff <- date_max - date_min

draw_barplots(inflation_delta, "Inflation delta for Poland", "Poland", 20, 0.9)
draw_barplots(inflation_delta, "Inflation delta for Romania", "Romania", 0.5, 0.5)
draw_barplots(inflation_delta, "Inflation delta for France", "France", 0.5, 0.5)

