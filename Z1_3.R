source("Functions.R") #SAD sprawko - jak ma wyglądać
source("Utils.R")
library(dplyr)
library(forecast)

draw_histogram <- function(df, title, column_name, mean, sd) {
  ggplot(df) +
    geom_histogram(binwidth=0.1, aes(x = get(column_name), y = after_stat(density))) +
    xlab("Inflation delta") +
    ggtitle(title) +
    theme(
      plot.title = element_text(size = 18)
    ) +
    xlim(-0.5,10) +
    ylim(0,1) +
    theme_minimal() +
    stat_function(fun = dnorm, args = list(mean=mean, sd=sd))
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

draw_histogram(inflation_delta, "Inflation delta for Poland", "Poland", mean(inflation_delta$Poland), sd(inflation_delta$Poland))
draw_histogram(inflation_delta, "Inflation delta for Romania", "Romania",  mean(inflation_delta$Romania), sd(inflation_delta$Romania))
draw_histogram(inflation_delta, "Inflation delta for France", "France",  mean(inflation_delta$France), sd(inflation_delta$France))
