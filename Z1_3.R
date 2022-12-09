source("Functions.R") #SAD sprawko - jak ma wyglądać
source("Utils.R")
library(dplyr)
library(forecast)
library(svglite)

draw_histogram <- function(df, title, column_name, mean, sd) {
  ggplot(df) +
    geom_histogram(binwidth=0.1, aes(x = get(column_name), y = after_stat(density))) +
    xlab("Inflation delta") +
    ggtitle(title) +
    theme(
      plot.title = element_text(size = 18)
    ) +
    xlim(-0.5,9) +
    ylim(0,1) +
    theme_minimal() +
    stat_function(fun = dnorm, args = list(mean=mean, sd=sd), col ='red')
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

poland_plot <- draw_histogram(inflation_delta, "Inflation delta for Poland", "Poland", mean(inflation_delta$Poland), sd(inflation_delta$Poland))
romania_plot <- draw_histogram(inflation_delta, "Inflation delta for Romania", "Romania",  mean(inflation_delta$Romania), sd(inflation_delta$Romania))
france_plot <- draw_histogram(inflation_delta, "Inflation delta for France", "France",  mean(inflation_delta$France), sd(inflation_delta$France))

ggsave(file="plots/inflation_delta_poland.eps", plot=poland_plot, width=10, height=8)
ggsave(file="plots/inflation_delta_france.eps", plot=france_plot, width=10, height=8)
ggsave(file="plots/inflation_delta_romania.eps", plot=romania_plot, width=10, height=8)
#todo beter fit