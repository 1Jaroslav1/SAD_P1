source("Functions.R")
source("Utils.R")
library(dplyr)

draw_histogram <- function(df, title, column_name, mean, sd, color) {
  cat(column_name, ": ", "mean - ", mean, ', standard deviation = ', sd, '\n')
  ggplot(df) +
    geom_histogram(binwidth = 0.1, aes(x = get(column_name), y = after_stat(density)), fill = paste(color, '1'), color = color) +
    xlab("Monthly inflation delta") +
    ylab("Density") +
    theme_light() +
    ggtitle(title) +
    theme(
      plot.title=element_text(size = 25, hjust=0.5, vjust=0.5, face='bold', margin = margin(20, 0, 20, 0)),
      axis.title = element_text(size = 20),
      axis.title.x = element_text(margin = margin(20, 0, 20, 0)),
      axis.title.y = element_text(margin = margin(0, 20, 0, 20)),
      axis.text = element_text(
        size = 15,
        face = 3
      )
    ) +
    stat_function(fun = dnorm, args = list(mean = mean, sd = sd), col = 'black')
}

time_range <- c("2018-01-01", "2022-01-01")
inflation_delta <- read_eu_inflation() %>%
  filter_by_time(time_range) %>%
  arrange(Date) %>%
  mutate(
    Poland = Poland - lag(Poland, 1),
    France = France - lag(France, 1),
    Romania = Romania - lag(Romania, 1)
  ) %>%
  select(
    Date, Poland, France, Romania
  ) %>%
  na.omit

draw_histogram(inflation_delta,
               "Monthly inflation delta for Poland",
                    "Poland",
               mean(inflation_delta$Poland),
               sd(inflation_delta$Poland),
               "cadetblue")
draw_histogram(inflation_delta,
               "Monthly inflation delta for Romania",
               "Romania",
               mean(inflation_delta$Romania),
               sd(inflation_delta$Romania),
               "darkgoldenrod")
draw_histogram(inflation_delta,
               "Monthly inflation delta for France",
               "France",
               mean(inflation_delta$France),
               sd(inflation_delta$France),
               "firebrick")

poland_plot <- draw_histogram(inflation_delta,
                              "Monthly inflation delta for Poland",
                              "Poland",
                              mean(inflation_delta$Poland),
                              sd(inflation_delta$Poland),
                              "cadetblue")
romania_plot <- draw_histogram(inflation_delta,
                               "Monthly inflation delta for Romania",
                               "Romania",
                               mean(inflation_delta$Romania),
                               sd(inflation_delta$Romania),
                               "darkgoldenrod")
france_plot <- draw_histogram(inflation_delta,
                              "Monthly inflation delta for France",
                              "France",
                              mean(inflation_delta$France),
                              sd(inflation_delta$France),
                              "firebrick")

ggsave(file = "plots/inflation_delta_poland.eps", plot = poland_plot, width = 10, height = 8)
ggsave(file = "plots/inflation_delta_france.eps", plot = france_plot, width = 10, height = 8)
ggsave(file = "plots/inflation_delta_romania.eps", plot = romania_plot, width = 10, height = 8)
