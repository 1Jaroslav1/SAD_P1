source("./Problem1/Functions.R")
source("./Problem1/Utils.R")
library(dplyr)

draw_histogram <- function(df, title, column_name, mean, sd, color) {
  cat(column_name, ": ", "mean - ", mean, ', standard deviation = ', sd, '\n')
  ggplot(df) +
    geom_histogram(binwidth = 0.1, aes(x = get(column_name), y = after_stat(density)), fill = paste(color, '1'), color = color) +
    xlab("Monthly inflation delta") +
    xlim(-1.5, 2.5) +
    ylab("Density") +
    ylim(0,2) +
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

draw_barplots <- function(df, title, country_name, color) {
  ggplot(df, aes(x = Date, y = get(country_name))) +
    geom_bar(stat = "identity", show.legend = FALSE, aes(fill = paste(color, '1'), color = color)) +
    xlab("Time") +
    ylab("Inflation [%]") +
    ylim(0, 17) +
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
    )
}


time_range <- c("2018-01-01", now)
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

ggsave(file = "./Problem1/plots/inflation_delta_poland.eps", plot = poland_plot, width = 10, height = 8)
ggsave(file = "./Problem1/plots/inflation_delta_france.eps", plot = france_plot, width = 10, height = 8)
ggsave(file = "./Problem1/plots/inflation_delta_romania.eps", plot = romania_plot, width = 10, height = 8)


inflation <- read_eu_inflation() %>% filter_by_time(time_range)
poland <- draw_barplots(inflation, "Inflation - Poland", "Poland", "cadetblue")
france <- draw_barplots(inflation, "Inflation - France", "France", "firebrick")
romania <- draw_barplots(inflation, "Inflation - Romania", "Romania", "darkgoldenrod")
ggsave(file = "./Problem1/plots/inflation_since2018_poland.png", plot = poland, width = 10, height = 8)
ggsave(file = "./Problem1/plots/inflation_since2018_france.png", plot = france, width = 10, height = 8)
ggsave(file = "./Problem1/plots/inflation_since2018_romania.png", plot = romania, width = 10, height = 8)
