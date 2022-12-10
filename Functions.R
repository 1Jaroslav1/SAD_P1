# Libraries

# HELP -> https://r-graph-gallery.com/stacked-area-graph.html

library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(tidyverse)
library(hrbrthemes)
library(viridis)
library(plyr)
# --------- Data Frame -----------

get_data_frame <- function(data) {
  data.frame(
    date = data[, "Date"],
    inflation = data[, "Inflation"],
    country = data[, "Country"]
  )
}

get_data_frame_by_country <- function(data, country_name) {
  get_data_frame(filter_data_by_country(data, country_name))
}

# --------- Draw Frame -----------

drawData <- function(data_frame, line_color, point_color, title) {
  ggplot(data_frame, aes(x = Date, y = Inflation)) +
    geom_line(aes(color = Country), size = 1) +
    geom_point(shape = 21, color = point_color, fill = point_color, size = 1.5) +
    scale_x_date(date_labels = "%Y-%m", date_minor_breaks = "1 month") +
    scale_color_manual(values = line_color) +
    xlab("Date") +
    ylab("Inflation") +
    ggtitle(title) +
    theme(
      plot.title = element_text(size = 18)
    )
}


draw_several_data <- function(data_frame, title) {
  ggplot(data_frame, aes(x = Date, y = Inflation, fill = Country)) +
    geom_area() +
    scale_x_date(date_labels = "%Y-%m", date_minor_breaks = "1 month") +
    xlab("Date") +
    ylab("Inflation") +
    ggtitle(title) +
    theme(
      plot.title = element_text(size = 18)
    )
}

draw_stacking_data <- function(data_frame, title) {
  ggplot(data_frame, aes(x = Date, y = Inflation, group = Country, color = Country)) +
    # scale_color_viridis(discrete = TRUE, option = "H") +
    geom_line(aes(color = factor(Country)), size = 2) +
    geom_point(shape = 21, color = "black", fill = "black", size = 1.5) +
    scale_x_date(date_labels = "%Y-%m", date_minor_breaks = "1 month") +
    guides(fill = guide_legend(title = NULL)) +
    xlab("Date") +
    ylab("Inflation [%]") +
    ggtitle(title) +
    theme_minimal() +
    theme(
      plot.title=element_text(size = 30, hjust=0.5, vjust=0.5, face='bold', margin = margin(20, 0, 20, 0)),
      axis.title = element_text(size=20,face="bold"),
      axis.title.x = element_text(margin = margin(20, 0, 20, 0)),
      axis.title.y = element_text(margin = margin(0, 20, 0, 20)),
      axis.text = element_text(
        size=15,
        face=3
      ),
      legend.text = element_text(size = 15),
      legend.title = element_text(size = 20),
    )
}

draw_box_plot <- function(data_frame, title) {
  ggplot(data_frame, aes(x = Country, y = Inflation, fill = Country)) +
    geom_boxplot() +
    scale_fill_viridis(discrete = TRUE, option="D") +
    geom_jitter(color = "black", size = 0.4) +
    ylab("Inflation") +
    ggtitle(title) +
    theme_minimal() +
    theme(
      plot.title=element_text(size = 30, hjust=0.5, vjust=0.5, face='bold', margin = margin(20, 0, 20, 0)),
      axis.title = element_text(size=20,face="bold"),
      axis.title.y = element_text(size=20,face="bold", margin = margin(0, 20, 0, 20)),
      axis.text = element_text(
        size=15,
        face=3
      ),
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      legend.text = element_text(size = 15),
      legend.title = element_text(size = 20),
    )
}

# draw_box_plot <- function(data_frame, title) {
#   median_count <- ddply(data_frame, .(Country), summarise, median = median(Inflation))
#   mean_count <- ddply(data_frame, .(Country), summarise, mean = mean(Inflation))

#   ggplot(data_frame, aes(x = Country, y = Inflation, fill = Country)) +
#     geom_boxplot() +
#     scale_fill_viridis(discrete = TRUE, alpha = 0.7) +
#     geom_jitter(color = "black", size = 0.4, alpha = 0.9) +
#     geom_text(data = median_count, aes(x = Country, y = round(median, 3), label = paste0("Median: ", round(median, 3), '%')), size = 5, vjust = -1) +
#     geom_text(data = mean_count, aes(x = Country, y = round(mean, 3), label = paste0("Mean: ", round(mean, 3), '%')), size = 5, vjust = -1) +
#     xlab("Country") +
#     ylab("Inflation") +
#     ggtitle(title) +
#     theme_minimal() +
#     theme(
#       plot.title=element_text(size = 30, hjust=0.5, vjust=0.5, face='bold', margin = margin(20, 0, 20, 0)),
#       axis.title = element_text(size=20,face="bold"),
#       axis.title.x = element_text(margin = margin(20, 0, 20, 0)),
#       axis.title.y = element_text(margin = margin(0, 20, 0, 20)),
#       axis.text = element_text(
#         size=15,
#         face=3
#       ),
#       axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
#       legend.text = element_text(size = 15),
#       legend.title = element_text(size = 20),
#     )
# }

draw_stacked_metrics <- function(df, title) {
  ggplot(df, aes(x = Date, y = Value, group = Metric)) +
    geom_point() +
    geom_line(aes(color = Metric)) +
    scale_x_date(date_labels = "%Y-%m", date_minor_breaks = "1 month") +
    guides(fill = guide_legend(title = NULL)) +
    xlab("Date") +
    ylab("Value [%]") +
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

draw_aggregated_barplots <- function(df, title, y_label) {
  ggplot(df, aes(x = Country, y = Values, fill = Metric, color = Metric)) +
    geom_bar(stat = "identity", position = "dodge") +
    xlab("Country") +
    ylab(y_label) +
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
    coord_flip()
}

# --------- Filters -----------

filter_by_time <- function(data, period) {
  data %>% filter(
    Date >= period[1],
    Date <= period[2]
  )
}

filter_data_by_country <- function(data, country_name) {
  return(filter(data, Country == country_name))
}

filter_by_metric <- function(df, column_name, category_name) {
  df %>%
    select(Country, all_of(column_name)) %>%
    rename(Values = all_of(column_name)) %>%
    mutate(Metric = category_name)
}

get_for_country_in_time_range <- function(df, filter_range, country_name) {
  df %>%
    select(Date, all_of(country_name)) %>%
    filter_by_time(filter_range) %>%
    rename(
      Value = all_of(country_name)
    ) %>%
    na.omit()
}

# ------ Analyze -------------

column_analyze = function(data, column_name) {
  filteredData = filter_data_by_country(data, column_name)
  mean = mean(filteredData$Inflation)
  range = range(filteredData$Inflation)
  var = var(filteredData$Inflation)
  median = median(filteredData$Inflation)

  cat(column_name, " mean: ", mean, "median: ", median, "range: ", range, " var: ", var, '\n')
}


# --------- Tests -----------

# drawData(filterDataByTime(getDataFrameByCountry(eu_inflation_tidy, "Poland"), c("2010-01-12", "2020-01-12")), "Green", "#E14D2A", "Plot")
# drawSeveralData(filterDataByTime(getDataFrame(eu_countries_NOT_in_zone_inflation_tidy), c("2010-01-12", "2020-01-12")), "Plot")
# drawStackingData(filterDataByTime(general_tidy_DF, c("2010-01-12", "2020-01-12")), "Plot")
# drawStackingData(filterDataByTime(getDataFrame(general_tidy_DF), c("2010-01-12", "2020-01-12")), "Plot")
# drawBoxplot(filterDataByTime(getDataFrame(eu_inflation_tidy), c("2010-01-12", "2020-01-12")), "Plot")