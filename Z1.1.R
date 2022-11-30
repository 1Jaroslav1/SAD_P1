# Libraries

# HELP -> https://r-graph-gallery.com/stacked-area-graph.html

library(ggplot2)
library(dplyr)
library("tidyverse")
library(tidyr)
library(readr)

# --------- Data Frame -----------

getDataFrame = function(data) {
  dataFrame = data.frame(
    date = data[,"Date"],
    inflation = data[,"Inflation"],
    country = data[, "Country"]
  )

  return(dataFrame)
}

getDataFrameByCountry = function(data, countryName) {
  country_data = filterDataByCountry(data, countryName)

  return(getDataFrame(country_data))
}

# --------- Draw Frame -----------

drawData = function(dataFrame, lineColor, pointColor){
  ggplot(dataFrame, aes(x=Date, y=Inflation)) +
    geom_line(aes(color=Country), size=1) +
    geom_point(shape=21, color=pointColor, fill=pointColor, size=1.5) +
    scale_x_date(date_labels = "%Y-%m", date_minor_breaks = "1 month") +
    scale_color_manual(values=lineColor)
}

drawSeveralData = function(dataFrame) {
  ggplot(dataFrame, aes(x=Date, y=Inflation, fill=Country)) +
    geom_area()
}

drawStackingData = function(dataFrame) {
  ggplot(dataFrame, aes(x=Date, y=Inflation, group=Country, color=Country)) +
    geom_line() +
    scale_color_viridis(discrete = TRUE) +
    facet_grid() +
    theme(legend.position="none") +
    ggtitle("Inflation") +
    theme_ipsum()
}

# --------- Filters -----------

filterDataByTime = function(data, period) {
  return(data[(data$Date > period[1] & data$Date < period[2]), ])
}

filterDataByCountry = function(data, countryName) {
  return(filter(data, Country == countryName))
}

# --------- Tests -----------

drawData(filterDataByTime(getDataFrameByCountry(eu_inflation_tidy, "Poland"), c("2010-01-12", "2020-01-12")), "Green", "#E14D2A")
drawSeveralData(filterDataByTime(getDataFrame(eu_countries_NOT_in_zone_inflation_tidy), c("2010-01-12", "2020-01-12")))
drawStackingData(filterDataByTime(getDataFrame(eu_countries_NOT_in_zone_inflation_tidy), c("2010-01-12", "2020-01-12")))
