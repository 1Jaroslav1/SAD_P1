# Libraries

# HELP -> https://r-graph-gallery.com/stacked-area-graph.html

library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(tidyverse)
library(hrbrthemes)
library(viridis)

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

drawData = function(dataFrame, lineColor, pointColor, title){
  ggplot(dataFrame, aes(x=Date, y=Inflation)) +
    geom_line(aes(color=Country), size=1) +
    geom_point(shape=21, color=pointColor, fill=pointColor, size=1.5) +
    scale_x_date(date_labels = "%Y-%m", date_minor_breaks = "1 month") +
    scale_color_manual(values=lineColor) +
    xlab("Date") +
    ylab("Inflation") +
    ggtitle(title)
}

drawSeveralData = function(dataFrame, title) {
  ggplot(dataFrame, aes(x=Date, y=Inflation, fill=Country)) +
    geom_area() +
    scale_x_date(date_labels = "%Y-%m", date_minor_breaks = "1 month") +
    xlab("Date") +
    ylab("Inflation") +
    ggtitle(title)
}

drawStackingData = function(dataFrame, title) {
    ggplot(dataFrame, aes(x=Date, y=Inflation, group=Country, color=Country)) +
      geom_line(aes(color=Country), size=1.5) +
      geom_point(shape=21, color="red", fill="red", size=1.5) +
      scale_x_date(date_labels = "%Y-%m", date_minor_breaks = "1 month") +
      guides(fill=guide_legend(title=NULL)) +
      xlab("Date") +
      ylab("Inflation") +
      ggtitle(title)
}

drawBoxplot = function(dataFrame, title) {
  ggplot(dataFrame, aes(x=Country, y=Inflation, fill=Country)) +
    geom_boxplot() +
    scale_fill_viridis(discrete = TRUE, alpha=0.7) +
    geom_jitter(color="black", size=0.4, alpha=0.9) +
    theme(legend.position="none", plot.title = element_text(size=11)) +
    xlab("Country") +
    ylab("Inflation") +
    ggtitle(title)
}

# --------- Filters -----------

filterDataByTime = function(data, period) {
  return(data[(data$Date > period[1] & data$Date < period[2]), ])
}

filterDataByCountry = function(data, countryName) {
  return(filter(data, Country == countryName))
}

# ------ Analize -------------

columnAnalyze = function(data, columnName) {
  filteredData = filterDataByCountry(data, columnName)
  mean = mean(filteredData$Inflation)
  range = range(filteredData$Inflation)
  var = var(filteredData$Inflation)

  cat(columnName, " mean:", mean, " range: ", range, " var: ", var, '\n')
}


# --------- Tests -----------

# drawData(filterDataByTime(getDataFrameByCountry(eu_inflation_tidy, "Poland"), c("2010-01-12", "2020-01-12")), "Green", "#E14D2A", "Plot")
# drawSeveralData(filterDataByTime(getDataFrame(eu_countries_NOT_in_zone_inflation_tidy), c("2010-01-12", "2020-01-12")), "Plot")
# drawStackingData(filterDataByTime(general_tidy_DF, c("2010-01-12", "2020-01-12")), "Plot")
# drawStackingData(filterDataByTime(getDataFrame(eu_inflation_tidy), c("2010-01-12", "2020-01-12")), "Plot")
drawBoxplot(filterDataByTime(getDataFrame(eu_inflation_tidy), c("2010-01-12", "2020-01-12")), "Plot")