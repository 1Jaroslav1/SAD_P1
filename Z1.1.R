# Libraries
library(ggplot2)
library(dplyr)
library("tidyverse")
library(tidyr)
library(readr)

dataFrame = data.frame(
  date = pl_inflation[,"Date"],
  inflation = pl_inflation[,"Inflation"]
)

dataFrame = filterDataByTime(dataFrame, c("2010-01-12", "2020-01-12"))



drawData = function(dataFrame, lineColor, pointColor){
  ggplot(dataFrame, aes(x=Date, y=Inflation)) +
    geom_line(aes(color="Poland"), size=1) +
    geom_point(shape=21, color=pointColor, fill=pointColor, size=1.5) +
    scale_x_date(date_labels = "%Y-%m", date_minor_breaks = "1 month") +
    scale_color_manual(values=lineColor)
}

drawSeveralData = function(data) {

}

filterDataByTime = function(data, period) {
  return(data[(data$Date > period[1] & data$Inflation < period[2]), ])
}

filterDataByCountry = function() {

}

drawData(dataFrame, "Green", "#E14D2A")

# time = eu_countries_NOT_in_zone_inflation[,"Period.Unit"]
# value = eu_countries_NOT_in_zone_inflation %>% select(2:9)
# group = colnames(eu_countries_NOT_in_zone_inflation)

# data.frame(time, value, group)
# multy_date =

# drawSeveralData(eu_countries_NOT_in_zone_inflation)