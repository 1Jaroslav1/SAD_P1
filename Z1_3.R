drawStackingData = function(dataFrame, title) {
    ggplot(dataFrame, aes(x=Date, y=Difference, group=Country, color=Country)) +
      scale_color_viridis(discrete = TRUE) +
      geom_line(aes(color=Country), size=1.5) +
      geom_point(shape=21, color="black", fill="black", size=1.5) +
      scale_x_date(date_labels = "%Y-%m", date_minor_breaks = "1 month") +
      guides(fill=guide_legend(title=NULL)) +
      xlab("Date") +
      ylab("Inflation") +
      ggtitle(title) +
      theme(
        plot.title = element_text(size=18)
      )
}

countries = c("Poland", "France", "Romania")

countries_inflation_data_tidy =  filter(eu_inflation_tidy, Country %in% countries)
countries_inflation_data_tidy = filterDataByTime(countries_inflation_data_tidy, c("2008-01-01", "2022-01-01"))

countries_inflation_data_DF = data.frame(
  eu_inflation[1],
  eu_inflation[countries]
)

# countries_inflation_data_tidy[difference] = NA
#
# for

countries_difference_inflation_data = mutate(countries_inflation_data_tidy, Difference=(Inflation - append(Inflation[-c(1, 2, 3)], c(0,0,0))))
countries_difference_inflation_data = head(countries_difference_inflation_data, -3)

drawStackingData(countries_difference_inflation_data, "Plot")

