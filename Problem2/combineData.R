# source("./Problem2/Poland.R")
# source("./Problem2/German.R")
# source("./Problem2/Czechia.R")

draw_stacking_data <- function(data_frame, title) {
  ggplot(data_frame, aes(x = Period, y = Value, group = Country, color = Country)) +
    scale_color_viridis(discrete = TRUE) +
    geom_line(aes(color = Country), size = 1.5) +
    geom_point(shape = 21, color = "black", fill = "black", size = 1.5) +
    guides(fill = guide_legend(title = NULL)) +
    xlab("Date") +
    ylab("Inflation") +
    ggtitle(title) +
    theme(
      plot.title = element_text(size = 18)
    )
}

avg_gross <- data.frame(Period = avg_gross_pl$Period)
avg_gross$Poland <- avg_gross_pl$Value
avg_gross$German <- avg_gross_german$Value
avg_gross$Czechia <- avg_gross_czechia$Value
avg_gross <-  pivot_longer(avg_gross, -Period, names_to = "Country", values_to = "Value")
draw_stacking_data(avg_gross, "Average monthly gross")

food_price_to_gross <- data.frame(Period = food_price_to_gross_df_pl$Period)
food_price_to_gross$Poland <- food_price_to_gross_df_pl$Value
food_price_to_gross$German <- food_price_to_gross_df_german$Value
food_price_to_gross$Czechia <- food_price_to_gross_df_czechia$Value
food_price_to_gross <-  pivot_longer(food_price_to_gross, -Period, names_to = "Country", values_to = "Value")
draw_stacking_data(food_price_to_gross, "Food price to Gross")

avg_food_price <- data.frame(Period = avg_food_price_pl$Period)
avg_food_price$Poland <- avg_food_price_pl$Value
avg_food_price$German <- avg_food_price_german$Value
avg_food_price$Czechia <- avg_food_price_czechia$Value
avg_food_price <-  pivot_longer(avg_food_price, -Period, names_to = "Country", values_to = "Value")
draw_stacking_data(avg_food_price, "Average food price")

hicd <- data.frame(Period = hicd_pl$Period)
hicd$Poland <- hicd_pl$Value
hicd$German <- hicd_german$Value
hicd$Czechia <- hicd_czechia$Value
hicd <-  pivot_longer(hicd, -Period, names_to = "Country", values_to = "Value")
draw_stacking_data(hicd, "HICD")
