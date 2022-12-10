source("./Problem2/Poland.R")
source("./Problem2/German.R")
source("./Problem2/Czechia.R")

draw_stacking_data <- function(data_frame, title, ylab) {
  ggplot(data_frame, aes(x = Period, y = Value, group = Country, color = Country)) +
    # scale_color_viridis(discrete = TRUE) +
    geom_line(aes(color = factor(Country)), size = 1.5) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
    geom_point(shape = 21, color = "black", fill = "black", size = 1.5) +
    geom_text(aes(label = scales::comma(Value)), size = 3, vjust = -1.5) +
    guides(fill = guide_legend(title = NULL)) +
    xlab("Date") +
    ylab(ylab) +
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
      legend.title = element_text(size = 20)
    )
}

hicp <- data.frame(Period = hicp_pl$Period)
hicp$Poland <- hicp_pl$Value
hicp$German <- hicp_german$Value
hicp$Czechia <- hicp_czechia$Value
hicp <-  pivot_longer(hicp, -Period, names_to = "Country", values_to = "Value")
hicp_plot <- draw_stacking_data(hicp, "Inflation", "Inflation (%)")

avg_food_price <- data.frame(Period = avg_food_price_pl$Period)
avg_food_price$Poland <- avg_food_price_pl$Value
avg_food_price$German <- avg_food_price_german$Value
avg_food_price$Czechia <- avg_food_price_czechia$Value
avg_food_price <-  pivot_longer(avg_food_price, -Period, names_to = "Country", values_to = "Value")
avg_food_price_plot <- draw_stacking_data(avg_food_price, "Average food cost per month", "Price in $")


avg_gross <- data.frame(Period = avg_gross_pl$Period)
avg_gross$Poland <- avg_gross_pl$Value
avg_gross$German <- avg_gross_german$Value
avg_gross$Czechia <- avg_gross_czechia$Value
avg_gross <-  pivot_longer(avg_gross, -Period, names_to = "Country", values_to = "Value")
avg_gross_plot <- draw_stacking_data(avg_gross, "Average salary per month (full-time)", "Salary in $")

food_price_to_gross <- data.frame(Period = food_price_to_gross_df_pl$Period)
food_price_to_gross$Poland <- food_price_to_gross_df_pl$Value
food_price_to_gross$German <- food_price_to_gross_df_german$Value
food_price_to_gross$Czechia <- food_price_to_gross_df_czechia$Value
food_price_to_gross <-  pivot_longer(food_price_to_gross, -Period, names_to = "Country", values_to = "Value")
food_price_to_gross_plot <- draw_stacking_data(food_price_to_gross, "Ratio of salary to the price of food", "Ratio in %")


ggsave(file = "./Problem2/plot_latex/hicp.eps", plot = hicp_plot, width = 10, height = 8)
ggsave(file = "./Problem2/plot_latex/avg_food_price.eps", plot = avg_food_price_plot, width = 10, height = 8)
ggsave(file = "./Problem2/plot_latex/avg_gross.eps", plot = avg_gross_plot, width = 10, height = 8)
ggsave(file = "./Problem2/plot_latex/food_price_to_gross.eps", plot = food_price_to_gross_plot, width = 10, height = 8)
