source("Functions.R")
source("InflationDataPreparing.R")

general_analyze_and_ploting <- function(data, title) {
  cat("-----", title, "---------\n")
  column_analyze(data, "Poland")
  column_analyze(data, "Europe")
  column_analyze(data, "In the Eurozone")
  column_analyze(data, "Outside the Eurozone")
  cat("----------------------------------- \n")
}

# From 2017/01/01 to 2020/01/09 - before pandemic
data_before_pandemic <- filter_by_time(general_inflation_tidy_DF, c(analysis_start, pandemic_start))
before_pandemic_inflation_plot <- draw_stacking_data(data_before_pandemic, "Inflation before the pandemic")
before_pandemic_inflation_box_plot <- draw_box_plot(data_before_pandemic, "Inflation before the pandemic")
general_analyze_and_ploting(data_before_pandemic, "Inflation before the pandemic")

# during pandemic
data_during_pandemic <- filter_by_time(general_inflation_tidy_DF, c(pandemic_start, during_pandemic))
during_pandemic_inflation_plot <- draw_stacking_data(data_during_pandemic, "Inflation at the peak of the pandemic")
during_pandemic_inflation_box_plot <- draw_box_plot(data_during_pandemic, "Inflation at the peak of the pandemic")
general_analyze_and_ploting(data_during_pandemic, "Inflation at the peak of the pandemic")

# before war
data_before_war <- filter_by_time(general_inflation_tidy_DF, c(during_pandemic, war_start))
before_war_inflation_plot <- draw_stacking_data(data_before_war, "Inflation before the war")
before_war_inflation_box_plot <- draw_box_plot(data_before_war, "Inflation before the war")
general_analyze_and_ploting(data_before_war, "Inflation before the war")

# during war
data_during_war <- filter_by_time(general_inflation_tidy_DF, c(war_start, now))
during_war_inflation_plot <- draw_stacking_data(data_during_war, "Inflation during the war")
during_war_inflation_box_plot <- draw_box_plot(data_during_war, "Inflation during the war")
general_analyze_and_ploting(data_during_war, "Inflation during the war")

ggsave(file = "./plots/before_pandemic_inflation_plot.eps", plot = before_pandemic_inflation_plot, width = 10, height = 8)
ggsave(file = "./plots/before_pandemic_inflation_box_plot.eps", plot = before_pandemic_inflation_box_plot, width = 10, height = 8)
ggsave(file = "./plots/during_pandemic_inflation_plot.eps", plot = during_pandemic_inflation_plot, width = 10, height = 8)
ggsave(file = "./plots/during_pandemic_inflation_box_plot.eps", plot = during_pandemic_inflation_box_plot, width = 10, height = 8)
ggsave(file = "./plots/before_war_inflation_plot.eps", plot = before_war_inflation_plot, width = 10, height = 8)
ggsave(file = "./plots/before_war_inflation_box_plot.eps", plot = before_war_inflation_box_plot, width = 10, height = 8)
ggsave(file = "./plots/during_war_inflation_plot.eps", plot = during_war_inflation_plot, width = 10, height = 8)
ggsave(file = "./plots/during_war_inflation_box_plot.eps", plot = during_war_inflation_box_plot, width = 10, height = 8)
