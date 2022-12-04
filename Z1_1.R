source("Functions.R")
source("InflationDataPreparing.R")

general_analyze_and_ploting <- function(data, title) {
  cat("-----", title, "---------\n")
  column_analyze(data, "Polska")
  column_analyze(data, "Europa")
  column_analyze(data, "Kraje w strefie Euro")
  column_analyze(data, "Kraje poza strefy Euro")
  cat("----------------------------------- \n")
}

# From 2017/01/01 to 2020/01/09 - before pandemic
data_before_pandemic <- filter_by_time(general_inflation_tidy_DF, c(analysis_start, pandemic_start))
draw_stacking_data(data_before_pandemic, "Poziom inflacji przed pandemią")
draw_box_plot(data_before_pandemic, "Poziom inflacji przed pandemią")
general_analyze_and_ploting(data_before_pandemic, "Poziom inflacji przed pandemią")

# during pandemic
data_during_pandemic <- filter_by_time(general_inflation_tidy_DF, c(pandemic_start, during_pandemic))
draw_stacking_data(data_during_pandemic, "Poziom inflacji w szczycie pamdemii")
draw_box_plot(data_during_pandemic, "Poziom inflacji w szczycie pamdemii")
general_analyze_and_ploting(data_during_pandemic, "Poziom inflacji w szczycie pamdemii")

# before war
data_before_war <- filter_by_time(general_inflation_tidy_DF, c(during_pandemic, war_start))
draw_stacking_data(data_before_war, "Poziom inflacji przed wojną")
draw_box_plot(data_before_war, "Poziom inflacji przed wojną")
general_analyze_and_ploting(data_before_war, "Poziom inflacji przed wojną")

# during war
data_during_war <- filter_by_time(general_inflation_tidy_DF, c(war_start, now))
draw_stacking_data(data_during_war, "Poziom inflacji podczas wojny")
draw_box_plot(data_during_war, "Poziom inflacji podczas wojny")
general_analyze_and_ploting(data_during_war, "Poziom inflacji podczas wojny")