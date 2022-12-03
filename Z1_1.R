source("Functions.R")
source("InflationDataPreparing.R")
generalAnalyzeAndPloting = function(data, title) {
  cat("-----", title, "---------\n")
  columnAnalyze(data, "Polska")
  columnAnalyze(data, "Europa")
  columnAnalyze(data, "Kraje w strefie Euro")
  columnAnalyze(data, "Kraje poza strefy Euro")
  cat("----------------------------------- \n")
}

# From 2017/01/01 to 2020/01/09 - before pandemic
data_before_pandemic = filterDataByTime(general_inflation_tidy_DF, c("2017-01-01", pandemic_start))
drawStackingData(data_before_pandemic, "Poziom inflacji przed pandemią")
drawBoxPlot(data_before_pandemic, "Poziom inflacji przed pandemią")
generalAnalyzeAndPloting(data_before_pandemic, "Poziom inflacji przed pandemią")

# during pandemic
data_during_pandemic = filterDataByTime(general_inflation_tidy_DF, c(pandemic_start, during_pandemic))
drawStackingData(data_during_pandemic, "Poziom inflacji w szczycie pamdemii")
drawBoxPlot(data_during_pandemic, "Poziom inflacji w szczycie pamdemii")
generalAnalyzeAndPloting(data_during_pandemic, "Poziom inflacji w szczycie pamdemii")

# before war
data_before_war = filterDataByTime(general_inflation_tidy_DF, c(during_pandemic, war_start))
drawStackingData(data_before_war, "Poziom inflacji przed wojną")
drawBoxPlot(data_before_war, "Poziom inflacji przed wojną")
generalAnalyzeAndPloting(data_before_war, "Poziom inflacji przed wojną")

# during war
data_during_war = filterDataByTime(general_inflation_tidy_DF, c(war_start, now))
drawStackingData(data_during_war, "Poziom inflacji podczas wojny")
drawBoxPlot(data_during_war, "Poziom inflacji podczas wojny")
generalAnalyzeAndPloting(data_during_war, "Poziom inflacji podczas wojny")