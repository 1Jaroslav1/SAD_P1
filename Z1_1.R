generalAnalyzeAndPloting = function(data, title) {
  cat("-----", title, "---------\n")
  columnAnalyze(data, "Poland")
  columnAnalyze(data, "Europe")
  columnAnalyze(data, "EU countries")
  columnAnalyze(data, "Other countries")
  cat("----------------------------------- \n")
}

# From 2017/01/01 to 2020/01/09 - before pandemic
data_before_pandemic = filterDataByTime(general_tidy_DF, c("2017-01-01", pandemic_start))
drawStackingData(data_before_pandemic, "Before pandemic")
drawBoxplot(data_before_pandemic, "Before pandemic")
generalAnalyzeAndPloting(data_before_pandemic, "Before pandemic")

# during pandemic
data_during_pandemic = filterDataByTime(general_tidy_DF, c(pandemic_start, during_pandemic))
drawStackingData(data_during_pandemic, "During Pandemic")
drawBoxplot(data_during_pandemic, "During Pandemic")
generalAnalyzeAndPloting(data_during_pandemic, "During Pandemic")

# before war

data_before_war = filterDataByTime(general_tidy_DF, c(during_pandemic, war_start))
drawStackingData(data_before_war, "Before War")
drawBoxplot(data_before_war, "Before War")
generalAnalyzeAndPloting(data_before_war, "Before War")

# during war
data_during_war = filterDataByTime(general_tidy_DF, c(war_start, now))
drawStackingData(data_during_war, "During War")
drawBoxplot(data_during_war, "During War")
generalAnalyzeAndPloting(data_during_war, "During War")