eu_inflation = read.csv("./EU_Inflation_HICP_data.csv")

colnames(eu_inflation)[1] = "Date"
colnames(eu_inflation)[7] = "EU"

eu_inflation = na.omit(eu_inflation)
eu_inflation[, 2:29] = sapply(eu_inflation[, 2:29], as.double)
eu_inflation["Date"] = anytime::anydate(paste(eu_inflation[,"Date"], 1))

eu_inflation_tidy = pivot_longer(eu_inflation, -Date, names_to = "Country", values_to = "Inflation")

eu_zone_countries = c("Date", "Austria","Belgium","Cyprus","Estonia","Finland","France","Germany","Greece","Ireland","Italy","Latvia","Lithuania","Luxembourg","Malta","Netherlands","Portugal","Slovakia","Slovenia","Spain")
not_eu_zone_countries = colnames(eu_inflation)[!(colnames(eu_inflation) %in% eu_zone_countries) & colnames(eu_inflation) != "EU..changingcomposition."]
not_eu_zone_countries = c(c("Date"), not_eu_zone_countries)

eu_countries_in_zone_inflation_tidy = filter(eu_inflation_tidy, Country %in% eu_zone_countries)
eu_countries_NOT_in_zone_inflation_tidy = filter(eu_inflation_tidy, Country %in% not_eu_zone_countries)
pl_inflation_tidy = filter(eu_inflation_tidy, Country == "Poland")
eu_changingcomposition_inflation_tidy = filter(eu_inflation_tidy, Country == "EU")


eu_countries_in_zone_inflation = eu_inflation[eu_zone_countries]
eu_countries_NOT_in_zone_inflation = eu_inflation[not_eu_zone_countries]
pl_inflation = eu_inflation[c("Date", "Poland")]
eu_changingcomposition_inflation = eu_inflation[c("Date", "EU")]

eu_inflaction_DF = data.frame(
  eu_inflation[1],
  Inflation = rowMeans(eu_inflation[2:29])
)

eu_countries_in_zone_inflation_DF = data.frame(
  eu_inflation[1],
  Inflation= rowMeans(eu_countries_in_zone_inflation[2:20])
)

eu_countries_NOT_in_zone_inflation_DF = data.frame(
  eu_inflation[1],
  Inflation = rowMeans(eu_countries_NOT_in_zone_inflation[2:10])
)

pl_inflation_DF = data.frame(
  eu_inflation[1],
  Inflation = rowMeans(pl_inflation[2])
)

general_DF = data.frame(
  eu_inflation[1],
  "Europe" = eu_inflaction_DF[2],
  "Eu_countries" = eu_countries_in_zone_inflation_DF[2],
  "Other_countries" = eu_countries_NOT_in_zone_inflation_DF[2],
  "Poland" = pl_inflation_DF[2]
)

colnames(general_DF)[2] = "Europe"
colnames(general_DF)[3] = "EU countries"
colnames(general_DF)[4] = "Other countries"
colnames(general_DF)[5] = "Poland"

general_tidy_DF = pivot_longer(general_DF, -Date, names_to = "Country", values_to = "Inflation")

# ------ Periods ------

pandemic_start = "2020-01-09"
during_pandemic = "2021-01-09"
war_start = "2022-02-24"
now = "2022-11-24"


