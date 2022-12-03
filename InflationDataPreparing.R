library(anytime)
eu_inflation = read.csv("data/EU_Inflation_HICP_data.csv")

# Change columns name

colnames(eu_inflation)[1] = "Date"
colnames(eu_inflation)[7] = "EU"

eu_inflation = na.omit(eu_inflation)
eu_inflation[, 2:29] = sapply(eu_inflation[, 2:29], as.double)
eu_inflation["Date"] = anytime::anydate(paste(eu_inflation[,"Date"], 1))

eu_inflation_tidy = pivot_longer(eu_inflation, -Date, names_to = "Country", values_to = "Inflation")

# create list of countries which are and aren`t in eu zone

eu_zone_countries = c("Date", "Austria","Belgium","Cyprus","Estonia","Finland","France","Germany","Greece","Ireland","Italy","Latvia","Lithuania","Luxembourg","Malta","Netherlands","Portugal","Slovakia","Slovenia","Spain")
not_eu_zone_countries = colnames(eu_inflation)[!(colnames(eu_inflation) %in% eu_zone_countries) & colnames(eu_inflation) != "EU"]
not_eu_zone_countries = c(c("Date"), not_eu_zone_countries)

# data -> date | country | inflation

eu_countries_in_zone_inflation_tidy = filter(eu_inflation_tidy, Country %in% eu_zone_countries)
eu_countries_NOT_in_zone_inflation_tidy = filter(eu_inflation_tidy, Country %in% not_eu_zone_countries)
pl_inflation_tidy = filter(eu_inflation_tidy, Country == "Poland")
eu_changingcomposition_inflation_tidy = filter(eu_inflation_tidy, Country == "EU")

# data -> date | inflation

eu_countries_in_zone_inflation = eu_inflation[eu_zone_countries]
eu_countries_NOT_in_zone_inflation = eu_inflation[not_eu_zone_countries]
pl_inflation = eu_inflation[c("Date", "Poland")]
eu_changingcomposition_inflation = eu_inflation[c("Date", "EU")]

eu_inflation_DF = data.frame(
  eu_inflation[1],
  Inflation = rowMeans(eu_inflation[2:29])
)

eu_countries_in_zone_inflation_DF = data.frame(
  eu_inflation[1],
  Inflation= rowMeans(eu_countries_in_zone_inflation[2:20])
)

eu_countries_NOT_in_zone_inflation_DF = data.frame(
  eu_inflation[1],
  Inflation = rowMeans(eu_countries_NOT_in_zone_inflation[2:9])
)

pl_inflation_DF = data.frame(
  eu_inflation[1],
  Inflation = rowMeans(pl_inflation[2])
)

general_inflation_DF = data.frame(
  eu_inflation[1],
  "Europe" = eu_inflation_DF[2],
  "Eu_countries" = eu_countries_in_zone_inflation_DF[2],
  "Other_countries" = eu_countries_NOT_in_zone_inflation_DF[2],
  "Poland" = pl_inflation_DF[2]
)

colnames(general_inflation_DF)[2] = "Europa"
colnames(general_inflation_DF)[3] = "Kraje w strefie Euro"
colnames(general_inflation_DF)[4] = "Kraje poza strefy Euro"
colnames(general_inflation_DF)[5] = "Polska"

general_inflation_tidy_DF = pivot_longer(general_inflation_DF, -Date, names_to = "Country", values_to = "Inflation")

# ------ Periods ------

pandemic_start = "2020-01-09"
during_pandemic = "2021-01-09"
war_start = "2022-02-01"
now = "2022-11-24"


