eu_inflation = read.csv("./EU_Inflation_HICP_data.csv")

colnames(eu_inflation)[1] = "Date"
colnames(eu_inflation)[7] = "EU"

eu_inflation = na.omit(eu_inflation)
eu_inflation[, 2:29] = sapply(eu_inflation[, 2:29], as.double)
eu_inflation["Date"] = anytime::anydate(paste(eu_inflation[,"Date"], 1))

eu_inflation_tidy <- pivot_longer(eu_inflation, -Date, names_to = "Country", values_to = "Inflation")

eu_zone_countries = c("Date", "Austria","Belgium","Cyprus","Estonia","Finland","France","Germany","Greece","Ireland","Italy","Latvia","Lithuania","Luxembourg","Malta","Netherlands","Portugal","Slovakia","Slovenia","Spain")
not_eu_zone_countries = colnames(eu_inflation)[!(colnames(eu_inflation) %in% eu_zone_countries) & colnames(eu_inflation) != "EU..changingcomposition."]
not_eu_zone_countries = c(c("Date"), not_eu_zone_countries)

eu_countries_in_zone_inflation = filter(eu_inflation_tidy, Country %in% eu_zone_countries)
eu_countries_NOT_in_zone_inflation = filter(eu_inflation_tidy, Country %in% not_eu_zone_countries)
pl_inflation = filter(eu_inflation_tidy, Country == "Poland")
eu_changingcomposition_inflation = filter(eu_inflation_tidy, Country == "EU")

