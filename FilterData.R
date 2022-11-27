eu_inflation = read.csv("./EU_Inflation_HICP_data.csv")
eu_zone_countries = c("Period.Unit", "Austria","Belgium","Cyprus","Estonia","Finland","France","Germany","Greece","Ireland","Italy","Latvia","Lithuania","Luxembourg","Malta","Netherlands","Portugal","Slovakia","Slovenia","Spain")
not_eu_zone_countries = colnames(eu_inflation)[!(colnames(eu_inflation) %in% eu_zone_countries) & colnames(eu_inflation) != "EU..changingcomposition."]
not_eu_zone_countries = c(c("Period.Unit"), not_eu_zone_countries)

eu_countries_in_zone_inflation = eu_inflation[eu_zone_countries]
eu_countries_NOT_in_zone_inflation = eu_inflation[not_eu_zone_countries]
pl_inflation = eu_inflation[c("Period.Unit", "Poland")]
eu_changingcomposition_inflation = eu_inflation[c("Period.Unit", "EU..changingcomposition.")]
