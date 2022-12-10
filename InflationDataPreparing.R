library(anytime)
source("Utils.R")

eu_inflation <- read_eu_inflation()

eu_inflation_tidy <- pivot_longer(eu_inflation, -Date, names_to = "Country", values_to = "Inflation")

# create list of countries which are and aren`t in eu zone

eu_zone_countries <- c("Date", "Austria", "Belgium", "Cyprus", "Estonia", "Finland", "France", "Germany", "Greece", "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg", "Malta", "Netherlands", "Portugal", "Slovakia", "Slovenia", "Spain")
not_eu_zone_countries <- colnames(eu_inflation)[!(colnames(eu_inflation) %in% eu_zone_countries) & colnames(eu_inflation) != "EU"]
not_eu_zone_countries <- c(c("Date"), not_eu_zone_countries)

# data -> date | country | inflation

eu_countries_in_zone_inflation_tidy <- filter(eu_inflation_tidy, Country %in% eu_zone_countries)
eu_countries_NOT_in_zone_inflation_tidy <- filter(eu_inflation_tidy, Country %in% not_eu_zone_countries)
pl_inflation_tidy <- filter(eu_inflation_tidy, Country == "Poland")
eu_changingcomposition_inflation_tidy <- filter(eu_inflation_tidy, Country == "EU")

# data -> date | inflation

eu_countries_in_zone_inflation <- eu_inflation[eu_zone_countries]
eu_countries_NOT_in_zone_inflation <- eu_inflation[not_eu_zone_countries]
pl_inflation <- eu_inflation[c("Date", "Poland")]
eu_changingcomposition_inflation <- eu_inflation[c("Date", "EU")]

eu_inflation_DF <- data.frame(
  eu_inflation[1],
  Inflation <- rowMeans(eu_inflation[2:29])
)

eu_countries_in_zone_inflation_DF <- data.frame(
  eu_inflation[1],
  Inflation = rowMeans(eu_countries_in_zone_inflation[2:20])
)

eu_countries_NOT_in_zone_inflation_DF <- data.frame(
  eu_inflation[1],
  Inflation = rowMeans(eu_countries_NOT_in_zone_inflation[2:9])
)

pl_inflation_DF <- data.frame(
  eu_inflation[1],
  Inflation = rowMeans(pl_inflation[2])
)

general_inflation_DF <- data.frame(
  eu_inflation[1],
  "Europe" = eu_inflation_DF[2],
  "Eu_countries" = eu_countries_in_zone_inflation_DF[2],
  "Other_countries" = eu_countries_NOT_in_zone_inflation_DF[2],
  "Poland" = pl_inflation_DF[2]
)

colnames(general_inflation_DF)[2] <- "Europe"
colnames(general_inflation_DF)[3] <- "In the Eurozone"
colnames(general_inflation_DF)[4] <- "Outside the Eurozone"
colnames(general_inflation_DF)[5] <- "Poland"

general_inflation_tidy_DF <- pivot_longer(general_inflation_DF, -Date, names_to = "Country", values_to = "Inflation")


