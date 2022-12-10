library(anytime)

# ------ Periods ------
analysis_start <- "2017-01-01"
pandemic_start <- "2020-01-01"
during_pandemic <- "2021-01-01"
war_start <- "2022-02-01"
now <- "2022-11-24"

read_eu_inflation <- function() {
  eu_inflation = read.csv("./Problem1/data/EU_Inflation_HICP_data.csv")

  # Change columns name

  colnames(eu_inflation)[1] = "Date"
  colnames(eu_inflation)[7] = "EU"

  eu_inflation = na.omit(eu_inflation)
  eu_inflation[, 2:29] = sapply(eu_inflation[, 2:29], as.double)
  eu_inflation["Date"] = anytime::anydate(paste(eu_inflation[, "Date"], 1))
  return(eu_inflation)
}