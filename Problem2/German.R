source("./Problem2/commonData.R")
source("./Problem2/Plots.R")

# preparing hicd value

hicd_german <- read.csv("Problem2/data/HICD-Poland.csv")
hicd_german <- hicd_german[c("Period", "hicd")]
hicd_german["Period"] <- anytime::anydate(paste(hicd_german[, "Period"], 1))
hicd_german["Period"] <- format(as.Date(hicd_german$Period), format = "%Y")
hicd_german <- aggregate(hicd_german[-c(1)], hicd_german[c("Period")],mean, na.rm = TRUE)
hicd_german["Period"] <- as.numeric(as.character(hicd_german$Period))

# preparing avg gross

avg_gross_german <- read.csv("Problem2/data/AVG-gross-poland-2010-2021.csv", sep = ";")
colnames(avg_gross_german)[1] = "Period"
avg_gross_german$Period <- gsub("Q1 '", "20", avg_gross_german$Period)
avg_gross_german$Period <- gsub("Q2 '", "20", avg_gross_german$Period)
avg_gross_german$Period <- gsub("Q3 '", "20", avg_gross_german$Period)
avg_gross_german$Period <- gsub("Q4 '", "20", avg_gross_german$Period)
avg_gross_german <- avg_gross_german[c("Period", "National_economy")]
avg_gross_german$National_economy <- gsub(" ", "", avg_gross_german$National_economy)
avg_gross_german$National_economy <- gsub(",", ".", avg_gross_german$National_economy)
avg_gross_german$National_economy <- as.double(avg_gross_german$National_economy)
avg_gross_german <- aggregate(avg_gross_german[-c(1)], avg_gross_german[c("Period")], mean, na.rm = TRUE)
avg_gross_german$Period <- as.numeric(as.character(avg_gross_german$Period))

# preparing food data

food_german <- read.csv("Problem2/data/cmo_food_usd_poland_en 2014-2027.csv", sep = ",")
food_german <- filter(food_german, Chart=='Price per Unit')

# 2014-2022

years_german <- avg_gross_german$Period[5:13]
food_price_to_gross_german <- c()
market_basket_cost_german <- c()

for(year in years_german) {
  mb_german <- getMonthMarketBasketByYear(food_german, paste0("X", year, collapse = NULL))
  avg_year_gross_german <- as.double(filter(avg_gross_german, avg_gross_german$Period == year)$National_economy)
  market_basket_cost_german <- append(market_basket_cost_german, mb_german)
  food_price_to_gross_german <- append(food_price_to_gross_german, c(mb_german/avg_year_gross_german *100))
}

avg_food_price_german = data.frame(Period = years_german, Value = market_basket_cost_german)
avg_food_price_german$Period <- as.numeric(as.character(avg_food_price_german$Period))

food_price_to_gross_df_german = data.frame(Period = years_german, Value = food_price_to_gross_german)
hicd_german <- filter(hicd_german, hicd_german$Period >= 2014)

colnames(hicd_german)[2] = "Value"
data_value_plot(hicd_german, "Blue", "Blue", "German HICD", "HICD")
data_value_plot(avg_food_price_german, "Blue", "Blue", "German average food price", "avg. price per month")

colnames(avg_gross_german)[2] = "Value"
data_value_plot(avg_gross_german, "Blue", "Blue", "German average monthly salary gross", "Salary per dollar.")
data_value_plot(food_price_to_gross_df_german, "Blue", "Blue", "German average food price and monthly salary corellation", "HICD")
