source("./Problem2/commonData.R")
source("./Problem2/Plots.R")

# preparing hicp value

hicp_german <- read.csv("Problem2/data/HICD-German.csv")
hicp_german <- hicp_german[c("Period", "hicp")]
hicp_german["Period"] <- anytime::anydate(paste(hicp_german[, "Period"], 1))
hicp_german["Period"] <- format(as.Date(hicp_german$Period), format = "%Y")
hicp_german <- aggregate(hicp_german[-c(1)], hicp_german[c("Period")],mean, na.rm = TRUE)
hicp_german["Period"] <- as.numeric(as.character(hicp_german$Period))

# preparing avg gross

avg_gross_german <- read.csv("Problem2/data/AVG-gross-german-2014-2022.csv", sep = ";")
colnames(avg_gross_german)[1] = "Period"
avg_gross_german$Period <- gsub("Q1 '", "20", avg_gross_german$Period)
avg_gross_german$Period <- gsub("Q2 '", "20", avg_gross_german$Period)
avg_gross_german$Period <- gsub("Q3 '", "20", avg_gross_german$Period)
avg_gross_german$Period <- gsub("Q4 '", "20", avg_gross_german$Period)
avg_gross_german <- avg_gross_german[c("Period", "Value")]
avg_gross_german$Value <- gsub(" ", "", avg_gross_german$Value)
avg_gross_german$Value <- gsub(",", ".", avg_gross_german$Value)
avg_gross_german$Value <- as.double(avg_gross_german$Value)
avg_gross_german <- aggregate(avg_gross_german[-c(1)], avg_gross_german[c("Period")], mean, na.rm = TRUE)
avg_gross_german$Period <- as.integer(as.character(avg_gross_german$Period))

# preparing food data

food_german <- read.csv("Problem2/data/cmo_food_usd_germany_en 2014-2027.csv", sep = ",")
food_german <- filter(food_german, Chart=='Price per Unit')

# 2014-2022

years_german <- avg_gross_german$Period[1:9]
food_price_to_gross_german <- c()
market_basket_cost_german <- c()

for(year in years_german) {
  mb_german <- getMonthMarketBasketByYear(food_german, paste0("X", year, collapse = NULL))
  avg_year_gross_german <- as.double(filter(avg_gross_german, avg_gross_german$Period == year)$Value)
  market_basket_cost_german <- append(market_basket_cost_german, mb_german)
  food_price_to_gross_german <- append(food_price_to_gross_german, c(mb_german/avg_year_gross_german *100))
}

avg_food_price_german = data.frame(Period = years_german, Value = market_basket_cost_german)
avg_food_price_german$Period <- as.numeric(as.character(avg_food_price_german$Period))

food_price_to_gross_df_german = data.frame(Period = years_german, Value = food_price_to_gross_german)
hicp_german <- filter(hicp_german, hicp_german$Period >= 2014)

colnames(hicp_german)[2] = "Value"
data_value_plot(hicp_german, "Blue", "Blue", "German Inflation", "Inflation (%)")
data_value_plot(avg_food_price_german, "Blue", "Blue", "German average food cost per month", "Price in $")

colnames(avg_gross_german)[2] = "Value"
data_value_plot(avg_gross_german, "Blue", "Blue", "German average salary per month (full-time)", "Salary in $")
data_value_plot(food_price_to_gross_df_german, "Blue", "Blue", "German ratio of salary to the price of food", "Ratio in %")

cor.test(hicp_german$Value, avg_food_price_german$Value, method = "pearson")
cor.test(hicp_german$Value, avg_gross_german$Value, method = "pearson")
