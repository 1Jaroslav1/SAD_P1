source("./Problem2/commonData.R")
source("./Problem2/Plots.R")

# preparing hicp value

hicp_czechia <- read.csv("Problem2/data/HICD-Czechia.csv")
hicp_czechia <- hicp_czechia[c("Period", "hicp")]
hicp_czechia["Period"] <- anytime::anydate(paste(hicp_czechia[, "Period"], 1))
hicp_czechia["Period"] <- format(as.Date(hicp_czechia$Period), format = "%Y")
hicp_czechia <- aggregate(hicp_czechia[-c(1)], hicp_czechia[c("Period")],mean, na.rm = TRUE)
hicp_czechia["Period"] <- as.numeric(as.character(hicp_czechia$Period))

# preparing avg gross

avg_gross_czechia <- read.csv("Problem2/data/AVG-gross-czechia-2014-2022.csv", sep = ";")
colnames(avg_gross_czechia)[1] = "Period"
avg_gross_czechia$Period <- gsub("Q1 '", "20", avg_gross_czechia$Period)
avg_gross_czechia$Period <- gsub("Q2 '", "20", avg_gross_czechia$Period)
avg_gross_czechia$Period <- gsub("Q3 '", "20", avg_gross_czechia$Period)
avg_gross_czechia$Period <- gsub("Q4 '", "20", avg_gross_czechia$Period)
avg_gross_czechia <- avg_gross_czechia[c("Period", "Value")]
avg_gross_czechia$Value <- gsub(" ", "", avg_gross_czechia$Value)
avg_gross_czechia$Value <- gsub(",", ".", avg_gross_czechia$Value)
avg_gross_czechia$Value <- as.double(avg_gross_czechia$Value)
avg_gross_czechia <- aggregate(avg_gross_czechia[-c(1)], avg_gross_czechia[c("Period")], mean, na.rm = TRUE)
avg_gross_czechia$Period <- as.integer(as.character(avg_gross_czechia$Period))

# preparing food data

food_czechia <- read.csv("Problem2/data/cmo_food_usd_czechia_en 2014-2027.csv", sep = ",")
food_czechia <- filter(food_czechia, Chart=='Price per Unit')

# 2014-2022

years_czechia <- avg_gross_czechia$Period[1:9]
food_price_to_gross_czechia <- c()
market_basket_cost_czechia <- c()

for(year in years_czechia) {
  mb_czechia <- getMonthMarketBasketByYear(food_czechia, paste0("X", year, collapse = NULL))
  avg_year_gross_czechia <- as.double(filter(avg_gross_czechia, avg_gross_czechia$Period == year)$Value)
  market_basket_cost_czechia <- append(market_basket_cost_czechia, mb_czechia)
  food_price_to_gross_czechia <- append(food_price_to_gross_czechia, c(mb_czechia/avg_year_gross_czechia *100))
}

avg_food_price_czechia = data.frame(Period = years_czechia, Value = market_basket_cost_czechia)
avg_food_price_czechia$Period <- as.numeric(as.character(avg_food_price_czechia$Period))

food_price_to_gross_df_czechia = data.frame(Period = years_czechia, Value = food_price_to_gross_czechia)
hicp_czechia <- filter(hicp_czechia, hicp_czechia$Period >= 2014)

colnames(hicp_czechia)[2] = "Value"
data_value_plot(hicp_czechia, "Blue", "Blue", "Czechia Inflation", "Inflation (%)")
data_value_plot(avg_food_price_czechia, "Blue", "Blue", "Czechia average food cost per month", "Price in $")

colnames(avg_gross_czechia)[2] = "Value"
data_value_plot(avg_gross_czechia, "Blue", "Blue", "Czechia average salary per month (full-time)", "Salary in $")
data_value_plot(food_price_to_gross_df_czechia, "Blue", "Blue", "Czechia ratio of salary to the price of food", "Ratio in %")

cor.test(hicp_czechia$Value, avg_food_price_czechia$Value, method = "pearson")
cor.test(hicp_czechia$Value, avg_gross_czechia$Value, method = "pearson")
