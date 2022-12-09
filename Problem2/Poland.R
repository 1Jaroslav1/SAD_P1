source("./Problem2/commonData.R")
source("./Problem2/Plots.R")

# preparing hicp value

hicp_pl <- read.csv("Problem2/data/HICD-Poland.csv")
hicp_pl <- hicp_pl[c("Period", "hicp")]
hicp_pl["Period"] <- anytime::anydate(paste(hicp_pl[, "Period"], 1))
hicp_pl["Period"] <- format(as.Date(hicp_pl$Period), format = "%Y")
hicp_pl <- aggregate(hicp_pl[-c(1)], hicp_pl[c("Period")],mean, na.rm = TRUE)
hicp_pl["Period"] <- as.numeric(as.character(hicp_pl$Period))

# preparing avg gross

avg_gross_pl_zl <- read.csv("Problem2/data/AVG-gross-poland-2010-2021.csv", sep = ";")
colnames(avg_gross_pl_zl)[1] = "Period"
avg_gross_pl_zl$Period <- gsub("Q1 '", "20", avg_gross_pl_zl$Period)
avg_gross_pl_zl$Period <- gsub("Q2 '", "20", avg_gross_pl_zl$Period)
avg_gross_pl_zl$Period <- gsub("Q3 '", "20", avg_gross_pl_zl$Period)
avg_gross_pl_zl$Period <- gsub("Q4 '", "20", avg_gross_pl_zl$Period)
avg_gross_pl_zl <- avg_gross_pl_zl[c("Period", "National_economy")]
avg_gross_pl_zl$National_economy <- gsub(" ", "", avg_gross_pl_zl$National_economy)
avg_gross_pl_zl$National_economy <- gsub(",", ".", avg_gross_pl_zl$National_economy)
avg_gross_pl_zl$National_economy <- as.double(avg_gross_pl_zl$National_economy)
avg_gross_pl_zl <- aggregate(avg_gross_pl_zl[-c(1)], avg_gross_pl_zl[c("Period")], mean, na.rm = TRUE)
avg_gross_pl_zl$Period <- as.numeric(as.character(avg_gross_pl_zl$Period))

# preparing food data

food_pl <- read.csv("Problem2/data/cmo_food_usd_poland_en 2014-2027.csv", sep = ",")
food_pl <- filter(food_pl, Chart=='Price per Unit')

# dollar -> zloty

exchage <- read.csv("Problem2/data/Polish_Zloty.csv", sep = ",")
exchage_values <- data.frame(exchage$Close)
colnames(exchage_values)[1] = "Value"
exchage_values["Date"] = as.character(format(as.Date(exchage$Date, tryFormats="%m/%d/%Y"), format = "%Y"))
exchage_values$Date <- gsub("00", "20", exchage_values$Date)
exchage_values$Value <- gsub(",", ".", exchage_values$Value)
exchage_values$Value <- as.double(exchage_values$Value)
exchage_values <- aggregate(exchage_values[-c(2)], exchage_values[c("Date")], mean, na.rm = TRUE)

avg_gross_pl$Value <- (avg_gross_pl_zl$National_economy / exchage_values$Value)

# 2014-2022

years_pl <- avg_gross_pl$Period[1:9]
food_price_to_gross_pl <- c()
market_basket_cost_pl <- c()

for(year in years_pl) {
  mb_pl <- getMonthMarketBasketByYear(food_pl, paste0("X", year, collapse = NULL)) #*  as.double(filter(exchage_values, exchage_values$Date == year)$Value)
  avg_year_gross_pl <- as.double(filter(avg_gross_pl, avg_gross_pl$Period == year)$Value)
  market_basket_cost_pl <- append(market_basket_cost_pl, mb_pl)
  food_price_to_gross_pl <- append(food_price_to_gross_pl, c(mb_pl/avg_year_gross_pl *100))
}

avg_food_price_pl = data.frame(Period = years_pl, Value = market_basket_cost_pl)
avg_food_price_pl$Period <- as.numeric(as.character(avg_food_price_pl$Period))

food_price_to_gross_df_pl = data.frame(Period = years_pl, Value = food_price_to_gross_pl)
hicp_pl <- filter(hicp_pl, hicp_pl$Period >= 2014)

colnames(hicp_pl)[2] = "Value"
data_value_plot(hicp_pl, "Blue", "Blue", "Poland Inflation", "Inflation (%)")
data_value_plot(avg_food_price_pl, "Green", "Green", "Poland average food cost per month", "Price in $")

colnames(avg_gross_pl)[2] = "Value"
data_value_plot(avg_gross_pl, "Orange", "Orange", "Poland average salary per month (full-time)", "Salary in $")
data_value_plot(food_price_to_gross_df_pl, "Red", "Red", "Poland ratio of salary to the price of food", "Ratio in %")

avg_food_price_pl_zl = data.frame(Period = years_pl, Value = market_basket_cost_pl * as.double(exchage_values$Value))
avg_gross_pl_zl <- data.frame(Period = years_pl, Value = avg_gross_pl$Value * as.double(exchage_values$Value))

data_value_plot(avg_food_price_pl_zl, "Green", "Green", "Poland average food cost per month", "Price in zł")
data_value_plot(avg_gross_pl_zl, "Orange", "Orange", "Poland average salary per month (full-time)", "Salary in zł")


cor.test(hicp_pl$Value, avg_food_price_pl_zl$Value, method = "pearson")
cor.test(hicp_pl$Value, avg_gross_pl_zl$Value, method = "pearson")
cor.test(hicp_pl$Value, food_price_to_gross_df_pl$Value, method = "pearson")

cor_visualization(hicp_pl, avg_food_price_pl_zl, "Inflation", "Food Cost")
cor_visualization(hicp_pl, avg_gross_pl_zl, "Inflation", "Salary")
cor_visualization(hicp_pl, food_price_to_gross_df_pl, "Inflation", "Ratio")
