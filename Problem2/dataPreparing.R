library(hash)

# preparing hicd value

hicd_pl <- read.csv("Problem2/data/HICD-Poland.csv")
hicd_pl <- hicd_pl[c("Period", "hicd")]
hicd_pl["Period"] <- anytime::anydate(paste(hicd_pl[, "Period"], 1))
hicd_pl["Period"] <- format(as.Date(hicd_pl$Period), format = "%Y")
hicd_pl <- aggregate(hicd_pl[-c(1)], hicd_pl[c("Period")],mean, na.rm = TRUE)

# preparing avg gross

avg_gross_pl <- read.csv("Problem2/data/AVG-gross-poland-2010-2021.csv", sep = ";")
colnames(avg_gross_pl)[1] = "Period"
avg_gross_pl$Period <- gsub("Q1 '", "20", avg_gross_pl$Period)
avg_gross_pl$Period <- gsub("Q2 '", "20", avg_gross_pl$Period)
avg_gross_pl$Period <- gsub("Q3 '", "20", avg_gross_pl$Period)
avg_gross_pl$Period <- gsub("Q4 '", "20", avg_gross_pl$Period)
avg_gross_pl <- avg_gross_pl[c("Period", "National_economy")]
avg_gross_pl$National_economy <- gsub(" ", "", avg_gross_pl$National_economy)
avg_gross_pl$National_economy <- gsub(",", ".", avg_gross_pl$National_economy)
avg_gross_pl$National_economy <- as.double(avg_gross_pl$National_economy)
avg_gross_pl <- aggregate(avg_gross_pl[-c(1)], avg_gross_pl[c("Period")], mean, na.rm = TRUE)

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

# unit -> 1kg or 1l
market_basket = hash()

# Milk
market_basket[["Milk"]] <- 5
market_basket[["Butter"]] <- 0.4
market_basket[["Cheese"]] <- 0.3
market_basket[["Other Dairy Products & Eggs"]] <- 1

# Vegetables
market_basket[["Potatoes"]] <- 4.8
market_basket[["Cabbage Vegetables"]] <- 2.3
market_basket[["Tomatoes"]] <- 1
market_basket[["Root Vegetables & Mushrooms"]] <- 1
market_basket[["Onions"]] <- 0.5

# Fruits
market_basket[["Apples & Pears"]] <- 1
market_basket[["Bananas"]] <- 1
market_basket[["Citrus Fruits"]] <- 0.5
market_basket[["Berries & Grapes"]] <- 0.5

# Bread & Cereal Products
market_basket[["Bread"]] <- 5
market_basket[["Breakfast Cereals"]] <- 1
market_basket[["Rice"]] <- 1
market_basket[["Pasta"]] <- 1

# Meat
market_basket[["Pork"]] <- 0.7
market_basket[["Beef & Veal"]] <- 1.2
market_basket[["Poultry"]] <- 1
market_basket[["Sausages"]] <- 1
market_basket[["Ham & Bacon"]] <- 0.2

# Fish
market_basket[["Fresh Fish"]] <- 0.6
market_basket[["Processed Fish & Seafood"]] <- 0.3

# Other
market_basket[["Eggs"]] <- 2
market_basket[["Edible Oils"]] <- 0.6
market_basket[["Margarine"]] <- 0.2
market_basket[["Sugar"]] <- 2
market_basket[["Chocolate Confectionery"]] <- 0.5

getManthMarketBasketByYear = function(food_pl, exchage_values, market_basket, year) {
  keys <- keys(market_basket)

  food <- data.frame(food_pl$Name)
  colnames(food)[1] = "Name"
  food["Price"] = food_pl[year]
  sum <- 0

  for (k in keys) {
    sum = sum + as.double(filter(food, food$Name == k)$Price) * values(market_basket, keys=k)
  }

  return(sum *  as.double(filter(exchage_values, exchage_values$Date == substring(year, 2))$Value))
}

# 2014-2022

years <- avg_gross_pl$Period[5:13]
covariance <- c()

for(year in years) {
  market_basket_cost <- getManthMarketBasketByYear(food_pl, exchage_values, market_basket, paste0("X",year, collapse = NULL))
  avg_year_gross <- as.double(filter(avg_gross_pl, avg_gross_pl$Period == year)$National_economy)
  print(market_basket_cost/avg_year_gross *100)
  covariance <- append(covariance, c(market_basket_cost/avg_year_gross *100))
}
covariance

