library(hash)

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

getMonthMarketBasketByYear = function(food_pl, year) {
  keys <- keys(market_basket)

  food <- data.frame(food_pl$Name)
  colnames(food)[1] = "Name"
  food["Price"] = food_pl[year]
  sum <- 0

  for (k in keys) {
    sum = sum + as.double(filter(food, food$Name == k)$Price) * values(market_basket, keys=k)
  }
  return(sum)
}
