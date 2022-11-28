# Libraries
library(ggplot2)
library(dplyr)

pl_inflation = filter(pl_inflation, !is.na(pl_inflation["Poland"]))

# Dummy data
data = data.frame(
  day = pl_inflation[,"Period.Unit"],
  value = pl_inflation[,"Poland"]
)

# Most basic bubble plot
p = ggplot(data, aes(x=day, y=value)) +
  geom_line() +
  geom_point(shape=21, color="black", fill="#69b3a2", size=1.5) +
  scale_x_date(date_labels = "%Y-%m", date_minor_breaks = "1 month")
p