library(tidyverse)
library(anytime)
source("Utils.R")

read_eu_depositis <- function() {
  eu_deposits <- read.csv("data/EU_deposits_1yr_data.csv")

  # filter out columns with null values for selected time periods
  empty_columns <- c("Lithuania..Lithuanian.litas",
                     "Slovenia..Slovenian.tolar",
                     "Slovakia..Slovak.koruna",
                     "Estonia..Estonian.kroon",
                     "Latvia..Latvian.lats",
                     "Malta..Maltese.lira")
  columns <- colnames(eu_deposits)
  non_null_cols <- columns[!(columns %in% empty_columns)]

  eu_deposits %>%
    mutate(
      Period.Unit. = anytime::anydate(paste(Period.Unit., 1))
    ) %>%
    filter(
      Period.Unit. > analysis_start
    ) %>%
    select(all_of(non_null_cols)) %>%
    mutate(across(!Period.Unit., as.double)) %>%
    rename(
      Date = Period.Unit.,
      Austria = Austria..Euro,
      Belgium = Belgium..Euro,
      Bulgaria = Bulgaria..Bulgarian.lev,
      Cyprus = Cyprus..Euro,
      Czech.Republic = Czech.Republic..Czech.koruna,
      Germany = Germany..Euro,
      Denmark = Denmark..Danish.krone,
      Estonia = Estonia..Euro,
      Spain = Spain..Euro,
      Finland = Finland..Euro,
      France = France..Euro,
      Greece = Greece..Euro,
      Croatia = Croatia..Croatian.kuna,
      Hungary = Hungary..Hungarian.forint,
      Ireland = Ireland..Euro,
      Italy = Italy..Euro,
      Lithuania = Lithuania..Euro,
      Luxembourg = Luxembourg..Euro,
      Latvia = Latvia..Euro,
      Malta = Malta..Euro,
      Netherlands = Netherlands..Euro,
      Poland = Poland..Polish.zloty,
      Portugal = Portugal..Euro,
      Romania = Romania..Romanian.leu,
      Sweden = Sweden..Swedish.krona,
      Slovenia = Slovenia..Euro,
      Slovakia = Slovakia..Euro
    )
}
