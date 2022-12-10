source("./Problem1/Utils.R")
source("./Problem1/DepositDataPreparation.R")
source("./Problem1/Functions.R")

# helper functions
inflation_vs_deposits_plot <- function(country_name, deposits, inflation, filter_range, plot_title) {
  inflation_for_country <- inflation %>%
    get_for_country_in_time_range(filter_range, country_name) %>%
    mutate(
      Metric = "Inflation"
    )

  deposits_for_country <- deposits %>%
    get_for_country_in_time_range(filter_range, country_name) %>%
    mutate(
      Metric = "Deposits"
    )
  combined <- union_all(inflation_for_country, deposits_for_country)
  draw_stacked_metrics(combined, paste(country_name, ": ", plot_title))
}

statistics_for_all_countries <- function(df, filter_range) {
  df %>%
    filter_by_time(filter_range) %>%
    pivot_longer(-Date, names_to = "Country", values_to = "Values") %>%
    na.omit() %>%
    group_by(Country) %>%
    summarise(
      mean = mean(Values),
      var = var(Values),
      median = median(Values)
    )
}

aggregated_deposit_and_inflation_barplot <- function(deposits, inflation, filter_range) {
  deposit_stats <- statistics_for_all_countries(deposits, filter_range)
  inflation_stats <- statistics_for_all_countries(inflation, filter_range)

  combined_mean <- union_all(filter_by_metric(inflation_stats, "mean", "Inflation"),
                             filter_by_metric(deposit_stats, "mean", "Deposits"))
  draw_aggregated_barplots(combined_mean,
                           paste("Inflation and deposits -", filter_range[3]))
}

caluclate_and_plot_real_deposit_rate <- function(deposits, inflation, filter_range) {
  deposits_for_range <- deposits %>% filter_by_time(c(filter_range[1], filter_range[2]))
  inflation_for_range <- inflation %>% filter_by_time(c(filter_range[1], filter_range[2]))

  real_deposit_percent <- deposits_for_range %>% select(Date)
  for (c in countries) {
    for (row_idx in seq_len(nrow(deposits_for_range))) {
      inf_per <- inflation_for_range[row_idx, c]
      dep_per <- deposits_for_range[row_idx, c]
      real_deposit_percent[row_idx, c] <- (dep_per - inf_per) / (1 + inf_per)
    }
  }
  deposits_long <- pivot_longer(real_deposit_percent, -Date, names_to = "Country", values_to = "Deposit")

  draw_deposit_rate <- function(data_frame, plot_name) {
    ggplot(data_frame, aes(x = Date, y = Deposit, group = Country, color = Country)) +
      geom_line(aes(color = Country), size = 1.5) +
      #scale_x_date(date_labels = "%Y-%m", date_minor_breaks = "1 month") +
      guides(fill = guide_legend(title = NULL)) +
      xlab("Date") +
      ylab("Deposit rate [%]") +
      ggtitle(paste("Real deposit rate -", plot_name)) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 25, hjust = 0.5, vjust = 0.5, face = 'bold', margin = margin(20, 0, 20, 0)),
        axis.title = element_text(size = 20),
        axis.title.x = element_text(margin = margin(20, 0, 20, 0)),
        axis.title.y = element_text(margin = margin(0, 20, 0, 20)),
        axis.text = element_text(
          size = 15,
          face = 3
        )
      )
  }
  ggsave(file = paste0("./Problem1/plots/1-2/deposit-rate", filter_range[1], "_", filter_range[2],".eps"), plot = draw_deposit_rate(deposits_long, filter_range[3]))
}


countries <- c("Poland", "Germany", "France", "Czechia", "Romania", "Hungary")
eu_inflation <- read_eu_inflation() %>%
  rename(Czechia = Czech.Republic) %>%
  select(all_of(countries), Date)
eu_deposits <- read_eu_depositis() %>%
  rename(Czechia = Czech.Republic) %>%
  select(all_of(countries), Date)
filter_ranges <- list(c(analysis_start, pandemic_start, "pre pandemic"),
                      c(pandemic_start, during_pandemic, "pandemic peak"),
                      c(during_pandemic, war_start, "before war"),
                      c(war_start, now, "since war started"))

for (filter_range in filter_ranges) {
  title <- filter_range[3]
  barplot_inf <- aggregated_deposit_and_inflation_barplot(eu_deposits, eu_inflation, filter_range)
  ggsave(file = paste0("./Problem1/plots/1-2/", "infl_dep_barplot", filter_range[1], "_", filter_range[2], ".eps"), plot = barplot_inf, width = 10, height = 8)
}

# deposit values
#pre pandemic - now
time_range <- c(analysis_start, now)
deposits_start_of_year <- eu_deposits %>%
  filter_by_time(time_range) %>%
  filter(as.numeric(strftime(Date, "%m")) == 1) %>%
  arrange(Date)
deposits_change_over_time <- deposits_start_of_year %>%
  select(Date) %>%
  add_row(Date = as.Date('2023-01-01'))

for (c in countries) {
  starting_deposit <- 50000
  deposit_values <- seq_len(nrow(deposits_start_of_year) + 1)
  deposit_values[1] <- starting_deposit
  for (idx in 1:nrow(deposits_start_of_year) + 1) {
    deposit_values[idx] <- deposit_values[idx - 1] * (1 + deposits_start_of_year[idx - 1, c] / 100)
  }
  deposits_change_over_time[, c] <- deposit_values
}

deposits_long <- pivot_longer(deposits_change_over_time, -Date, names_to = "Country", values_to = "Deposit")

draw_deposits <- function(data_frame, title) {
  ggplot(data_frame, aes(x = Date, y = Deposit, group = Country, color = Country)) +
    geom_line(aes(color = Country), size = 1.5) +
    #scale_x_date(date_labels = "%Y-%m", date_minor_breaks = "1 month") +
    guides(fill = guide_legend(title = NULL)) +
    xlab("Date") +
    ylab("Deposit value") +
    ggtitle(title) +
    theme(
      plot.title = element_text(size = 25, hjust = 0.5, vjust = 0.5, face = 'bold', margin = margin(20, 0, 20, 0)),
      axis.title = element_text(size = 20),
      axis.title.x = element_text(margin = margin(20, 0, 20, 0)),
      axis.title.y = element_text(margin = margin(0, 20, 0, 20)),
      axis.text = element_text(
        size = 15,
        face = 3
      )
    ) +
    theme_minimal()
}

deposit_change <- draw_deposits(deposits_long, "Depozyty")
ggsave(file = "./Problem1/plots/1-2/deposit-change.eps", plot = deposit_change)

#wyliczenie realnej stopy procentowej
caluclate_and_plot_real_deposit_rate

for (filter_range in filter_ranges){
  caluclate_and_plot_real_deposit_rate(eu_deposits, eu_inflation, filter_range)
}