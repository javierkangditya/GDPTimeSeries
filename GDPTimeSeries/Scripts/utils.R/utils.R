# ============================================================
# utils.R – GDP Time Series Analysis (1980-2028)
# ============================================================
# Author       : Javier Kangditya
# Project      : Time Series Forecasting & Analysis of Global GDP
# Dataset      : IMF's GDP Data: 1980-2028 Global Trends
# ============================================================

# =============================
# 1. Load Libraries
# =============================
library(readxl)
library(tidyverse)
library(tsibble)
library(knitr)
library(writexl)
library(ggplot2)
library(forecast)
library(zoo)
library(tseries)

# =============================
# 2. Data Cleaning
# =============================
# Load dataset
gdp_data <- read_excel("Data/Raw/gdp.xlsx")

# Remove duplicates and missing values (drop 0s)
gdp_data <- gdp_data %>%
  distinct() %>%
  drop_na() %>%
  filter(if_all(`1980`:`2028`, ~ . != 0))

# Pivot from wide to long
gdp_long <- gdp_data %>%
  pivot_longer(
    cols = `1980`:`2028`,
    names_to = "Year",
    values_to = "GDP_per_capita"
  ) %>%
  mutate(Year = as.integer(Year))

# Convert to tsibble
gdp_tsibble <- gdp_long %>%
  as_tsibble(
    key = Country,
    index = Year
  )

# Save processed long dataset
write_xlsx(gdp_long, "Data/Processed/gdp_long.xlsx")

# =============================
# 3. Exploratory Data Analysis
# =============================
# Selected ASEAN-5 countries
selected_countries <- c("Indonesia", "Malaysia", "Singapore", "Thailand", "Philippines")
gdp_tsibble_asean <- gdp_tsibble %>% filter(Country %in% selected_countries)

# Line plot – GDP per capita over time
p1 <- ggplot(gdp_tsibble_asean, aes(x = Year, y = GDP_per_capita, color = Country)) +
  geom_line(size = 1) +
  geom_smooth(method = "loess", se = FALSE, linetype = "dashed") +
  labs(title = "GDP per Capita Over Time – ASEAN-5", x = "Year", y = "GDP per Capita (USD)") +
  theme_minimal() + theme(legend.position = "bottom")
print(p1)

# Boxplot per country
p2 <- ggplot(gdp_tsibble_asean, aes(x = Country, y = GDP_per_capita, fill = Country)) +
  geom_boxplot() +
  labs(title = "Distribution of GDP per Capita – ASEAN-5", y = "GDP per Capita (USD)", x = "") +
  theme_minimal() + theme(legend.position = "none")
print(p2)

# Histogram / Density per country
p3 <- ggplot(gdp_tsibble_asean, aes(x = GDP_per_capita, fill = Country)) +
  geom_histogram(alpha = 0.6, position = "identity", bins = 30) +
  labs(title = "Histogram of GDP per Capita – ASEAN-5", x = "GDP per Capita (USD)", y = "Count") +
  theme_minimal()
print(p3)

# Yearly growth rate
gdp_growth_asean <- gdp_tsibble_asean %>%
  group_by(Country) %>%
  arrange(Year) %>%
  mutate(GDP_growth = (GDP_per_capita / lag(GDP_per_capita) - 1) * 100) %>%
  filter(!is.na(GDP_growth))

p4 <- ggplot(gdp_growth_asean, aes(x = Year, y = GDP_growth, color = Country)) +
  geom_line(size = 1) +
  labs(title = "Yearly GDP Growth – ASEAN-5", x = "Year", y = "GDP Growth (%)") +
  theme_minimal() + theme(legend.position = "bottom")
print(p4)

# Save EDA plots
ggsave("Outputs/Plots/GDP_per_Capita_Line.png", plot = p1, width = 10, height = 6, dpi = 300)
ggsave("Outputs/Plots/GDP_Boxplot.png", plot = p2, width = 8, height = 6, dpi = 300)
ggsave("Outputs/Plots/GDP_Histogram.png", plot = p3, width = 8, height = 6, dpi = 300)
ggsave("Outputs/Plots/GDP_Growth_Line.png", plot = p4, width = 10, height = 6, dpi = 300)

# =============================
# 4. Feature Engineering – Time Series
# =============================
gdp_features <- gdp_tsibble_asean %>%
  arrange(Country, Year) %>%
  group_by(Country) %>%
  mutate(
    lag_1 = lag(GDP_per_capita, 1),
    lag_2 = lag(GDP_per_capita, 2),
    diff_1 = GDP_per_capita - lag(GDP_per_capita, 1),
    GDP_growth = (GDP_per_capita / lag(GDP_per_capita, 1) - 1) * 100,
    log_GDP = log(GDP_per_capita),
    GDP_ma_5 = zoo::rollmean(GDP_per_capita, k = 5, fill = NA)
  ) %>%
  filter(!is.na(lag_1))

# Preview sample
kable(gdp_features %>% slice(1:10), caption = "Sample Feature Engineered Data – ASEAN-5")

# Save feature engineered data
write_xlsx(as.data.frame(gdp_features), "Data/Processed/gdp_features_asean.xlsx")

# =============================
# 5. Forecasting – ARIMA
# =============================
forecast_horizon <- 5
forecast_results <- list()
forecast_plots <- list()
countries <- unique(gdp_features$Country)

for(ctry in countries){
  df <- gdp_features %>% filter(Country == ctry) %>% arrange(Year)
  ts_data <- ts(df$GDP_per_capita, start = min(df$Year), frequency = 1)
  fit <- auto.arima(ts_data)
  fc <- forecast(fit, h = forecast_horizon)
  
  forecast_df <- data.frame(
    Country = ctry,
    Year = (max(df$Year)+1):(max(df$Year)+forecast_horizon),
    Forecast_GDP = as.numeric(fc$mean)
  )
  forecast_results[[ctry]] <- forecast_df
  
  p <- autoplot(fc) +
    autolayer(ts_data, series = "Actual") +
    ggtitle(paste("GDP Forecast –", ctry)) +
    xlab("Year") + ylab("GDP per Capita") +
    theme_minimal()
  forecast_plots[[ctry]] <- p
  ggsave(paste0("Outputs/Forecasts/GDP_Forecast_", ctry, ".png"), plot = p, width = 10, height = 6, dpi = 300)
}

all_forecasts <- do.call(rbind, forecast_results)
write_xlsx(all_forecasts, "Outputs/Forecasts/GDP_ARIMA_Forecast.xlsx")

# =============================
# 6. Forecasting – ETS
# =============================
ets_results <- list()
ets_plots <- list()

for(ctry in countries){
  df <- gdp_features %>% filter(Country == ctry) %>% arrange(Year)
  ts_data <- ts(df$GDP_per_capita, start = min(df$Year), frequency = 1)
  fit <- ets(ts_data)
  fc <- forecast(fit, h = forecast_horizon)
  
  forecast_df <- data.frame(
    Country = ctry,
    Year = (max(df$Year)+1):(max(df$Year)+forecast_horizon),
    Forecast_GDP_ETS = as.numeric(fc$mean)
  )
  ets_results[[ctry]] <- forecast_df
  
  p <- autoplot(fc) +
    autolayer(ts_data, series = "Actual") +
    ggtitle(paste("ETS GDP Forecast –", ctry)) +
    xlab("Year") + ylab("GDP per Capita") +
    theme_minimal()
  
  ets_plots[[ctry]] <- p
  ggsave(paste0("Outputs/Forecasts/GDP_Forecast_ETS_", ctry, ".png"), plot = p, width = 10, height = 6, dpi = 300)
}

all_ets_forecasts <- do.call(rbind, ets_results)
write_xlsx(all_ets_forecasts, "Outputs/Forecasts/GDP_ETS_Forecast.xlsx")

# =============================
# 7. Evaluation – Forecast Comparison
# =============================
for(ctry in countries){
  df <- gdp_features %>% filter(Country == ctry) %>% arrange(Year) %>% as_tibble()
  arima_fc <- all_forecasts %>% filter(Country == ctry) %>% as_tibble()
  ets_fc <- all_ets_forecasts %>% filter(Country == ctry) %>% as_tibble()
  
  plot_df <- bind_rows(
    df %>% select(Year, GDP_per_capita) %>% mutate(Type = "Actual"),
    arima_fc %>% rename(GDP_per_capita = Forecast_GDP) %>% mutate(Type = "ARIMA Forecast"),
    ets_fc %>% rename(GDP_per_capita = Forecast_GDP_ETS) %>% mutate(Type = "ETS Forecast")
  )
  
  p <- ggplot(plot_df, aes(x = Year, y = GDP_per_capita, color = Type)) +
    geom_line(size = 1) +
    geom_point(data = subset(plot_df, Type != "Actual"), size = 2, alpha = 0.7) +
    labs(title = paste("GDP Forecast Comparison –", ctry),
         x = "Year", y = "GDP per Capita", color = "Legend") +
    theme_minimal() +
    scale_color_manual(values = c("Actual" = "black", "ARIMA Forecast" = "blue", "ETS Forecast" = "red"))
  
  print(p)
  ggsave(paste0("Outputs/Forecasts/GDP_Forecast_Comparison_", ctry, ".png"), plot = p, width = 10, height = 6, dpi = 300)
}

# =============================
# 8. Conclusion & Insights (Static Markdown)
# =============================
# Refer to notebook Markdown for final insights and recommendations