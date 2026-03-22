# GDPTimeSeriesAnalysis

## Project Overview
This project analyzes global GDP per capita from 1980 to 2023 and forecasts trends for 2024–2028. The goal is to understand historical economic trajectories and predict future GDP patterns for selected countries, focusing on ASEAN-5.

## Folder Structure
- `Data/Raw` : original dataset (IMF GDP data)  
- `Data/Processed` : cleaned long-format dataset and feature-engineered data  
- `Notebooks` : R Markdown notebook documenting the workflow  
- `Scripts` : reusable R scripts for each pipeline step  
- `Outputs/Plots` : EDA plots (line plots, boxplots, histograms, moving averages, facet plots)  
- `Outputs/Forecasts` : ARIMA and ETS forecast plots and tables  

## Pipeline
1. **Data Cleaning** – remove duplicates, handle missing values, filter out GDP = 0, pivot wide to long, convert to tsibble  
2. **Exploratory Data Analysis (EDA)** – line plots, boxplots, histograms, growth rate analysis; advanced plots including moving averages and log distributions  
3. **Feature Engineering** – create lag features, differences, growth rates, log transformations, rolling averages to enhance time series forecasting  
4. **Forecasting / Modeling** – ARIMA and ETS models per country  
5. **Evaluation** – forecast comparison plots, error metrics, insights  

## How to Run
1. Open RStudio project  
2. Run R scripts in `Scripts/utils.R` in order or knit the R Markdown notebook `Notebooks/GDP_TimeSeries.Rmd`  
3. Make sure required packages are installed: `tidyverse`, `readxl`, `tsibble`, `writexl`, `ggplot2`, `forecast`, `zoo`, `tseries`, `knitr`  

## Key Insights
- ASEAN-5 countries show steadily increasing GDP per capita from 1980–2023, with short-term fluctuations  
- Lag features, differencing, log transformations, and rolling averages improved model forecasting  
- ARIMA captures short-term year-to-year fluctuations, while ETS produces smoother long-term forecasts  
- Singapore consistently shows the highest GDP per capita; forecasts suggest this trend continues to 2028  
- Combining ARIMA and ETS provides a balanced perspective for both short-term reactive policy and long-term strategic planning  

## References
- IMF World Economic Outlook Database – Global GDP Data 1980–2028  
