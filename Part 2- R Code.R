rm(list=ls())
cat("\f")

# -------------------------------------------------------------------------
# 1. SETUP: Load Libraries
# -------------------------------------------------------------------------

# Load required libraries
install.packages("quantmod")
install.packages("urca")
install.packages("patchwork")
install.packages("ggcorrplot")



library(quantmod)    # For financial data (though fredr is used here)
library(tidyverse)   # Data manipulation and plotting
library(zoo)         # For time series manipulation (as.yearqtr)
library(tseries)     # For time series tests
library(urca)        # For unit root and cointegration tests
library(fredr)       # To access FRED data
library(vars)        # For VAR modeling
library(patchwork)   # For combining multiple ggplot plots
library(ggcorrplot)  # For correlation heatmaps

# -------------------------------------------------------------------------
# 2. CONFIGURATION: API Key and Recession Dates
# -------------------------------------------------------------------------

# Set your FRED API key
fredr_set_key("91c9cf8510bdaf7b742ba41f4ed5d589") # Uncomment and run this line

# Define recession periods for the USA
gfc_start <- as.Date("2007-12-01")
gfc_end <- as.Date("2009-06-30")

covid_start <- as.Date("2020-02-01")
covid_end <- as.Date("2020-04-30")

# Create a recession dates dataframe for plotting
recession_rects <- data.frame(
  Recession = c("GFC", "COVID-19"),
  Start = as.yearqtr(c(gfc_start, covid_start)),
  End = as.yearqtr(c(gfc_end, covid_end))
)

# -------------------------------------------------------------------------
# 3. DATA RETRIEVAL AND PREPARATION
# -------------------------------------------------------------------------

# Define function to get quarterly data from FRED
get_fred_data <- function(series_id, start_date, end_date) {
  fredr(
    series_id = series_id,
    observation_start = start_date,
    observation_end = end_date,
    frequency = "q" # Request quarterly data directly
  ) %>%
    select(date, value) %>%
    rename(Date = date, Value = value)
}

# Select key macro variables for USA
start_date <- as.Date("2000-01-01")
end_date <- as.Date("2023-12-31")

# Define function to get quarterly data from FRED (CORRECTED)
get_fred_data <- function(series_id, start_date, end_date) {
  fredr(
    series_id = series_id,
    observation_start = start_date,
    observation_end = end_date,
    frequency = "q"
  ) %>%
    # EXPLICITLY call dplyr::select to avoid package conflict
    dplyr::select(date, value) %>% 
    rename(Date = date, Value = value)
}

# ---
# Now you can run your original code without error:
start_date <- as.Date("2000-01-01")
end_date <- as.Date("2023-12-31")

gdp <- get_fred_data("GDP", start_date, end_date)
unrate <- get_fred_data("UNRATE", start_date, end_date)
sp500 <- get_fred_data("SP500", start_date, end_date)
cpi <- get_fred_data("CPIAUCSL", start_date, end_date)

# This will now work
print(head(gdp))

# Merge data into one dataframe by Date
macro_data <- reduce(list(gdp, unrate, sp500, cpi), full_join, by = "Date")

# Rename columns and convert Date to yearqtr
macro_data_qtr <- macro_data %>%
  # FIX: The rename call was trying to find columns that didn't exist.
  # We must rename the positional columns created by the join.
  # The old names (names(.)[2], etc.) are the auto-generated ones
  # like "Value.x", "Value.y", etc.
  rename(
    GDP = !!names(.)[2],
    UnemploymentRate = !!names(.)[3],
    SP500 = !!names(.)[4],
    CPI = !!names(.)[5]
  ) %>%
  mutate(Quarter = as.yearqtr(Date)) %>%
  # FIX: Explicitly use dplyr::select to avoid function masking
  dplyr::select(Quarter, GDP, UnemploymentRate, SP500, CPI) %>%
  filter(!is.na(GDP) & !is.na(UnemploymentRate) & !is.na(SP500) & !is.na(CPI)) # Ensure complete cases
macro_data_qtr <- macro_data_qtr %>%
  arrange(Quarter) %>%
  mutate(
    # Quarter-on-Quarter (QoQ) growth rates
    GDP_growth = 100 * (GDP / lag(GDP) - 1),
    Unemp_change = UnemploymentRate - lag(UnemploymentRate), # Change in pp
    SP500_return = 100 * (SP500 / lag(SP500) - 1),
    CPI_inflation = 100 * (CPI / lag(CPI) - 1)
  )

# Add a 'Phase' variable for Boxplots and Comparative Plots
macro_data_qtr <- macro_data_qtr %>%
  mutate(
    Phase = case_when(
      Quarter >= as.yearqtr(gfc_start) & Quarter <= as.yearqtr(gfc_end) ~ "GFC",
      Quarter >= as.yearqtr(covid_start) & Quarter <= as.yearqtr(covid_end) ~ "COVID-19",
      TRUE ~ "Non-Recession"
    ),
    # Factor for ordered plotting
    Phase = factor(Phase, levels = c("Non-Recession", "GFC", "COVID-19"))
  )


# -------------------------------------------------------------------------
# 4. GRAPHICAL REPRESENTATIONS
# -------------------------------------------------------------------------

### Items 1 & 2: Line Charts with Recession Highlighting

# We can create a function to plot variables with shaded recession bars
plot_macro_shaded <- function(df, varname, title) {
  # Convert varname string to symbol for aes()
  var_sym <- sym(varname)
  
  ggplot(df, aes(x = Quarter, y = !!var_sym)) +
    geom_line(color = "blue", linewidth = 1) +
    # Add shaded recession bars
    geom_rect(
      data = recession_rects,
      aes(xmin = Start, xmax = End, ymin = -Inf, ymax = Inf),
      inherit.aes = FALSE,
      fill = "red",
      alpha = 0.2
    ) +
    labs(
      title = title,
      subtitle = "US Macroeconomic Variable (Recessions shaded in red)",
      y = varname,
      x = "Quarter"
    ) +
    theme_minimal()
}

# Plot all four level variables
p1 <- plot_macro_shaded(macro_data_qtr, "GDP", "US Real GDP (Levels)")
p2 <- plot_macro_shaded(macro_data_qtr, "UnemploymentRate", "US Unemployment Rate (Levels)")
p3 <- plot_macro_shaded(macro_data_qtr, "SP500", "S&P 500 Index (Levels)")
p4 <- plot_macro_shaded(macro_data_qtr, "CPI", "US CPI (Levels)")

# Display plots (using patchwork)
(p1 | p2) / (p3 | p4)


### Item 3: Growth Rate Plots

# We can visualize the growth rates (more volatile) over time
# Pivot data to long format for easy faceting
growth_data_long <- macro_data_qtr %>%
  dplyr::select(Quarter, GDP_growth, Unemp_change, SP500_return, CPI_inflation) %>% # <-- Fix: Explicitly use dplyr::select
  pivot_longer(
    cols = -Quarter,
    names_to = "Variable",
    values_to = "Value"
  )

ggplot(growth_data_long, aes(x = Quarter, y = Value)) +
  geom_line() +
  # Add shaded recession bars
  geom_rect(
    data = recession_rects,
    aes(xmin = Start, xmax = End, ymin = -Inf, ymax = Inf),
    inherit.aes = FALSE,
    fill = "red",
    alpha = 0.2
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  # Create a separate panel for each variable
  facet_wrap(~ Variable, scales = "free_y", ncol = 1) +
  labs(
    title = "Growth Rates and Changes in Key Macro Variables",
    subtitle = "Recessions shaded in red",
    x = "Quarter",
    y = "QoQ % Change (or p.p. change for Unemp)"
  ) +
  theme_bw()


### Item 6: Comparative Plots (GFC vs. COVID-19)

# We can compare the *path* of variables relative to the start of each recession
# Function to get data relative to a recession start
get_relative_path <- function(df, start_qtr, end_qtr, varname, recession_name) {
  var_sym <- sym(varname)
  
  # Ensure start_qtr and end_qtr are 'yearqtr' objects
  start_qtr <- as.yearqtr(start_qtr)
  end_qtr <- as.yearqtr(end_qtr)
  
  # Get data from 4 quarters before to 8 quarters after the *end*
  df_filtered <- df %>%
    filter(Quarter >= (start_qtr - 1) & Quarter <= (end_qtr + 2)) %>%
    mutate(
      # Create a "relative quarter" index (T=0 is the start quarter)
      Relative_Quarter = as.numeric(Quarter - start_qtr) * 4
    )
  
  # Normalize to 100 at the start of the recession (T=0)
  start_value <- (df_filtered %>% filter(Relative_Quarter == 0) %>% pull(!!var_sym))
  
  # Handle potential case where start_value might be missing (though unlikely here)
  if(length(start_value) == 0 || is.na(start_value)) {
    return(NULL) # Return NULL if normalization base is missing
  }
  
  df_filtered %>%
    mutate(
      Normalized_Value = 100 * (!!var_sym / start_value),
      Recession = recession_name
    ) %>%
    dplyr::select(Recession, Relative_Quarter, Normalized_Value)
}

# Get paths for GDP
gfc_gdp_path <- get_relative_path(macro_data_qtr, gfc_start, gfc_end, "GDP", "GFC")
covid_gdp_path <- get_relative_path(macro_data_qtr, covid_start, covid_end, "GDP", "COVID-19")

# Get paths for Unemployment
gfc_unemp_path <- get_relative_path(macro_data_qtr, gfc_start, gfc_end, "UnemploymentRate", "GFC")
covid_unemp_path <- get_relative_path(macro_data_qtr, covid_start, covid_end, "UnemploymentRate", "COVID-19")

# Combine and plot
gdp_paths <- bind_rows(gfc_gdp_path, covid_gdp_path)
unemp_paths <- bind_rows(gfc_unemp_path, covid_unemp_path)

# Plot GDP Comparison
p_comp_gdp <- ggplot(gdp_paths, aes(x = Relative_Quarter, y = Normalized_Value, color = Recession)) +
  geom_line(linewidth = 1.2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(
    title = "Comparative GDP Recovery: GFC vs. COVID-19",
    subtitle = "Value = 100 at Recession Start (T=0)",
    x = "Quarters Relative to Recession Start",
    y = "Real GDP (Normalized to 100)"
  ) +
  theme_minimal()

# Plot Unemployment Comparison
p_comp_unemp <- ggplot(unemp_paths, aes(x = Relative_Quarter, y = Normalized_Value, color = Recession)) +
  geom_line(linewidth = 1.2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(
    title = "Comparative Unemployment Path: GFC vs. COVID-19",
    subtitle = "Value = 100 at Recession Start (T=0)",
    x = "Quarters Relative to Recession Start",
    y = "Unemployment Rate (Normalized to 100)"
  ) +
  theme_minimal()

# Show comparison plots using patchwork
(p_comp_gdp / p_comp_unemp)


### Items 5 & 7: Scatterplots and Correlation Matrix (Heatmap)

# 7a. Scatterplot: GDP Growth vs. Unemployment Change (an "Okun's Law" type plot)
# We can color the points by the 'Phase'
ggplot(macro_data_qtr, aes(x = Unemp_change, y = GDP_growth, color = Phase)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", aes(group = 1), color = "black", se = FALSE) + # Add overall regression line
  geom_vline(xintercept = 0, linetype = "dotted") +
  geom_hline(yintercept = 0, linetype = "dotted") +
  labs(
    title = "GDP Growth vs. Change in Unemployment Rate",
    subtitle = "Quarterly Data, 2000-2023",
    x = "Quarterly Change in Unemployment Rate (p.p.)",
    y = "Quarterly GDP Growth (%)"
  ) +
  theme_minimal()


# 5 & 7b. Correlation Matrix Heatmap
# Select only the growth/change variables
corr_data <- macro_data_qtr %>%
  dplyr::select(GDP_growth, Unemp_change, SP500_return, CPI_inflation) %>%
  na.omit()

# Compute correlation matrix
corr_matrix <- cor(corr_data)

# Plot the heatmap
ggcorrplot(
  corr_matrix,
  method = "circle", # Use circles, size indicates magnitude
  type = "lower",     # Show lower triangular matrix
  lab = TRUE,         # Add correlation coefficients as labels
  lab_size = 3,
  colors = c("red", "white", "blue"), # Color scheme
  title = "Correlation Heatmap of Macro Variable Growth Rates"
)


### Item 8: Boxplots by Economic Phase

# We can use the 'Phase' variable to compare distributions
# Use the 'growth_data_long' dataframe created earlier, and join the Phase
boxplot_data <- growth_data_long %>%
  # Explicitly use dplyr::select to avoid namespace conflict
  left_join(dplyr::select(macro_data_qtr, Quarter, Phase), by = "Quarter") %>%
  na.omit()

ggplot(boxplot_data, aes(x = Phase, y = Value, fill = Phase)) +
  geom_boxplot() +
  # Create a separate panel for each variable
  facet_wrap(~ Variable, scales = "free_y", ncol = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  labs(
    title = "Distribution of Macro Variables by Economic Phase",
    subtitle = "Comparing Non-Recession, GFC, and COVID-19 Periods",
    x = "Economic Phase",
    y = "Quarterly Value"
  ) +
  theme_bw() +
  theme(legend.position = "none") # Hide legend as x-axis is clear


# -------------------------------------------------------------------------
# 5. VAR MODELING (From Original Code)
# -------------------------------------------------------------------------

### Item 4: Impulse Response Function Graphs

# Prepare data for VAR - select growth rates and changes
var_data <- macro_data_qtr %>%
  dplyr::select(Quarter, GDP_growth, Unemp_change, SP500_return, CPI_inflation) %>%
  na.omit()

var_ts <- ts(var_data[, -1], start = c(year(min(var_data$Quarter)), quarter(min(var_data$Quarter))), frequency = 4)

# Estimate VAR with lag selection
# Note: VAR estimation is sensitive to sample size and stationarity.
# This is a simplified example.
var_model <- VAR(var_ts, ic = "AIC", lag.max = 4)

summary(var_model)

# Impulse response functions can be plotted to analyze propagation
# How does a 1-std-dev shock to S&P 500 returns propagate to GDP growth?
irf_gdp <- irf(var_model, impulse = "SP500_return", response = "GDP_growth", boot = TRUE, n.ahead = 12)

# Plot the IRF
# The plot() function for 'irf' objects is from the 'vars' package (base graphics)
plot(irf_gdp, main = "Impulse Response: Shock from S&P 500 to GDP Growth", 
     ylab = "Response of GDP_growth", xlab = "Quarters")
