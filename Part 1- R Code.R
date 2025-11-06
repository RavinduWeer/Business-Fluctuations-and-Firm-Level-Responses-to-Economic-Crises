rm(list=ls())
cat("\f")

# Install and load required packages
# install.packages(c("hpfilter", "zoo", "fredr", "mFilter", "dplyr", "tidyr", "ggplot2", "reshape2"))

library(fredr)
library(mFilter)
library(dplyr)
library(tidyr)
library(ggplot2)
library(reshape2)
library(zoo)

# Set FRED API key
fredr_set_key("91c9cf8510bdaf7b742ba41f4ed5d589")

# Define start and end dates
start_date <- as.Date("2005-01-01")
end_date <- as.Date("2024-12-31")

# Define FRED series IDs for countries (Australia replaces UK)
series_ids <- list(
  USA = list(
    GDP = "GDPC1",                    # Real GDP Quarterly
    Unemployment = "UNRATE",          # Unemployment Rate Monthly
    LongTermBond = "GS10",            # 10-Year Treasury Constant Maturity Rate Monthly
    CPI = "CPIAUCSL"                  # Consumer Price Index Monthly
  ),
  France = list(
    GDP = "CLVMNACSCAB1GQFR",                # France Real GDP Quarterly
    Unemployment = "LRHUTTTTFRM156S",        # France Unemployment Rate Quarterly
    LongTermBond = "IRLTLT01FRM156N",        # France 10-Year Govt Bond Yield Monthly
    CPI = "FRACPIALLMINMEI"                  # France CPI Monthly
  ),
  Australia = list(
    GDP = "NAEXKP01AUQ657S",                 # Australia Real GDP Quarterly
    Unemployment = "LRHUTTTTAUM156S",        # Australia Unemployment Rate Monthly
    LongTermBond = "IRLTLT01AUM156N",        # Australia 10-Year Govt Bond Yield Monthly
    CPI = "AUSCPIALLQINMEI"                  # Australia CPI Quarterly
  )
)

# Function to extract data and clean missing/zero values
get_fred_data <- function(series_id) {
  df <- fredr(
    series_id = series_id,
    observation_start = start_date,
    observation_end = end_date
  ) %>%
    select(date, value) %>%
    filter(!is.na(value) & value != 0)
  return(df)
}

# Download all data
data_list <- list()
for (country in names(series_ids)) {
  for (varname in names(series_ids[[country]])) {
    df <- get_fred_data(series_ids[[country]][[varname]])
    df <- df %>% mutate(Country = country, Variable = varname)
    
    # Convert Australia's CPI from quarterly to monthly for consistent aggregation
    if (country == "Australia" && varname == "CPI") {
      df <- df %>%
        complete(date = seq.Date(min(date), max(date), by = "month")) %>%
        arrange(date) %>%
        mutate(value = na.approx(value, rule = 2))  # Linear interpolation
    }
    
    data_list[[paste(country, varname, sep = "_")]] <- df
  }
}

# Combine all data
all_data <- bind_rows(data_list)

# Convert to quarterly frequency
all_data_quarterly <- all_data %>%
  mutate(Quarter = as.yearqtr(date)) %>%
  group_by(Country, Variable, Quarter) %>%
  summarize(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
  arrange(Country, Variable, Quarter) %>%
  filter(!is.na(value) & value != 0)

# Reshape wide format
data_wide <- all_data_quarterly %>%
  unite("CountryVar", Country, Variable) %>%
  pivot_wider(names_from = CountryVar, values_from = value)

# Apply HP filter to detrend
hp_detrended <- data_wide
for (col in names(hp_detrended)[-1]) { # [-1] skips the 'Quarter' column
  
  # Ensure there's enough data to filter
  if(sum(!is.na(hp_detrended[[col]])) > 8) { # Check for sufficient data points
    start_q <- hp_detrended$Quarter[min(which(!is.na(hp_detrended[[col]])))]
    ts_data <- ts(hp_detrended[[col]], 
                  frequency = 4, 
                  start = c(as.numeric(format(start_q, "%Y")), 
                            as.numeric(format(start_q, "%q"))))
    
    # Use try() to catch errors during filtering
    hp_result <- try(hpfilter(ts_data, freq = 1600, type = "lambda"), silent = TRUE)
    
    if (inherits(hp_result, "try-error")) {
      hp_detrended[[col]] <- NA # Set to NA if filtering fails
    } else {
      # Need to align the cycle with the original data, as hpfilter might drop NAs
      cycle_data <- as.numeric(hp_result$cycle)
      original_data <- hp_detrended[[col]]
      
      # Re-insert NAs to match original data length
      full_cycle <- rep(NA, length(original_data))
      non_na_indices <- which(!is.na(original_data))
      
      # Check if lengths match
      if(length(cycle_data) == length(non_na_indices)){
        full_cycle[non_na_indices] <- cycle_data
        hp_detrended[[col]] <- full_cycle
      } else {
        hp_detrended[[col]] <- NA # Mismatch, set to NA
      }
    }
  } else {
    hp_detrended[[col]] <- NA # Not enough data
  }
}


# Compute volatility (standard deviation)
std_dev <- apply(hp_detrended[,-1], 2, sd, na.rm = TRUE)
std_dev_df <- data.frame(
  CountryVar = names(std_dev),
  StdDev = as.numeric(std_dev)
) %>%
  separate(CountryVar, into = c("Country", "Variable"), sep = "_") %>%
  filter(!is.na(StdDev))

# Compute GDP correlations
correlations <- data.frame()
for (var in unique(std_dev_df$Variable)) {
  for (country in unique(std_dev_df$Country)) {
    var_col <- paste(country, var, sep = "_")
    gdp_col <- paste(country, "GDP", sep = "_")
    if (var_col %in% names(hp_detrended) && gdp_col %in% names(hp_detrended)) {
      complete_idx <- complete.cases(hp_detrended[[var_col]], hp_detrended[[gdp_col]])
      if (sum(complete_idx) > 1) {
        corr <- cor(hp_detrended[[var_col]][complete_idx], hp_detrended[[gdp_col]][complete_idx])
        correlations <- rbind(correlations,
                              data.frame(Country = country,
                                         Variable = var,
                                         Correlation = corr))
      }
    }
  }
}

# --- PLOTTING ---

# Volatility plot (Task 1 - Unchanged)
volatility_plot <- ggplot(std_dev_df %>% filter(Variable != "GDP"),
                          aes(x = Variable, y = StdDev, fill = Country)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Volatility (Standard Deviation) of Detrended Variables",
       y = "Standard Deviation") +
  theme_minimal()

# Correlation plot (Task 2 - Unchanged)
corr_plot <- ggplot(correlations %>% filter(Variable != "GDP"),
                    aes(x = Variable, y = Correlation, fill = Country)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Correlation with GDP (Output)",
       y = "Correlation Coefficient") +
  theme_minimal()

# --- MODIFIED TIME SERIES PLOT SECTION ---

# Plot all detrended time series (Task 3)
# This replaces the old 'ts_plot_gdp'
# 1. Pivot the entire detrended dataset (all variables) to a long format
hp_long <- hp_detrended %>%
  # The first column is 'Quarter', pivot all other columns
  pivot_longer(
    cols = -Quarter, 
    names_to = "CountryVar",
    values_to = "Value"
  ) %>%
  # Separate the "Country_Variable" column back into "Country" and "Variable"
  separate(CountryVar, into = c("Country", "Variable"), sep = "_") %>%
  filter(!is.na(Value)) # Remove any missing values

# 2. Create the faceted plot
# This single plot object will generate separate charts for each variable
ts_plot_all_variables <- ggplot(hp_long, aes(x = Quarter, y = Value, color = Country)) +
  geom_line() +
  # Use facet_wrap to create a separate plot panel for each 'Variable'
  # 'scales = "free_y"' is crucial so each variable has its own y-axis scale
  facet_wrap(~ Variable, scales = "free_y") + 
  labs(
    title = "Detrended Time Series Comparison by Variable",
    x = "Quarter",
    y = "HP-filtered Cycle"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom") # Move legend for better layout

# --- END MODIFIED SECTION ---


# Print plots
print(volatility_plot)
print(corr_plot)
print(ts_plot_all_variables) # Print the new, comprehensive time series plot
