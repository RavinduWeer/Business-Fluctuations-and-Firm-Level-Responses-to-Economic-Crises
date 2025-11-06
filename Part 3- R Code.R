rm(list=ls())
cat("\f")


# --------------------------------------------------------------------------
# Project 2: Part 3 - WRDS Compustat Data Analysis
# --------------------------------------------------------------------------

# 1. INSTALL AND LOAD LIBRARIES
# --------------------------------------------------------------------------
# List of required packages
packages <- c("RPostgres", "DBI", "dplyr", "getPass", "lubridate", 
              "ggplot2",   # For plotting
              "broom",     # For tidying regression models
              "patchwork") # For combining plots

# Install packages if they are not already installed
new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

# Load libraries
library(RPostgres)
library(DBI)
library(dplyr)
library(getPass)
library(lubridate)
library(ggplot2)
library(broom)
library(patchwork)

# --------------------------------------------------------------------------
# 2. CONNECT TO WRDS
# --------------------------------------------------------------------------
# Prompt for username and password.
wrds_username <- "weerasrw"
wrds_password <- getPass("Enter your WRDS password:")

# Establish the connection
wrds <- dbConnect(RPostgres::Postgres(),
                  host = 'wrds-pgdata.wharton.upenn.edu',
                  port = 9737,
                  user = wrds_username,
                  password = wrds_password,
                  sslmode = 'require',
                  dbname = 'wrds')

print("Connection to WRDS successful!")

# --------------------------------------------------------------------------
# 3. CONSTRUCT AND SEND SQL QUERY (CORRECTED)
# --------------------------------------------------------------------------
# We join comp.fundq (financials) with comp.company (firm info like SIC)
sql_query <- "
    SELECT 
        a.gvkey,      -- Firm identifier
        a.datadate,   -- The date of the observation
        b.sic,        -- Standard Industrial Classification (from comp.company)
        a.atq,        -- Total Assets (Quarterly)
        a.ltq,        -- Total Liabilities (Quarterly)
        a.saleq,      -- Sales (Quarterly)
        a.capxy       -- Capital Expenditures (Quarterly, YTD)
    FROM 
        comp.fundq AS a
    LEFT JOIN 
        comp.company AS b ON a.gvkey = b.gvkey
    WHERE 
        a.indfmt = 'INDL'     -- Industrial format
        AND a.datafmt = 'STD' -- Standardized data
        AND a.popsrc = 'D'    -- Domestic firms
        AND a.consol = 'C'    -- Consolidated statements
        AND a.datadate >= '2005-01-01'
        AND a.datadate <= '2012-12-31'
        AND a.atq IS NOT NULL AND a.atq > 0
"

# Send the query and fetch the data
print("Fetching data from Compustat...")
compustat_data <- dbGetQuery(wrds, sql_query)
print("Data download complete.")

# Close the database connection
dbDisconnect(wrds)

# --------------------------------------------------------------------------
# 4. CLEAN DATA AND CONSTRUCT VARIABLES
# --------------------------------------------------------------------------
print("Cleaning data and constructing variables...")

firm_panel <- compustat_data %>%
  arrange(gvkey, datadate) %>%
  group_by(gvkey) %>%
  
  # De-annualize YTD capital expenditure (capxy)
  mutate(capx_q = ifelse(quarter(datadate) == 1, capxy, capxy - lag(capxy, 1, default = 0))) %>%
  
  mutate(
    # 1. Leverage Ratio: Total Liabilities / Total Assets
    leverage = ltq / atq,
    
    # 2. Investment Spending (as % of lagged assets)
    investment = capx_q / lag(atq, 1),
    
    # 3. Revenue Growth (Year-over-Year)
    revenue_growth = (saleq - lag(saleq, 4)) / lag(saleq, 4)
  ) %>%
  
  # Add a 2-digit SIC code for sectoral analysis
  mutate(sic_sector = floor(as.numeric(sic) / 100)) %>%
  
  # Clean up and keep only necessary columns
  select(gvkey, datadate, sic_sector, leverage, investment, revenue_growth) %>%
  filter(!is.na(leverage) & !is.na(investment) & !is.na(revenue_growth)) %>%
  filter(is.finite(leverage) & is.finite(investment) & is.finite(revenue_growth)) %>%
  ungroup()

print("Firm-level panel constructed.")

# --------------------------------------------------------------------------
# 5. CREATE AGGREGATE SUMMARY FOR PLOTTING
# --------------------------------------------------------------------------
# This is for Visualization 1.
quarterly_summary <- firm_panel %>%
  group_by(datadate) %>%
  summarise(
    median_leverage = median(leverage, na.rm = TRUE),
    median_investment = median(investment, na.rm = TRUE),
    median_revenue_growth = median(revenue_growth, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  arrange(datadate)

print("Quarterly summary for plotting is ready.")

# --------------------------------------------------------------------------
# 6. VISUALIZATION 1: Compare Performance Before, During, and After GFC
# --------------------------------------------------------------------------
print("--- Generating Visualization 1: Time Series Plots ---")

# Define GFC period (as per your prompt: 2007Q4 to 2009Q2)
gfc_start <- as.Date("2007-10-01")
gfc_end <- as.Date("2009-06-30")

# Plot 1: Revenue Growth
plot_rev_growth <- ggplot(quarterly_summary, aes(x = datadate, y = median_revenue_growth)) +
  geom_rect(aes(xmin = gfc_start, xmax = gfc_end, ymin = -Inf, ymax = Inf), 
            fill = "gray80", alpha = 0.5) +
  geom_line(color = "#0072B2", size = 1) +
  geom_vline(xintercept = gfc_start, linetype = "dashed", color = "red") +
  geom_vline(xintercept = gfc_end, linetype = "dashed", color = "red") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Median Firm Revenue Growth (YoY)",
       subtitle = "GFC Period (2007Q4-2009Q2) shaded in gray",
       x = "Date", y = "Median YoY Growth") +
  theme_minimal()

# Plot 2: Investment Spending
plot_investment <- ggplot(quarterly_summary, aes(x = datadate, y = median_investment)) +
  geom_rect(aes(xmin = gfc_start, xmax = gfc_end, ymin = -Inf, ymax = Inf), 
            fill = "gray80", alpha = 0.5) +
  geom_line(color = "#D55E00", size = 1) +
  geom_vline(xintercept = gfc_start, linetype = "dashed", color = "red") +
  geom_vline(xintercept = gfc_end, linetype = "dashed", color = "red") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Median Firm Investment Spending",
       subtitle = "GFC Period (2007Q4-2009Q2) shaded in gray",
       x = "Date", y = "Median (Investment / Assets)") +
  theme_minimal()

# Plot 3: Leverage
plot_leverage <- ggplot(quarterly_summary, aes(x = datadate, y = median_leverage)) +
  geom_rect(aes(xmin = gfc_start, xmax = gfc_end, ymin = -Inf, ymax = Inf), 
            fill = "gray80", alpha = 0.5) +
  geom_line(color = "#009E73", size = 1) +
  geom_vline(xintercept = gfc_start, linetype = "dashed", color = "red") +
  geom_vline(xintercept = gfc_end, linetype = "dashed", color = "red") +
  labs(title = "Median Firm Leverage Ratio",
       subtitle = "GFC Period (2007Q4-2009Q2) shaded in gray",
       x = "Date", y = "Median (Total Liabilities / Assets)") +
  theme_minimal()

# Combine the three plots into one stacked chart
plot1_combined <- (plot_rev_growth / plot_investment / plot_leverage)

# Display the combined plot
print(plot1_combined)


# --------------------------------------------------------------------------
# 7. VISUALIZATION 2: Identify Sectoral Differences
# --------------------------------------------------------------------------
print("--- Generating Visualization 2: Sectoral Bar Chart ---")

# Create period and sector labels
sector_data <- firm_panel %>%
  mutate(
    # 1. Define the three periods
    period = case_when(
      datadate < gfc_start ~ "Pre-GFC",
      datadate >= gfc_start & datadate <= gfc_end ~ "During-GFC",
      datadate > gfc_end ~ "Post-GFC"
    ),
    # 2. Define sectors (e.g., Manufacturing vs. Retail)
    sector_name = case_when(
      sic_sector >= 20 & sic_sector <= 39 ~ "Manufacturing",
      sic_sector >= 52 & sic_sector <= 59 ~ "Retail",
      TRUE ~ "Other" # Group all other sectors
    )
  ) %>%
  # Filter to only keep our defined sectors
  filter(sector_name != "Other")

# Calculate average investment by period and sector
sector_summary <- sector_data %>%
  group_by(period, sector_name) %>%
  summarise(
    avg_investment = mean(investment, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  # Order the periods logically
  mutate(period = factor(period, levels = c("Pre-GFC", "During-GFC", "Post-GFC")))

# Create the grouped bar chart
plot2_sector_bars <- ggplot(sector_summary, 
                            aes(x = sector_name, y = avg_investment, fill = period)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  labs(title = "Average Firm Investment by Sector and Period",
       subtitle = "Comparing Manufacturing vs. Retail",
       x = "Sector",
       y = "Average Investment / Assets",
       fill = "Crisis Period") +
  theme_minimal()

# Display the plot
print(plot2_sector_bars)


# --------------------------------------------------------------------------
# 8. VISUALIZATION 3: Explain Observed Patterns
# --------------------------------------------------------------------------
print("--- Generating Visualization 3: Regression Coefficient Plot ---")

# Step 1: Create the data (one row per firm)
# We want to explain the *change* in performance *during* the GFC
# using *pre-crisis* characteristics.

# Summarize Pre-GFC characteristics
firms_pre_gfc <- firm_panel %>%
  filter(datadate < gfc_start) %>%
  group_by(gvkey, sic_sector) %>%
  summarise(
    pre_gfc_leverage = mean(leverage, na.rm = TRUE),
    pre_gfc_investment = mean(investment, na.rm = TRUE),
    .groups = 'drop'
  )

# Summarize performance *during* the GFC
firms_during_gfc <- firm_panel %>%
  filter(datadate >= gfc_start & datadate <= gfc_end) %>%
  group_by(gvkey) %>%
  summarise(
    during_gfc_investment = mean(investment, na.rm = TRUE),
    .groups = 'drop'
  )

# Join them into one dataset
regression_data <- inner_join(firms_pre_gfc, firms_during_gfc, by = "gvkey") %>%
  # Create the dependent variable: Change in Investment
  mutate(
    investment_change = during_gfc_investment - pre_gfc_investment,
    # Create a dummy variable for manufacturing
    is_manufacturing = ifelse(sic_sector >= 20 & sic_sector <= 39, 1, 0)
  ) %>%
  filter(is.finite(investment_change) & is.finite(pre_gfc_leverage))

# Step 2: Run the regression
# DV: investment_change
# IVs: pre_gfc_leverage, is_manufacturing
model <- lm(investment_change ~ pre_gfc_leverage + is_manufacturing, data = regression_data)

# Step 3: Tidy the model output and plot coefficients
model_tidy <- tidy(model, conf.int = TRUE)

plot3_coeffs <- model_tidy %>%
  filter(term != "(Intercept)") %>% # Don't plot the intercept
  ggplot(aes(x = estimate, y = term)) +
  geom_point(color = "blue", size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.1) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Which Firm Characteristics Explain Investment Declines?",
       subtitle = "DV = Change in Investment (During-GFC minus Pre-GFC)",
       x = "Coefficient Estimate (with 95% C.I.)",
       y = "Firm Characteristic") +
  theme_minimal()

# Display the plot
print(plot3_coeffs)

print("--- All visualizations generated. ---")
