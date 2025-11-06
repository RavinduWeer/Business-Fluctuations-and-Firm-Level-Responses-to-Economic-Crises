# Business-Fluctuations-and-Firm-Level-Responses-to-Economic-Crises
This project analyzes cross-country business cycles , conducts an event study on the GFC &amp; another recession and examines firm-level responses to the GFC using WRDS Compustat.

This repository contains the R scripts and analysis for the "Project 2: Business Fluctuations and Firm-Level Responses" assignment. The project examines business cycle fluctuations, economic crises, and firm-level responses using macroeconomic and microeconomic data.
The project is divided into three main parts:
1.	Part 1: Cross-Country Business Cycle Analysis
2.	Part 2: Event Study – The Global Financial Crisis and COVID-19 Recession
3.	Part 3: Firm-Level Responses Using WRDS Compustat

1. Data Sources 
The data for this project is drawn from two primary sources:
•	Part 1 & 2 (Macroeconomic Data):
o	Federal Reserve Economic Data (FRED): Used to collect quarterly and monthly data for GDP, unemployment, CPI, interest rates, and stock indices.
•	Part 3 (Microeconomic Data):
o	Wharton Research Data Services (WRDS): Specifically, the Compustat database is used to source quarterly firm-level financial data (e.g., revenue, investment, leverage, profitability).
o	
2. Steps for Data Cleaning and Analysis
Part 1: Cross-Country Business Cycle Analysis
1.	Data Collection: Quarterly data for the US, France, and Australia (GDP, Unemployment, CPI, Long-Term Bonds) is downloaded from FRED using the fredr package.
2.	Cleaning & Aggregation: Series with monthly frequency are aggregated to quarterly by taking the mean. Missing values in source data (e.g., Australia's CPI) are linearly interpolated before this aggregation.
3.	Detrending: All time series are detrended using the Hodrick-Prescott (HP) filter ($\lambda=1600$) to isolate the cyclical component.
4.	Analysis:
o	Volatility: Standard deviations of the detrended cycles are computed and plotted in a grouped bar chart.
o	Correlation: Correlations of each variable's cycle with the GDP cycle are computed to identify procyclical, countercyclical, or acyclical relationships.
o	Dynamics: Overlaid time-series plots are generated to visually compare the timing and magnitude of cycles across countries.
Part 2: Event Study – GFC and COVID-19 (USA)
1.	Data Collection: Quarterly US data for Real GDP, Unemployment Rate, S&P 500, and CPI is downloaded from FRED.
2.	Recession Identification: Dates for the Global Financial Crisis (2007-2009) and the COVID-19 recession (2020) are defined to shade plots.
3.	Transformation: Key variables are converted to growth rates (e.g., GDP_growth) or quarter-on-quarter changes (e.g., Unemp_change).
4.	Analysis:
o	Tracking: Variables (in levels and growth rates) are plotted over time with shaded recession bars to track their paths.
o	Comparison: Normalized plots (where T=100 at the recession's start) are created to compare the depth and speed of recovery for GDP and Unemployment across the two crises.
o	Propagation: A correlation heatmap and a Vector Autoregression (VAR) model are used to analyze the relationships and propagation mechanisms between the variables (e.g., an Impulse Response Function from an S&P 500 shock to GDP growth).
Part 3: Firm-Level Responses (GFC)
1.	Data Collection: Quarterly firm-level data (e.g., revenue growth, capital expenditure, leverage, ROA) is sourced from the WRDS Compustat database.
2.	Cleaning: Data is cleaned to remove outliers and construct a consistent firm-level panel.
3.	Analysis: Firm performance is compared before, during, and after the GFC. The analysis aggregates these responses to identify patterns and discusses potential sectoral differences (e.g., manufacturing vs. services).

3. Instructions for Reproducing Results 
1.	Install R Packages:
The scripts rely on several R packages. You can install all required packages by running the following command in your R console:
R
install.packages(c("fredr", "mFilter", "dplyr", "tidyr", "ggplot2", "reshape2", "zoo", "quantmod", "urca", "patchwork", "ggcorrplot", "vars"))
2.	Set FRED API Key:
Parts 1 and 2 require a FRED API key. You can obtain one from the FRED website. In the R scripts (Part1.R, Part2.R), set your key at the top:
R
fredr_set_key("YOUR_API_KEY_GOES_HERE")
3.	WRDS Access (Part 3):
Part 3 requires access to the WRDS Compustat database which is a private subscription service20. You must have valid institutional credentials (e.g., a WRDS account or Day Pass) to run the Part3.R script21.
4.	Run the Scripts:
Execute the R scripts in order (Part1.R, Part2.R, Part3.R). The scripts will download the necessary data (except for WRDS), perform the analysis, and save the resulting figures.
