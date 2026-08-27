### Repository Overview
This repository contains the core analytical pipeline extracted from the thesis "Financial Constraints in the Transmission of Monetary Policy: An Industry-Level Investigation". It focuses strictly on the primary empirical framework, excluding secondary figures and robustness checks. 

The analysis investigates how sector-level financial constraints—specifically corporate leverage, asset tangibility, and maturing debt share—alter the macroeconomic transmission of monetary policy shocks. It utilizes a panel Local Projections framework on quarterly data from 2002 to 2020 for 59 U.S. industries. By interacting high-frequency monetary policy surprises with corporate balance-sheet aggregates, the framework evaluates the responses of real output and prices across varying degrees of financial friction.

### Dependencies
To execute this pipeline, you need the following data sources and packages.

**Data Requirements:**
*   Internal Revenue Service Statistics of Income (IRS SOI)
*   Bureau of Economic Analysis (BEA) Real Value Added and Deflator
*   Census Bureau Business Dynamics Statistics (BDS)
*   FRED Macroeconomic Aggregates (1-year Treasury Yield, EBP, Commodity Price Index, GDP, PCE)
*   Monetary policy shock series (Bu et al., 2021)
*   Frequency of Price Adjustment (FPA) data (Pasten et al., 2020)

**R Packages:**
*   `tidyverse`
*   `readxl`
*   `lpirfs`
*   `patchwork`
*   `caret`
*   `plm`
*   `lmtest`
*   `mpshock`

### Pipeline Execution
The scripts must be executed in sequential order. 

**01_process_irs.R**
Parses raw IRS SOI files, isolates the target balance-sheet line items using regex, and aggregates the financial data to match the BEA sector classification. 

**02_build_master_panel.R**
Merges the processed IRS data with BEA macroeconomic outcomes, BDS demographic data, and monetary policy shocks to construct the final sector-time panel. It also handles the k-NN imputation for missing price stickiness (FPA) data.

**03_prep_interactions.R**
Applies winsorization to handle outliers, standardizes the variables, and generates the lagged interaction terms required for the local projection regressions.

**04_irf_baseline.R**
Estimates the unconditional macroeconomic transmission of the monetary shock on output and prices across the average industry, providing the baseline impulse response functions.

**05_irf_simple_controls.R**
Estimates local projections interacting the shock with the main financial constraints independently. It caches these simple models and renders the corresponding impulse response grids.

**06_irf_full_controls.R**
Computes the fully controlled local projection models, accounting for competing financial frictions, structural traits, and nominal rigidities. It outputs the final grid, overlaying the simple and full models for direct comparison.

**utils_plotting.R**
Contains the custom functions required to plot the impulse response functions with exact confidence bands. This file is sourced automatically by the analysis scripts.