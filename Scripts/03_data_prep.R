library(tidyverse)

# define variables for interactions and controls
interact_vars <- c("Debt_to_Equity", "Avg_Firm_Size", "FPA",
                   "Young_Firm_Share", "Asset_Tangibility", "Quick_ratio",
                   "St_ratio", "KY_ratio", "Gross_Margin",
                   "Leverage_Assets", "Leverage_Assets_2",
                   "Durable", "Nondurable") 

# define monetary policy shock variables
shocks <- c("MP_brw")


# define function to winsorize variables at specified quantiles to handle outliers
winsorize_var <- function(x, lower = 0.01, upper = 0.99) {
  q <- quantile(x, probs = c(lower, upper), na.rm = TRUE)
  x[x < q[1]] <- q[1]
  x[x > q[2]] <- q[2]
  return(x)
}

# define function to generate lagged interactions between current shocks and variables at t-4
create_interactions <- function(data, shocks, interact_var) {
  name_pattern <- paste0("{.col}_x_", interact_var)
  
  data %>%
    arrange(Sector, Time) %>%
    group_by(Sector) %>%
    mutate(!!interact_var := dplyr::lag(.data[[interact_var]], n = 4)) %>%
    ungroup() %>%
    mutate(across(all_of(shocks), 
                  ~ .x * .data[[interact_var]], 
                  .names = name_pattern))
}


# load dataset and apply initial transformations
df <- readRDS("Data/dataset_final.rds") %>%
  mutate(
    # apply log transformation and winsorize variables to bound extreme values
    Avg_Firm_Size  = log(Avg_Firm_Size),
    Avg_Firm_Size  = winsorize_var(Avg_Firm_Size),
    Debt_to_Equity = winsorize_var(Debt_to_Equity),
    KY_ratio       = winsorize_var(KY_ratio),
    Gross_Margin   = winsorize_var(Gross_Margin),
    
    # standardize specified variables for comparability
    Avg_Firm_Size = as.numeric(scale(Avg_Firm_Size)),
    KY_ratio      = as.numeric(scale(KY_ratio)),
    
    # standardize monetary policy shocks
    across(
      all_of(shocks), 
      ~ as.numeric(scale(.x))),
    
    # calculate log nominal output
    l_nominal = l_Va + l_piv, 
    
    # de-mean interaction variables to center them around zero
    across(
      all_of(interact_vars),
      ~ .x - mean(.x, na.rm = TRUE),
      .names = "{.col}")
  ) %>%
  as.data.frame()


# apply the interaction function iteratively across all specified variables
df <- interact_vars %>%
  reduce(~ create_interactions(.x, shocks, .y), .init = df)

# export the prepared dataset for modeling
saveRDS(df, "Data/df_ready.rds")