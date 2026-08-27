library(tidyverse)
library(readxl)

# load the mapping dictionary
sector_mapping <- read_xlsx("Data/irs_mapping_v2.xlsx")

# list raw IRS files
file_paths <- list.files("Data/IRS Soi", pattern = "^[^~].*\\.xlsx?$", full.names = TRUE)

# process files to build the master panel
master_panel <- map_dfr(file_paths, function(file) {
  
  year <- as.numeric(str_extract(basename(file), "\\d{4}"))
  year_col <- paste0(year)
  
  if (!year_col %in% colnames(sector_mapping)) {
    warning(paste("Year", year, "not found in mapping file. Skipping."))
    return(NULL)
  }
  
  raw_data <- tryCatch({
    read_excel(file, col_names = FALSE, col_types = "text")
  }, error = function(e) return(NULL))
  
  if (is.null(raw_data)) return(NULL)
  
  colnames(raw_data) <- as.character(seq_len(ncol(raw_data)))
  
  # locate the row containing column indices
  index_row_idx <- which(apply(raw_data, 1, function(r) "1" %in% r && "2" %in% r && "3" %in% r))[1]
  if (is.na(index_row_idx)) index_row_idx <- 6 
  
  true_indices <- as.character(raw_data[index_row_idx, ])
  true_indices[is.na(true_indices) | true_indices == ""] <- paste0("dummy_", seq_along(true_indices)[is.na(true_indices) | true_indices == ""])
  true_indices[1] <- "Item"
  colnames(raw_data) <- true_indices
  
  data_body <- raw_data[(index_row_idx + 1):nrow(raw_data), ]
  
  # clean text and extract target variables
  clean_body <- data_body %>%
    mutate(Year = year, .before = 1) %>%
    
    # standardize text and drop empty or irrelevant rows
    mutate(Item = str_squish(str_remove_all(Item, "\\.+$"))) %>%
    mutate(Item = na_if(Item, "")) %>%
    fill(Item, .direction = "down") %>%
    filter(!is.na(Item), !str_detect(Item, "^Footnotes|^Note:|^Source:|^\\[")) %>%
    
    # isolate target line items using regex
    filter(
      str_detect(Item, "(?i)^Number of returns") | 
        str_detect(Item, "(?i)^Cash") |
        str_detect(Item, "(?i)investments? in government obligations|(?i)u\\.?\\s*s\\.?\\s*government obligations") |
        str_detect(Item, "(?i)than one year|(?i)than 1 year|(?i)year or more") |
        str_detect(Item, "(?i)^Total assets") |
        str_detect(Item, "(?i)^Inventories") |
        str_detect(Item, "(?i)^Depreciable assets") |
        str_detect(Item, "(?i)accumulated depreciation") |
        str_detect(Item, "(?i)^Land") |
        str_detect(Item, "(?i)^Net worth|^Total net worth") |
        str_detect(Item, "(?i)^Capital stock") |
        str_detect(Item, "(?i)paid.in capital|(?i)capital surplus") |
        str_detect(Item, "(?i)Retained earnings") |
        str_detect(Item, "(?i)Adjustments to.*equity|(?i)Adjustments to net worth") |
        str_detect(Item, "(?i)Cost of treasury stock") |
        str_detect(Item, "(?i)^Total receipts$") |
        str_detect(Item, "(?i)^Cost of goods sold")  |
        str_detect(Item, "(?i)^Accounts payable") |
        str_detect(Item, "(?i)^Loans from shareholders") |
        str_detect(Item, "(?i)^Other current liabilities")
    ) %>%
    mutate(
      Variable = case_when(
        str_detect(Item, "(?i)^Number of returns") ~ "Number_of_returns", 
        str_detect(Item, "(?i)^Cash") ~ "Cash",
        str_detect(Item, "(?i)investments? in government obligations|(?i)u\\.?\\s*s\\.?\\s*government obligations") ~ "Gov_Obligations",
        str_detect(Item, "(?i)than one year|(?i)than 1 year") ~ "Short_Term_Debt",
        str_detect(Item, "(?i)year or more") ~ "Long_Term_Debt",
        str_detect(Item, "(?i)^Total assets") ~ "Total_Assets",
        str_detect(Item, "(?i)^Inventories") ~ "Inventories",
        str_detect(Item, "(?i)^Depreciable assets") ~ "Depreciable_Assets",
        str_detect(Item, "(?i)accumulated depreciation") ~ "Accumulated_Depreciation",
        str_detect(Item, "(?i)^Land") ~ "Land",
        str_detect(Item, "(?i)^Net worth|^Total net worth") ~ "Total_Net_Worth",
        str_detect(Item, "(?i)^Capital stock") ~ "Capital_Stock",
        str_detect(Item, "(?i)paid.in capital|(?i)capital surplus") ~ "Paid_In_Capital",
        str_detect(Item, "(?i)unappropriated") ~ "RE_Unapprop",
        str_detect(Item, "(?i)appropriated") ~ "RE_Approp",
        str_detect(Item, "(?i)Adjustments") ~ "Equity_Adjustments",
        str_detect(Item, "(?i)treasury stock") ~ "Treasury_Stock",
        str_detect(Item, "(?i)^Total receipts$") ~ "Total_Receipts",
        str_detect(Item, "(?i)^Cost of goods sold") ~ "Cost_Goods_Sold", 
        str_detect(Item, "(?i)^Accounts payable") ~ "Acc_Payable",
        str_detect(Item, "(?i)^Loans from shareholders") ~ "Loans_Shareholders",
        str_detect(Item, "(?i)^Other current liabilities") ~ "Other_Curr_Liabilities",
        TRUE ~ NA_character_
      ), .after = Year
    ) %>%
    filter(!is.na(Variable))
  
  year_res <- tibble(Year = year, Variable = clean_body$Variable)
  
  # extract mapped columns and calculate sector sums
  for (i in seq_len(nrow(sector_mapping))) {
    sector_name <- sector_mapping$Target_Sector[i]
    mapped_indices_str <- sector_mapping[[year_col]][i]
    
    if (is.na(mapped_indices_str) || mapped_indices_str == "") next
    
    col_indices <- as.character(as.numeric(str_trim(unlist(str_split(mapped_indices_str, ",")))))
    
    valid_cols <- intersect(col_indices, colnames(clean_body))
    if (length(valid_cols) == 0) next
    
    subset_cols <- clean_body %>% select(all_of(valid_cols))
    
    # parse numbers, converting literal dashes and IRS disclosure 'd' flags to NA
    subset_cols <- subset_cols %>%
      mutate(across(everything(), ~ {
        cell_text <- str_trim(as.character(.))
        case_when(
          str_detect(cell_text, "^-+$") ~ NA_real_,         
          str_detect(cell_text, "(?i)^d\\*?$") ~ NA_real_,  
          TRUE ~ suppressWarnings(parse_number(str_remove_all(cell_text, "\\*")))
        )
      }))
    
    # sum values across valid columns for each row
    year_res[[sector_name]] <- apply(subset_cols, 1, function(row_vals) {
      if (all(is.na(row_vals))) {
        NA_real_
      } else {
        sum(row_vals, na.rm = TRUE)
      }
    })
  }
  
  # reshape to standard format
  year_res_long <- year_res %>%
    pivot_longer(cols = -c(Year, Variable), names_to = "Sector", values_to = "Value") %>%
    pivot_wider(names_from = Variable, values_from = Value, values_fn = max) 
  
  return(year_res_long)
})


# calculate leverage and reconstruct equity

expected_cols <- c(
  "Number_of_returns",
  "Cash", "Gov_Obligations", "Total_Receipts", "Cost_Goods_Sold", 
  "Total_Net_Worth", "Capital_Stock", "Paid_In_Capital", "RE_Approp", 
  "RE_Unapprop", "Equity_Adjustments", "Treasury_Stock", "Loans_Shareholders",
  "Total_Assets", "Acc_Payable", "Other_Curr_Liabilities", "Short_Term_Debt", "Long_Term_Debt",
  "Inventories", "Depreciable_Assets", "Accumulated_Depreciation", "Land"
)

# initialize expected columns with NA if missing
for (col in expected_cols) {
  if (!col %in% names(master_panel)) master_panel[[col]] <- NA_real_ 
}

master_panel <- master_panel %>%
  mutate(across(all_of(expected_cols), ~ as.numeric(.))) %>%
  
  # impute 0 strictly for missing government bonds
  mutate(Gov_Obligations = replace_na(Gov_Obligations, 0)) %>%
  
  # convert literal 0s to NA in core denominators to prevent Inf
  mutate(
    Total_Receipts = na_if(Total_Receipts, 0),
    Total_Assets = na_if(Total_Assets, 0),
    Number_of_returns = na_if(Number_of_returns, 0)
  ) %>%
  
  mutate(
    Calculated_Net_Worth = case_when(
      !is.na(Total_Net_Worth) & Total_Net_Worth != 0 ~ Total_Net_Worth,
      is.na(Capital_Stock) & is.na(Paid_In_Capital) & is.na(RE_Approp) & is.na(RE_Unapprop) & is.na(Equity_Adjustments) & is.na(Treasury_Stock) ~ NA_real_,
      TRUE ~ replace_na(Capital_Stock, 0) + replace_na(Paid_In_Capital, 0) + replace_na(RE_Approp, 0) + replace_na(RE_Unapprop, 0) + replace_na(Equity_Adjustments, 0) - replace_na(Treasury_Stock, 0)
    ),
    
    Leverage_Assets = case_when(
      is.na(Short_Term_Debt) & is.na(Long_Term_Debt) & is.na(Loans_Shareholders) ~ NA_real_,
      TRUE ~ (replace_na(Short_Term_Debt, 0) + replace_na(Long_Term_Debt, 0) + replace_na(Loans_Shareholders, 0)) / Total_Assets
    ),
    
    Debt_to_Equity = case_when(
      is.na(Short_Term_Debt) & is.na(Long_Term_Debt) ~ NA_real_,
      TRUE ~ (replace_na(Short_Term_Debt, 0) + replace_na(Long_Term_Debt, 0)) / Calculated_Net_Worth
    )
  ) %>%
  select(-Capital_Stock, -Paid_In_Capital, -RE_Approp, -RE_Unapprop, -Equity_Adjustments, -Treasury_Stock, -Total_Net_Worth) %>%
  rename(Total_Net_Worth = Calculated_Net_Worth) %>%
  
  # ensure calculated equity does not evaluate to 0
  mutate(Total_Net_Worth = na_if(Total_Net_Worth, 0)) %>%
  
  # scrub infinities and NaNs across the dataset
  mutate(across(where(is.numeric), ~ if_else(is.infinite(.) | is.nan(.), NA_real_, .)))

# export harmonized dataset
saveRDS(master_panel, "Data/IRS_sec_final_harmonized.rds")