# load dependencies
library(tidyverse)
library(readxl)
library(mpshock) 

# load raw data
irs_df <- readRDS("Data/IRS_sec_final_harmonized.rds")
bea_va_raw <- read_csv("Data/real_value_added.csv", skip = 3, show_col_types = FALSE)
bea_go_raw <- read_csv("Data/real_gross_output.csv", skip = 3, show_col_types = FALSE)
bea_price_raw <- read_csv("Data/price_index.csv", skip = 3, show_col_types = FALSE)
bea_price_v_raw <- read_csv("Data/price_index_v.csv", skip = 3, show_col_types = FALSE)
bds_raw <- read_csv("Data/bds2023_vcn3_fa.csv", na = c("", "NA", "X"), show_col_types = FALSE)
fpa_raw <- read_csv2("Data/price_stickiness.csv", show_col_types = FALSE)

# process monetary policy shocks and aggregate to quarterly frequency
shocks_brw <- read_csv("Data/Shocks/brw-shock-series.csv", show_col_types = FALSE) %>%
  select(month, `BRW_monthly (updated)`) %>%
  drop_na(month) %>%
  mutate(
    Year = as.numeric(str_extract(month, "^\\d{4}")),
    Month_num = as.numeric(str_extract(month, "\\d+$")),
    Quarter = (Month_num - 1) %/% 3 + 1,
    Time = paste0(Year, "Q", Quarter)
  ) %>%
  group_by(Time) %>%
  summarize(MP_brw = sum(`BRW_monthly (updated)`, na.rm = TRUE), .groups = "drop")

shocks_jk <- mpshock::jarocinski_karadi_mp %>%
  mutate(date = as.Date(date),
         Year = year(date),
         Month = month(date),
         Quarter = (Month - 1) %/% 3 + 1,
         Time = paste0(Year, "Q", Quarter)) %>%
  group_by(Time) %>%
  summarize(MP_jk = sum(shock, na.rm = TRUE), .groups = "drop")

# extract dates and compute intra-quarter weights for high-frequency shocks
temp_brw <- read_csv("Data/Shocks/brw-shock-series.csv", show_col_types = FALSE) %>%
  select(date_fomc = `date_fomc...3`, `BRW_fomc (updated)`) %>%
  drop_na(date_fomc) %>%
  mutate(
    date = dmy(date_fomc),
    Year = year(date),
    Quarter = quarter(date),
    Time = paste0(Year, "Q", Quarter),
    
    q_start = floor_date(date, "quarter"),
    q_end = ceiling_date(date, "quarter") - days(1),
    
    days_in_q = as.numeric(q_end - q_start) + 1,
    days_left = as.numeric(q_end - date) + 1,
    weight = days_left / days_in_q
  )

# apply weights and sum strictly within the contemporaneous quarter
shocks_brw_tw <- temp_brw %>%
  mutate(shock_tw = `BRW_fomc (updated)` * weight) %>%
  group_by(Time) %>%
  summarize(MP_brw_tw = sum(shock_tw, na.rm = TRUE), .groups = "drop") %>%
  arrange(Time)

# load and process macroeconomic aggregates, calculating quarterly growth rates
gdp <- read.csv("Data/Aggregates/agg_GDP.csv") %>%
  mutate(
    obs_date_clean = as.Date(observation_date),
    Year = year(obs_date_clean),
    Month = month(obs_date_clean),
    Quarter = (Month - 1) %/% 3 + 1,
    Time = paste0(Year, "Q", Quarter) 
  ) %>%
  mutate(gdp_growth = 100 * (log(GDPC1) - log(dplyr::lag(GDPC1, 1)))) %>%
  select(Time, gdp = GDPC1, gdp_growth)

comm_price_index <- read.csv("Data/Aggregates/comm_index.csv") %>%
  mutate(
    obs_date_clean = as.Date(observation_date),
    Year = year(obs_date_clean),
    Month = month(obs_date_clean),
    Quarter = (Month - 1) %/% 3 + 1,
    Time = paste0(Year, "Q", Quarter) 
  ) %>%
  mutate(cpi_growth = 100 * (log(PALLFNFINDEXQ) - log(dplyr::lag(PALLFNFINDEXQ, 1)))) %>%
  select(Time, cpi = PALLFNFINDEXQ, cpi_growth) 

pce <- read.csv("Data/Aggregates/agg_PCE.csv") %>%
  mutate(
    obs_date_clean = as.Date(observation_date),
    Year = year(obs_date_clean),
    Month = month(obs_date_clean),
    Quarter = (Month - 1) %/% 3 + 1,
    Time = paste0(Year, "Q", Quarter) 
  ) %>%
  mutate(inflation = 100 * (log(PCEPILFE) - log(dplyr::lag(PCEPILFE, 1)))) %>%
  select(Time, Agg_pce = PCEPILFE, inflation)

gs1 <- read.csv("Data/Aggregates/GS1.csv") %>%
  mutate(
    obs_date_clean = as.Date(observation_date),
    Year = year(obs_date_clean),
    Month = month(obs_date_clean),
    Quarter = (Month - 1) %/% 3 + 1,
    Time = paste0(Year, "Q", Quarter) 
  ) %>%
  group_by(Time) %>%
  summarize(GS1 = mean(GS1, na.rm = TRUE), .groups = "drop")

gs2 <- read.csv("Data/Aggregates/GS2.csv") %>%
  mutate(
    obs_date_clean = as.Date(observation_date),
    Year = year(obs_date_clean),
    Month = month(obs_date_clean),
    Quarter = (Month - 1) %/% 3 + 1,
    Time = paste0(Year, "Q", Quarter) 
  ) %>%
  group_by(Time) %>%
  summarize(GS2 = mean(GS2, na.rm = TRUE), .groups = "drop")

ebp <- read.csv("Data/Aggregates/ebp.csv") %>%
  mutate(
    obs_date_clean = as.Date(date),
    Year = year(obs_date_clean),
    Month = month(obs_date_clean),
    Quarter = (Month - 1) %/% 3 + 1,
    Time = paste0(Year, "Q", Quarter) 
  ) %>%
  group_by(Time) %>%
  summarize(
    ebp = mean(ebp, na.rm = TRUE),
    gz_spread = mean(gz_spread, na.rm = TRUE),
    .groups = "drop"
  )

# merge monetary policy shocks and macro aggregates into a single dataset
macro_vars <- shocks_brw %>%
  full_join(shocks_brw_tw, by = "Time") %>%
  full_join(gdp, by = "Time") %>%
  full_join(comm_price_index, by = "Time") %>%
  full_join(pce, by = "Time") %>%
  full_join(gs1, by = "Time") %>%
  full_join(gs2, by = "Time") %>%
  full_join(ebp, by = "Time")

# reshape bea data to long format and extract numerical years
bea_va_raw <- bea_va_raw %>%
  select(-1) %>%
  rename(BEA_Sector = 1) %>%
  mutate(BEA_Sector = str_trim(BEA_Sector)) %>%
  pivot_longer(
    cols = -BEA_Sector,
    names_to = "Year_Quarter_Raw",
    values_to = "Real_Value_Added"
  ) %>%
  mutate(Year = as.numeric(str_extract(Year_Quarter_Raw, "^\\d{4}"))) %>%
  filter(!is.na(Year)) %>%
  group_by(BEA_Sector, Year) %>%
  mutate(Quarter = row_number()) %>%
  ungroup() %>%
  mutate(Real_Value_Added = as.numeric(str_replace_all(Real_Value_Added, "---|[,]", "")))

bea_go_raw <- bea_go_raw %>%
  select(-1) %>%
  rename(BEA_Sector = 1) %>%
  mutate(BEA_Sector = str_trim(BEA_Sector)) %>%
  pivot_longer(
    cols = -BEA_Sector,
    names_to = "Year_Quarter_Raw",
    values_to = "Real_Gross_Output"
  ) %>%
  mutate(Year = as.numeric(str_extract(Year_Quarter_Raw, "^\\d{4}"))) %>%
  filter(!is.na(Year)) %>%
  group_by(BEA_Sector, Year) %>%
  mutate(Quarter = row_number()) %>%
  ungroup() %>%
  mutate(Real_Gross_Output = as.numeric(str_replace_all(Real_Gross_Output, "---|[,]", "")))

bea_price_raw <- bea_price_raw %>%
  select(-1) %>%
  rename(BEA_Sector = 1) %>%
  mutate(BEA_Sector = str_trim(BEA_Sector)) %>%
  pivot_longer(
    cols = -BEA_Sector,
    names_to = "Year_Quarter_Raw",
    values_to = "Price_Index"
  ) %>%
  mutate(Year = as.numeric(str_extract(Year_Quarter_Raw, "^\\d{4}"))) %>%
  filter(!is.na(Year)) %>%
  group_by(BEA_Sector, Year) %>%
  mutate(Quarter = row_number()) %>%
  ungroup() %>%
  mutate(Price_Index = as.numeric(str_replace_all(Price_Index, "---|[,]", "")))

bea_price_v_raw <- bea_price_v_raw %>%
  select(-1) %>%
  rename(BEA_Sector = 1) %>%
  mutate(BEA_Sector = str_trim(BEA_Sector)) %>%
  pivot_longer(
    cols = -BEA_Sector,
    names_to = "Year_Quarter_Raw",
    values_to = "Price_Index_va"
  ) %>%
  mutate(Year = as.numeric(str_extract(Year_Quarter_Raw, "^\\d{4}"))) %>%
  filter(!is.na(Year)) %>%
  group_by(BEA_Sector, Year) %>%
  mutate(Quarter = row_number()) %>%
  ungroup() %>%
  mutate(Price_Index_va = as.numeric(str_replace_all(Price_Index_va, "---|[,]", "")))

# define a crosswalk to map 3-digit NAICS codes to BEA sectors
# a tribble allows multiple BEA sub-sectors to be associated with a single NAICS code
bds_bea_crosswalk <- tribble(
  ~vcnaics3, ~BEA_Sector,
  "113", "Forestry, fishing, and related activities",
  "114", "Forestry, fishing, and related activities",
  "115", "Forestry, fishing, and related activities",
  "211", "Oil and gas extraction",
  "212", "Mining, except oil and gas",
  "213", "Support activities for mining",
  "221", "Utilities",
  "236", "Construction",
  "237", "Construction",
  "238", "Construction",
  "311", "Food and beverage and tobacco products",
  "312", "Food and beverage and tobacco products",
  "313", "Textile mills and textile product mills",
  "314", "Textile mills and textile product mills",
  "315", "Apparel and leather and allied products",
  "316", "Apparel and leather and allied products",
  "321", "Wood products",
  "322", "Paper products",
  "323", "Printing and related support activities",
  "324", "Petroleum and coal products",
  "325", "Chemical products",
  "326", "Plastics and rubber products",
  "327", "Nonmetallic mineral products",
  "331", "Primary metals",
  "332", "Fabricated metal products",
  "333", "Machinery",
  "334", "Computer and electronic products",
  "335", "Electrical equipment, appliances, and components",
  "336", "Motor vehicles, bodies and trailers, and parts",
  "336", "Other transportation equipment",
  "337", "Furniture and related products",
  "339", "Miscellaneous manufacturing",
  "423", "Wholesale trade",
  "424", "Wholesale trade",
  "425", "Wholesale trade",
  "441", "Motor vehicle and parts dealers",
  "442", "Other retail",
  "443", "Other retail",
  "444", "Other retail",
  "445", "Food and beverage stores",
  "446", "Other retail",
  "447", "Other retail",
  "448", "Other retail",
  "451", "Other retail",
  "452", "General merchandise stores",
  "453", "Other retail",
  "454", "Other retail",
  "481", "Air transportation",
  "482", "Rail transportation",
  "483", "Water transportation",
  "484", "Truck transportation",
  "485", "Transit and ground passenger transportation",
  "486", "Pipeline transportation",
  "487", "Other transportation and support activities",
  "488", "Other transportation and support activities",
  "492", "Other transportation and support activities",
  "493", "Warehousing and storage",
  "511", "Publishing industries, except internet (includes software)",
  "512", "Motion picture and sound recording industries",
  "515", "Broadcasting and telecommunications",
  "517", "Broadcasting and telecommunications",
  "518", "Data processing, internet publishing, and other information services",
  "519", "Data processing, internet publishing, and other information services",
  "541", "Legal services",
  "541", "Computer systems design and related services",
  "541", "Miscellaneous professional, scientific, and technical services",
  "551", "Management of companies and enterprises",
  "561", "Administrative and support services",
  "562", "Waste management and remediation services",
  "611", "Educational services",
  "621", "Ambulatory health care services",
  "622", "Hospitals",
  "623", "Nursing and residential care facilities",
  "624", "Social assistance",
  "711", "Performing arts, spectator sports, museums, and related activities",
  "712", "Performing arts, spectator sports, museums, and related activities",
  "713", "Amusements, gambling, and recreation industries",
  "721", "Accommodation",
  "722", "Food services and drinking places",
  "811", "Other services, except government",
  "812", "Other services, except government",
  "813", "Other services, except government"
)

# aggregate irs data to the bea sectors and compute financial ratios
safe_sum <- function(x) {
  if (all(is.na(x))) NA_real_ else sum(x, na.rm = TRUE)
}

irs_quarterly <- irs_df %>%
  rename(BEA_Sector = Sector) %>% 
  filter(!is.na(BEA_Sector)) %>%
  group_by(Year, BEA_Sector) %>%
  summarize(
    across(
      c(Total_Assets, Acc_Payable, Other_Curr_Liabilities, Short_Term_Debt, Long_Term_Debt, Total_Net_Worth, 
        Inventories, Depreciable_Assets, Accumulated_Depreciation, Land, Loans_Shareholders,
        Cash, Gov_Obligations, Total_Receipts, Cost_Goods_Sold, Number_of_returns), 
      safe_sum
    ),
    .groups = "drop"
  ) %>%
  mutate(
    Total_Receipts = na_if(Total_Receipts, 0),
    Total_Assets = na_if(Total_Assets, 0),
    Total_Net_Worth = na_if(Total_Net_Worth, 0),
    Cost_Goods_Sold = na_if(Cost_Goods_Sold, 0)
  ) %>%
  mutate(
    Leverage_Assets = (Short_Term_Debt + Long_Term_Debt + Loans_Shareholders) / Total_Assets,
    Leverage_Assets_2 = (Total_Assets - Total_Net_Worth) / Total_Assets,
    St_ratio = Short_Term_Debt / (Short_Term_Debt + Long_Term_Debt),
    Debt_to_Equity = (Short_Term_Debt + Long_Term_Debt) / Total_Net_Worth,
    Asset_Tangibility = (Depreciable_Assets - Accumulated_Depreciation + Land) / Total_Assets,
    Current_Liabilities = Acc_Payable + Short_Term_Debt + Other_Curr_Liabilities,
    Quick_ratio = (Cash + Gov_Obligations) / Current_Liabilities,
    KY_ratio = (Depreciable_Assets - Accumulated_Depreciation) / Total_Receipts,
    Gross_Margin = (Total_Receipts - Cost_Goods_Sold) / Total_Receipts,
    Avg_Firm_Size = case_when(
      Number_of_returns == 0 | is.na(Number_of_returns) ~ NA_real_,
      TRUE ~ Total_Assets / Number_of_returns
    ),
  ) %>%
  crossing(Quarter = 1:4) %>%
  select(Year, Quarter, BEA_Sector, Total_Assets, Current_Liabilities, Short_Term_Debt, Long_Term_Debt, 
         Total_Net_Worth, Total_Receipts, KY_ratio, Debt_to_Equity, Leverage_Assets, Leverage_Assets_2,
         Depreciable_Assets, Accumulated_Depreciation, Inventories, Land, Cost_Goods_Sold,
         Asset_Tangibility, Cash, Gov_Obligations, St_ratio, Quick_ratio, Gross_Margin, Avg_Firm_Size)

# map bds data to bea sectors and calculate the share of young firms
bds_quarterly <- bds_raw %>%
  mutate(across(firms:firmdeath_emp, as.numeric)) %>%
  mutate(vcnaics3 = as.character(vcnaics3)) %>%
  inner_join(bds_bea_crosswalk, by = "vcnaics3") %>%
  filter(!is.na(BEA_Sector)) %>%
  mutate(
    is_young = if_else(fage %in% c("a) 0", "b) 1", "c) 2", "d) 3", "e) 4", "f) 5"), 1, 0)
  ) %>%
  group_by(year, BEA_Sector) %>%
  summarize(
    BDS_Total_Firms = sum(firms, na.rm = TRUE),
    BDS_Young_Firms = sum(firms[is_young == 1], na.rm = TRUE),
    
    .groups = "drop"
  ) %>%
  mutate(
    Young_Firm_Share = BDS_Young_Firms / BDS_Total_Firms,
  ) %>%
  rename(Year = year) %>%
  crossing(Quarter = 1:4) %>%
  
  select(Year, Quarter, BEA_Sector, BDS_Total_Firms, Young_Firm_Share)

# map price stickiness (fpa) data to bea sectors
fpa_base <- fpa_raw %>%
  mutate(vcnaics3 = str_sub(str_trim(io_code), 1, 3)) %>%
  inner_join(
    bds_bea_crosswalk %>%
      bind_rows(
        tibble(vcnaics3 = "420", BEA_Sector = "Wholesale trade"),
        tibble(vcnaics3 = "4A0", BEA_Sector = "Other retail"),
        tibble(vcnaics3 = "4A0", BEA_Sector = "Food and beverage stores"),
        tibble(vcnaics3 = "4A0", BEA_Sector = "Motor vehicle and parts dealers"),
        tibble(vcnaics3 = "4A0", BEA_Sector = "General merchandise stores"),
        tibble(vcnaics3 = "48A", BEA_Sector = "Other transportation and support activities")
      ), 
    by = "vcnaics3"
  ) %>%
  group_by(BEA_Sector) %>%
  summarize(FPA = mean(as.numeric(FPA), na.rm = TRUE), .groups = "drop")

# filter out aggregate and non-private sectors from the bea dataset
sectors_to_drop <- c(
  "Gross domestic product", "Private industries", "Not allocated by industry1",
  "Addenda:", "All industries", "Private goods-producing industries1",
  "Private goods-producing industries2", "Private services-producing industries2",
  "Private services-producing industries3",
  "Information-communications-technology-producing industries 4",
  "Information-communications-technology-producing industries3",
  "Federal", "General government", "Government", "Government enterprises", 
  "National defense", "Nondefense", "State and local",
  "Federal Reserve banks, credit intermediation, and related activities",
  "Finance and insurance", "Finance, insurance, real estate, rental, and leasing",
  "Funds, trusts, and other financial vehicles", "Insurance carriers and related activities",
  "Securities, commodity contracts, and investments", "Housing", "Other real estate",
  "Real estate", "Real estate and rental and leasing", 
  "Rental and leasing services and lessors of intangible assets",
  "Accommodation and food services",
  "Administrative and waste management services",
  "Agriculture, forestry, fishing, and hunting",
  "Arts, entertainment, and recreation",
  "Arts, entertainment, recreation, accommodation, and food services",
  "Durable goods",
  "Educational services, health care, and social assistance",
  "Health care and social assistance",
  "Information",
  "Manufacturing",
  "Mining",
  "Nondurable goods",
  "Professional and business services",
  "Professional, scientific, and technical services",
  "Retail trade",
  "Transportation and warehousing"
)

bea_panel <- bea_va_raw %>%
  full_join(bea_go_raw, by = c("Year", "Quarter", "BEA_Sector")) %>%
  full_join(bea_price_raw, by = c("Year", "Quarter", "BEA_Sector")) %>%
  full_join(bea_price_v_raw, by = c("Year", "Quarter", "BEA_Sector")) %>%
  filter(!is.na(BEA_Sector) & !(BEA_Sector %in% sectors_to_drop)) %>%
  mutate(Time = paste0(Year, "Q", Quarter)) %>%
  select(BEA_Sector, Time, Year, Quarter, Real_Value_Added, 
         Real_Gross_Output, Price_Index, Price_Index_va)

# construct the master panel by expanding the sector-time grid and merging all datasets
all_times <- unique(macro_vars$Time)
all_sectors <- unique(bea_panel$BEA_Sector)

final_panel_59 <- expand_grid(BEA_Sector = all_sectors, Time = all_times) %>%
  mutate(
    Year = as.numeric(substr(Time, 1, 4)),
    Quarter = as.numeric(substr(Time, 6, 6))
  ) %>%
  left_join(macro_vars, by = "Time") %>%
  left_join(bea_panel, by = c("Time", "BEA_Sector", "Year", "Quarter")) %>%
  left_join(irs_quarterly, by = c("BEA_Sector", "Year", "Quarter")) %>%
  left_join(bds_quarterly, by = c("BEA_Sector", "Year", "Quarter")) %>%
  left_join(fpa_base, by = "BEA_Sector") %>%
  filter(Year >= 2002) %>%
  rename(Sector = BEA_Sector) %>%
  arrange(Sector, Time)

# define durable and nondurable goods sectors
durables <- c(
  "Construction", 
  "Wood products", 
  "Nonmetallic mineral products",
  "Primary metals", 
  "Fabricated metal products", 
  "Machinery",
  "Computer and electronic products", 
  "Electrical equipment, appliances, and components",
  "Motor vehicles, bodies and trailers, and parts", 
  "Other transportation equipment",
  "Furniture and related products", 
  "Miscellaneous manufacturing"
)

nondurables <- c(
  "Farms", 
  "Forestry, fishing, and related activities",
  "Oil and gas extraction", 
  "Mining, except oil and gas", 
  "Support activities for mining",
  "Food and beverage and tobacco products", 
  "Textile mills and textile product mills",
  "Apparel and leather and allied products", 
  "Paper products",
  "Printing and related support activities", 
  "Petroleum and coal products",
  "Chemical products", 
  "Plastics and rubber products"
)

# calculate log outcomes, growth rates, and assign structural sector types
final_panel_59 <- final_panel_59 %>%
  arrange(Sector, Year) %>%
  group_by(Sector) %>%
  mutate(
    l_Va = 100 * log(Real_Value_Added),
    l_go = 100 * log(Real_Gross_Output),
    l_pi = 100 * log(Price_Index),
    l_piv = 100 * log(Price_Index_va),
    VA_growth = l_Va - dplyr::lag(l_Va, 1),
    PI_growth = l_pi - dplyr::lag(l_pi, 1),
    
    Sector_Type = case_when(
      Sector %in% durables ~ "Durable",
      Sector %in% nondurables ~ "Nondurable",
      TRUE ~ "Service"
    ),
    Sector_Type = factor(Sector_Type, levels = c("Service", "Durable", "Nondurable")),
    
    Durable = if_else(Sector_Type == "Durable", 1, 0),
    Nondurable = if_else(Sector_Type == "Nondurable", 1, 0)
  ) %>%
  ungroup()

# collapse the panel to a historical cross-section to prepare for missing data imputation
df_collapsed <- final_panel_59 %>%
  group_by(Sector) %>%
  summarise(
    FPA = first(FPA), 
    Avg_Firm_Size = mean(Avg_Firm_Size, na.rm = TRUE),
    Depreciable_Assets = mean(Depreciable_Assets, na.rm = TRUE),
    KY_ratio = mean(KY_ratio, na.rm = TRUE),
    Accumulated_Depreciation = mean(Accumulated_Depreciation, na.rm = TRUE),
    .groups = 'drop'
  )

# isolate training data (non-missing) and missing data for the imputation model
impute_data <- df_collapsed %>%
  select(Sector, FPA, Depreciable_Assets, Avg_Firm_Size, KY_ratio,
         Accumulated_Depreciation)

train_data <- impute_data %>% filter(!is.na(FPA))
missing_data <- impute_data %>% filter(is.na(FPA))

# train a k-nearest neighbors (k-nn) model using leave-one-out cross-validation
set.seed(123) 
train_control <- caret::trainControl(method = "LOOCV")

knn_model <- caret::train(FPA ~ Depreciable_Assets + Avg_Firm_Size + 
                            KY_ratio + Accumulated_Depreciation, 
                          data = train_data, 
                          method = "knn", 
                          preProcess = c("center", "scale"), 
                          trControl = train_control,
                          tuneGrid = expand.grid(k = 1:5))

# generate predictions and inject the imputed values back into the master panel
predictions <- predict(knn_model, newdata = missing_data)

final_panel_59 <- final_panel_59 %>%
  mutate(
    Imputed_FPA = if_else(is.na(FPA), 1, 0),
    FPA = if_else(is.na(FPA) & Sector %in% missing_data$Sector, 
                  predictions[match(Sector, missing_data$Sector)], 
                  FPA)
  )

# export the final combined dataset
saveRDS(final_panel_59, "Data/dataset_final.rds")