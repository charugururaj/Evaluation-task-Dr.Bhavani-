
# Project: Evaluation task for Dr. Bhavani
# Date: 9/22/2025
# Author: Charu Gururaj 

# Installing necessary packages

pacman::p_load(readr, dplyr, stringr, lubridate, janitor)

# Setting paths to the datafiles

path_chartevents <- "C:\\Users\\... \\Evaluation Task\\chartevents3.csv"
path_mappings <- "C:\\Users\\... \\Evaluation Task\\mappings.csv"
path_out <- "C:\\Users\\...\\Evaluation Task\\clif_vitals.csv"

# Loading data

ce <- read_csv(path_chartevents,
               col_types = cols(.default = col_guess()),
               progress = TRUE) %>% 
  clean_names()

maps <- read_csv(path_mappings,
                 show_col_types = FALSE) %>% 
  clean_names()

# Checking for missing values in the merge variable

sum(is.na(ce$itemid))
# No row has missing itemID

sum(ce$warning == 1)
# no warnings

# merging the ce and maps: mapping using itemID and attaching mapping

df <- ce %>% 
  semi_join(maps %>% select(itemid), by = "itemid") %>% 
  left_join(maps, by = "itemid")

# creating standardized expected column names

rename_candidates <- c(
  "hadm_id" = "hospitalization_id",
  "storetime" = "recorded_dttm",
  "label_vital_name" = "vital_name",
  "valuenum" = "vital_value",
  "meas_site_name_2" = "meas_site_name"
)

# renaming columns

for (src in names(rename_candidates)) {
  dst <- rename_candidates[[src]]
  if (src %in% names(df) && !(dst %in% names(df))) {
    names(df)[names(df) == src] <- dst
  }
}
# removing outliers

# Defining limits

limits <- tibble::tribble(
  ~vital_category,      ~lower_limit, ~upper_limit,
  "height_inches",               30,           96,
  "weight_kg",                   30,         1100,
  "sbp",                          0,          300,
  "dbp",                          0,          200,
  "map",                          0,          250,
  "pulse",                        0,          300,
  "respiratory_rate",             0,           60,
  "spo2",                        50,          100,
  "temp_c",                      32,           44
)

# applying outlier rules

df_vitals <- df %>% 
  left_join(limits, by= "vital_category") %>% 
  mutate(
    outlier_flag = !is.na(vital_value) & !is.na(lower_limit) & !is.na(upper_limit) &
      (vital_value < lower_limit | vital_value > upper_limit),
    vital_value  = ifelse(outlier_flag, NA_real_, vital_value)
  )

# removing duplicates
# Keeping one value per hospitalization, timestamp, vital, measurement site.

n_before <- nrow(df_vitals)
df_vitals2 <- df_vitals %>% distinct(hospitalization_id, recorded_dttm, vital_name, vital_category,
                                     vital_value, meas_site_name)
n_exact_removed <- n_before - nrow(df_vitals2)

# implement the required datatypes

clif_vitals <- df_vitals2 %>%
  mutate(
    hospitalization_id = as.character(hospitalization_id),
    recorded_dttm      = as.POSIXct(recorded_dttm, tz = "UTC"),
    vital_name         = as.character(vital_name),
    vital_category     = as.character(vital_category),
    vital_value        = as.numeric(vital_value),
    meas_site_name     = as.character(meas_site_name)
  )

arrow::write_parquet(clif_vitals, path_out)


