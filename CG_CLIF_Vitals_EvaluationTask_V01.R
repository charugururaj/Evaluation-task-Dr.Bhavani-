
# Project: Evaluation task for Dr. Bhavani
# Date: 9/22/2025
# Author: Charu Gururaj 

# Installing necessary packages

pacman::p_load(readr, dplyr, stringr, lubridate, janitor)

# Setting paths to the datafiles

path_chartevents <- "C:\\Users\\charu\\OneDrive - Emory University\\Desktop\\MSPH\\Resume\\Job Applications\\Aug-2025\\Emory SoM\\Dr. Bhavani\\Evaluation Task\\chartevents3.csv"
path_mappings <- "C:\\Users\\charu\\OneDrive - Emory University\\Desktop\\MSPH\\Resume\\Job Applications\\Aug-2025\\Emory SoM\\Dr. Bhavani\\Evaluation Task\\mappings.csv"
path_out <- "C:\\Users\\charu\\OneDrive - Emory University\\Desktop\\MSPH\\Resume\\Job Applications\\Aug-2025\\Emory SoM\\Dr. Bhavani\\Evaluation Task\\clif_vitals.csv"

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

# implement the required datatypes

df <- df %>%
  mutate(
    hospitalization_id = as.character(hospitalization_id),
    recorded_dttm      = suppressWarnings(lubridate::ymd_hms(recorded_dttm)), # parse datetime
    vital_name         = as.character(vital_name),
    vital_category     = as.character(vital_category),
    vital_value        = as.numeric(vital_value),
    meas_site_name     = as.character(meas_site_name)
  )

# keeping only the required columns for the CLIF vitals category

clif_vitals <- df %>%
  select(
    hospitalization_id,
    recorded_dttm,
    vital_name,
    vital_category,
    vital_value,
    meas_site_name
  )

glimpse(clif_vitals)

write_csv(clif_vitals, path_out)
message("Wrote: ", normalizePath(path_out))



# count the number of unique values in ID

num_unique_id <- length(unique(chartevents_3$subject_id))
print(num_unique_id)

colnames(chartevents_3) <- c("column_one")