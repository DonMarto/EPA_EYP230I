## This file makes the same operations as EDA.ipynb.
## For full commentary on the decisios made, refer to 
## that file, or read the full report.

# Load necessary libraries
library(dplyr)
library(stringr)
library(readr) # Used for correctly parse CSV with commas in fields


# We manually load the file. 
# df <- read.csv("data/vehicles.csv")
df <- read_csv(choose.files()) # 49927 obs of 84 variables


# Cols to drop
cols_to_drop = c(# EV / secondary fuel type
  'charge120', 'charge240', 'cityE', 'combE',
  'evMotor', 'highwayE', 'c240Dscr', 'charge240b', 'c240bDscr',
  # Alternative fuel (type 2) performance
  'cityA08', 'cityA08U', 'co2A', 'co2TailpipeAGpm',
  'combA08', 'combA08U', 'fuelCostA08', 'fuelType',
  'fuelType2', 'ghgScoreA', 'highwayA08', 'highwayA08U',
  'rangeA', 'rangeCityA', 'rangeHwyA', 'UCityA', 'UHighwayA',
  # Unrelated to engine / car performance
  'barrels08', 'barrelsA08', 'engId',
  'fuelCost08', 'id', 'mpgData', 'youSaveSpend', 'createdOn',
  'modifiedOn', 'baseModel')
df_filtered <- df %>% select(-all_of(cols_to_drop))

cols_to_drop = c('city08', 'city08U', 'cityCD', 'cityUF',
  'comb08U', 'combinedCD', 'combinedUF', 'co2', 
  'eng_dscr', 'feScore', 'fuelType1', 'ghgScore',
  'highway08', 'highway08U', 'highwayCD', 'highwayUF',
  'hlv', 'hpv', 'lv2', 'lv4', 'pv2', 'pv4',
  'phevBlended', 'range', 'rangeHwy', 'rangeCity',
  'UCity', 'UHighway',
  'VClass', 'guzzler', 'trans_dscr', 'mfrCode',
  'phevCity', 'phevHwy'
)

df_filtered <- df_filtered %>% select(-all_of(cols_to_drop))


# 1. Binary indicators for non-missing
df_filtered$tCharger <- as.integer(!is.na(df_filtered$tCharger))
df_filtered$sCharger <- as.integer(!is.na(df_filtered$sCharger))

# 2. Map startStop
df_filtered$startStop <- ifelse(!is.na(df_filtered$startStop) &
                                  df_filtered$startStop == "Y", 1, 0)

# 3. Recode drive
df_filtered$drive <- dplyr::recode(df_filtered$drive,
                                   "4-Wheel or All-Wheel Drive" = "4WD/AWD",
                                   "All-Wheel Drive" = "4WD/AWD",
                                   "4-Wheel Drive" = "4WD/AWD",
                                   "Rear-Wheel Drive" = "RWD",
                                   "Front-Wheel Drive" = "FWD"
)

# 4. Filter unwanted drive categories
df_filtered <- df_filtered[!df_filtered$drive %in% c("2-Wheel Drive", "Part-time 4-Wheel Drive"), ]

# 5. Drop NA in drive
df_filtered <- df_filtered[!is.na(df_filtered$drive), ]

# 6. Remove certain atvType
atv_eliminate <- c("EV", "eFCV", "FCV", "CNG", "Bifuel (CNG)", "Bifuel (LPG)")
df_filtered <- df_filtered[!df_filtered$atvType %in% atv_eliminate, ]

# 7. Recode atvType
df_filtered$atvType <- ifelse(is.na(df_filtered$atvType), "Gasoline",
                              dplyr::recode(df_filtered$atvType,
                                            "Diesel" = "Diesel",
                                            "Hybrid" = "Hybrid",
                                            "FFV" = "FFV",
                                            "Plug-in Hybrid" = "Plug-in Hybrid"
                              )
)




# 1. Classification function
clasificar_trans <- function(trany) {
  if (is.na(trany)) {
    return(NA)
  }
  else if (str_detect(trany, "variable gear ratios") || str_detect(trany, "AV")) {
    return("CVT")
  }
  else if (str_starts(trany, "Manual")) {
    return("Manual")
  }
  else {
    return("Automatic")
  }
}

# 2. Apply transformations
df_filtered <- df_filtered %>%
  mutate(
    # Transmission type
    transType = sapply(trany, clasificar_trans),
    
    # Extract second word (gear info)
    gears = sapply(strsplit(trany, " "), `[`, 2)
  ) %>%
  
  # 3. Remove original column
  select(-trany) %>%
  
  # 4. Keep only numeric part of gears
  mutate(
    gears = str_replace_all(gears, "\\D+", ""),
    
    # CVT → NA gears
    gears = ifelse(transType == "CVT", 0, gears)
  ) %>%
  
  # 5. Remove gears == "1"
  filter(gears != "1") %>%
  
  # 6. Convert to numeric
  mutate(
    gears = as.numeric(gears)
  ) %>%
  
  mutate(
    # CVT → NA gears
    gears = ifelse(transType == "CVT", NaN, gears)
  ) %>%
  
  # 7. Adjust comb08
  mutate(
    comb08 = ifelse(
      atvType == "Plug-in Hybrid" & phevComb > 0,
      phevComb,
      comb08
    )
  ) %>%
  
  # 8. Drop phevComb
  # select(-phevComb)

df_filtered <- df_filtered %>%
  mutate(
    transType = factor(transType),
    drive = factor(drive)
  ) %>%
  bind_cols(model.matrix(~ transType + drive, data = .)[, -1] %>% as.data.frame()) %>%
  select(-drive, -transType)

df_filtered$displ_grupo <- cut(
  df_filtered$displ,
  breaks = c(0, 2, 3, 4, 5, 6, 7),
  labels = c("≤2L", "2-3L", "3-4L", "4-5L", "5-6L", "6-7L")
)

write.csv(df_filtered, file = file.choose(), quote = FALSE, row.names = FALSE)
