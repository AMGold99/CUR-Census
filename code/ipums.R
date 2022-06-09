#--------------------------
#
#   IPUMS (Extract 16)
#      Working Age 
#    by Race and Sex
#           -
#     Poverty Levels: 
#    1.00, 1.25, 1.30, 
#       1.50, 2.00
#
#--------------------------



# Load packages ####
library(tidyverse)
library(ipumsr)

# read in IPUMS data ####
ddi <- read_ipums_ddi("~/Land_Use_Rights/Census/usa_00016.xml")
data <- read_ipums_micro(ddi) # very large file


# process CONUS data sample (extract 16 on Asa's IPUMS account)
pctwaa_puma <- data |>
  
  # remove those in group quarters
  filter(GQ %in% c(1,2)) |>
  
  # create new race variable
  mutate(race_final = case_when(
    RACE == 1 & HISPAN == 0 ~ 1,
    RACE == 2 ~ 2,
    RACE == 3 ~ 3,
    RACE %in% c(4,5,6) ~ 4,
    HISPAN != 0 ~ 5,
    RACE == 7 ~ 6,
    RACE == 8 ~ 7,
  )) |>
  # remove all three or more races
  filter(RACE != 9) |>
  
  # add leading zeroes to 3-digit puma values
  mutate(PUMA = case_when(
    nchar(PUMA) == 3 ~ paste0("0",PUMA),
    TRUE ~ as.character(PUMA)
  ))

pctwaa_puma1 <- pctwaa_puma |>
  
  # add leading zeroes to 1-digit state fips
  mutate(STATEFIP = case_when(
    nchar(STATEFIP) == 1 ~ paste0("0",STATEFIP),
    TRUE ~ as.character(STATEFIP)
  )) |>
  
  # combine state and pumas to get unique geoids
  mutate(GEOID = paste0(STATEFIP,PUMA)) |>
  
  # create categorical vars for poverty level
  mutate(povlevel = case_when(
           POVERTY <= 100 ~ "b100",
           POVERTY <= 125 ~ "a100_b125",
           POVERTY <= 130 ~ "a125_b130",
           POVERTY <= 150 ~ "a130_b150",
           POVERTY <= 200 ~ "a150_b200",
           POVERTY > 200 ~ "a200"
           )
         )

pctwaa_puma2 <- pctwaa_puma1 |>
  group_by(GEOID, SEX, race_final, povlevel) |>
  summarise(PERWT = sum(PERWT, na.rm = TRUE)) |>
  
  pivot_wider(
    names_from = povlevel,
    values_from = PERWT
  ) |>
  
  # aggregate across poverty intervals to get cumulative numbers at/below thresholds
  mutate(below100 = b100,
         below125 = sum(b100, a100_b125, na.rm = TRUE),
         below130 = sum(b100, a100_b125, a125_b130, na.rm = TRUE),
         below150 = sum(b100, a100_b125, a125_b130, a130_b150, na.rm = TRUE),
         below200 = sum(b100, a100_b125, a125_b130, a130_b150, a150_b200, na.rm = TRUE),
         above200 = a200,
         total = sum(below200, above200, na.rm = TRUE)) |>
  
  # create proportion variables
  mutate(below100_prop = below100/total,
         below125_prop = below125/total,
         below130_prop = below130/total,
         below150_prop = below150/total,
         below200_prop = below200/total,
         above200_prop = above200/total) |>
  
  # remove poverty interval variables
  select(!(a100_b125:b100)) |>
  
  # replace NAs with 0
  mutate(across(below100:above200_prop, ~ replace_na(.x, 0)))



# Save csv to output directory
write_csv(pctwaa_puma2,
          file = file.path(output_dir, "pctwaa_puma.csv")
          )

# Google Drive Connection ####
census_id <- as_id("https://drive.google.com/drive/folders/1_08a11RyvSLHbzk9Kj2sylBVHqSmBX1-")

drive_upload(
  file.path(output_dir, "pctwaa_puma.csv"),
  census_id
)  
  
  




