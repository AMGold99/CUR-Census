#---------------------------
#
#  Producer Demographics
#  Race and Ethnicity
#
#---------------------------


# load packages
library(tidyverse)

# Race (and Hispanic by Race) ####

# set working directory
setwd("Producer Demographics - Race")

# create vector of all source files
state_files_race <- list.files()

# load and row-bind all state files
race_prelim <- map_dfr(state_files_race, 
        ~ mutate(read_csv(.x),
                 across(where(is.numeric), as.character)))


# create vector of desired race-specific values...
final_race_vars <- race_prelim$`data item` |> 
  unique() |> 
  tibble() |>
  rename(value = 1) |>
  filter(str_detect(value, regex("Number of Producers$", ignore_case = TRUE))) |>
  filter(!str_detect(value, regex("age", ignore_case = TRUE))) |>
  filter(!str_detect(value, regex("combined", ignore_case = TRUE))) |>
  slice(1:21) |>
  pull(value)

# ...and do the same for hispanic (race)
final_hispan_vars <- race_prelim$`data item` |> 
  unique() |> 
  tibble() |>
  rename(value = 1) |>
  filter(str_detect(value, regex("Number of Producers$", ignore_case = TRUE))) |>
  filter(!str_detect(value, regex("age", ignore_case = TRUE))) |>
  filter(!str_detect(value, regex("combined", ignore_case = TRUE))) |>
  filter(str_detect(value, regex("hispanic", ignore_case = TRUE))) |>
  pull(value)

# clean and pivot race and Hispanic-race (no Hispanic-sex) dataset
race_master <- race_prelim |>
  filter(is.element(`data item`,c(final_race_vars, final_hispan_vars))) |>
  select(!c(county, commodity, `county code`, `domain category`)) |>
  rename(demo = "data item",
         FIPS = "state fips") |>
  mutate(demo = str_to_title(demo),
         state = str_to_title(state),
         demo = str_trim(demo),
         demo = str_remove(demo, "^Producers,[:blank:]"),
         demo = str_remove(demo, "[:blank:]-[:blank:]Number Of Producers$"),
         value = str_remove_all(value, ",")) |>
  pivot_wider(
    names_from = demo,
    values_from = value
  ) |>
  mutate(across(Producers:`Hispanic, Asian`, as.numeric)) |>
  suppressWarnings() |>
  mutate(across(is.numeric, ~ replace_na(.x, 0)))









# Hispanic by Sex ####

# set working directory
setwd("../Producer Demographics - Hispanic")


state_files_hispanic <- list.files()

hispan_prelim <- map_dfr(state_files_hispanic, 
                       ~ mutate(read_csv(.x),
                                across(where(is.numeric), as.character)))

hispan_sex_vars <- hispan_prelim$`data item` |> 
  unique() |>
  tibble() |> 
  rename(value = 1) |>
  filter(!str_detect(value, "(Principal|Occupation|Residence|Manager|Age|Days|Years)")) |>
  slice(1,3,5) |> 
  pull(value)

hispan_master <- hispan_prelim |>
  filter(is.element(`data item`,hispan_sex_vars)) |>
  select(!c(county, commodity, `county code`, `domain category`)) |>
  rename(demo = "data item",
         FIPS = "state fips") |>
  mutate(demo = str_to_title(demo),
         state = str_to_title(state),
         demo = str_trim(demo),
         demo = str_remove(demo, "^Producers,[:blank:]"),
         demo = str_remove(demo, "[:blank:]-[:blank:]Number Of Producers$"),
         value = str_remove_all(value, ","),
         value = as.numeric(value)) |>
  pivot_wider(
    names_from = demo,
    values_from = value
  ) |>
  select(!Hispanic)


# Merge ####


# create master df with race and Hispanic (by race and now sex) together
demographic_master <- race_master |>
  left_join(hispan_master) |>
  relocate(Male, .after = "Producers") |>
  relocate(Female, .before = "Male") |>
  arrange(FIPS)



# save as csv to output directory ####
write_csv(demographic_master,
          "../output/producer_demo.csv")


# send to census2022 Drive folder ####

setwd("..")

# authorize google drive connection
drive_auth(email = "gold1@stolaf.edu")

# specify shared drive location
census_id <- as_id("https://drive.google.com/drive/folders/1_08a11RyvSLHbzk9Kj2sylBVHqSmBX1-")

# upload bg_race to census2022
drive_upload("output/producer_demo.csv",
             census_id, 
             overwrite = TRUE)

  

producer_test <- read_csv('output/producer_demo.csv') |>
  pivot_longer(
    cols = Producers:`Hispanic, Female`,
    names_to = "race",
    values_to = "n_state"
  ) |>
  tidyr::separate(col = race, 
                  into = c('first','second'), 
                  sep = ", ", 
                  extra = "drop", 
                  fill = "right")

producer_test|>View()
# 1 = male; 2 = female









