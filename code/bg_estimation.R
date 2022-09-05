library(tidyverse)
library(arrow)
## Load Demographic Data ####

## PUMA level
pumaRACE <- read_csv("output/puma_race.csv") |>
  mutate(GEOID_puma = paste0(state, public_use_microdata_area)) |>
  relocate(GEOID_puma, .before = everything()) |>
  pivot_longer(
    cols = WhiteNH:HispanOther,
    names_to = "race",
    values_to = "N_r"
  )

## Block Group Level
bgRACE <- read_csv("output/bg_race.csv") |>
  pivot_longer(
    cols = WhiteNH:HispanOther,
    names_to = "race",
    values_to = "n_r"
  ) |>
  select(!(1)) |>
  rename(GEOID_bg = "GEOID")



## Join Block group and PUMA ####

# join block group data with crosswalk
bg_puma <- bgRACE |>
  rename(bg10 = GEOID_bg) |>
  left_join(bgpuma_crosswalk, by = "bg10") |>
  select(!ends_with("y")) |>
  rename(state = "state.x")

# join bg and puma together
bg_puma1 <- bg_puma |>
  left_join(pumaRACE |> rename(puma10 = "GEOID_puma"),
            by = c("puma10", "race")) |>
  mutate(ndivN = n_r/N_r) |>
  select(!c(n_r, N_r, starts_with("state"), 
            public_use_microdata_area,
            tract, block_group)) |>
  filter(!is.na(puma10)) |>
  mutate(ndivN = replace_na(ndivN, 0))



pov_join <- bg_puma1 |>
  filter(!is.na(puma10)) |>
  rename(race_final = "race") |>
  filter(race_final %in% intersect(unique(bg_puma1$race), unique(pctwaa_puma2$race_final))) |>
  left_join(pctwaa_puma2 |> rename(puma10 = "GEOID"),
            by = c('puma10', 'race_final')) |>
  
  # multiply n_r/N_r by WAA^x_{r,s}
  mutate(across(below100:total, ~ .x * ndivN)) |>
  
  mutate(across(below100:total, ~ replace_na(.x, 0))) |>
  
  # convert race to factor var
  mutate(race_final = factor(race_final, levels = c("WhiteNH", 
                                                    "BlackNH", 
                                                    "AminNH", 
                                                    "AsianNH", 
                                                    "OtherNH", 
                                                    "TwoMore", 
                                                    "WhiteHispan", 
                                                    "BlackHispan", 
                                                    "HispanOther"))) |>
  mutate(SEX = factor(SEX, levels = c('1','2'), 
                      labels = c('Male','Female')))
  


## create full grid template (bg and puma, race, sex)
all_bgs <- unfactor(unique(bgRACE$GEOID_bg))
all_race <- unique(pov_join$race_final)
all_sex <- factor(c('1','2'), labels = c('Male','Female'))

combo_template <- expand.grid(
  'bg10' = all_bgs,
  'race_final' = all_race,
  'SEX' = all_sex
) |>
  tibble() |>
  left_join(bgpuma_crosswalk) |>
  select(!state)

## join pov_join to full grid
pov_join_final <- combo_template |>
  left_join(pov_join) |>
  arrange(bg10, race_final, SEX) |>
  mutate(across(below100:total, ~ replace_na(.x, 0)))




## summary statistics
pov_join_final |> skimr::skim()



## Write to file system ####
write_parquet(pov_join_final,
              "output/bg-estimates.pqt")


## Google Drive Connection
census_id <- as_id("https://drive.google.com/drive/folders/1_08a11RyvSLHbzk9Kj2sylBVHqSmBX1-")

## upload to drive
drive_upload(
  "output/bg-estimates.csv",
  census_id,
  overwrite = TRUE
)

# test edit


