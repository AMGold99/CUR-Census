library(tidyverse)
library(googlesheets4)

## resolve differences
bg_diffs <- range_read(ss = "1p3UB2sRwXmwTFAtcIr_RF5ww2A-RGaOGZyBdW4VzN8c") |>
  mutate(across(everything(), as.character)) |>
  rename(crosswalk = 1,
         acs = 2) |>
  mutate(across(everything(),
                ~ case_when(
                  nchar(.x) == 11 ~ paste0("0", .x),
                  TRUE ~ as.character(.x)
                ))) |>
  rename(bg10 = "crosswalk")


# Add to Crosswalk ####

## bgpuma_cross10 <- read_csv("data/geocorr2018_2216202928.csv",
#                            skip = 1)

# bgpuma_crosswalk <- bgpuma_cross10 |>
#   mutate(Tract = str_remove(Tract, "\\."),
#          bg10 = paste0(`County code`, Tract, `Block group`),
#          puma10 = paste0(`State code`, `PUMA (2012)`)) |>
#   select(bg10, puma10) |>
#   mutate(state = str_sub(bg10, 1, 2))

bgpuma_crosswalk <- read_csv("output/bgpuma_crosswalk") |>
  left_join(bg_diffs) |>
  mutate(bg10 = case_when(
    is.na(acs) ~ bg10,
    !is.na(acs) ~ acs
  ),
  .keep = "unused")
