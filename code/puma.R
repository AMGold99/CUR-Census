#----------------------#
#
# Census Demographics
#       PUMA
#
#----------------------#


# Load packages ####
#install.packages('censusapi')
library(censusapi)
library(tidyverse)
library(googlesheets4)

# set file paths ####

output_dir <- "~/Land_Use_Rights/Census/output"

# Set API key ####
Sys.setenv(CENSUS_KEY="24a0e6e31ba71d8b3e0f70ba0b4037fd194d6aec")


# Interactive metadata inspection ####
apis <- listCensusApis()

# find table name here
apis |>
  filter(str_detect(title, "American Community Survey"))

# use table name to inspect variables 
# (or use type = "geography" to inspect available geographies)
listCensusMetadata(
  name = "2020/acs/acs5/subject",
  type = "variables"
) |> 
  filter(group == "S1703")




# Load data via API call ####


# PUMA Total population by race: N_r

pumapop_vars <- c("B03002_003E", "B03002_004E", "B03002_005E", 
                  "B03002_006E", "B03002_007E", "B03002_008E", 
                  "B03002_009E", "B03002_012E", "B03002_013E", 
                  "B03002_014E", "B03002_015E", "B03002_016E", 
                  "B03002_017E", "B03002_018E", "B03002_019E")

pumaRACE <- getCensus(name = "acs/acs5",
                      vintage = 2020,
                      vars = bgpop_vars,
                      region = "public use microdata area:*") |>
  mutate(
    WhiteNH = B03002_003E,
    BlackNH = B03002_004E,
    AminNH = B03002_005E,
    AsianNH = B03002_006E + B03002_007E,
    OtherNH = B03002_008E,
    TwoMore = B03002_009E,
    Hispan = B03002_012E,
    WhiteHispan = B03002_013E,
    BlackHispan = B03002_014E,
    HispanOther = B03002_015E + B03002_016E + B03002_017E + B03002_018E + B03002_019E,
    .keep = "unused"
  )

write_csv(pumaRACE,
          "output/puma_race.csv")
          

# Google Drive Connection ####

# authorize google drive connection
drive_auth(email = "gold1@stolaf.edu")

## specify census2022 folder id
census_id <- as_id("https://drive.google.com/drive/folders/1_08a11RyvSLHbzk9Kj2sylBVHqSmBX1-")

# upload bg_race to census2022
drive_upload("output/puma_race.csv",
             census_id,
             overwrite = TRUE)










