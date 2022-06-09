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

pumapop_vars <- c("B03002_003E", "B02001_003E",
                  "B02001_004E", "B02001_005E",
                  "B03002_012E", "B02001_006E",
                  "B02001_007E", "B02001_008E")

pumaRACE <- getCensus(name = "acs/acs5",
                      vintage = 2020,
                      vars = bgpop_vars,
                      region = "public use microdata area:*")

write_csv(pumaRACE,
          file.path(output_dir, "puma_race.csv")
          )

# Google Drive Connection ####

# authorize google drive connection
drive_auth(email = "gold1@stolaf.edu")

## specify census2022 folder id
census_id <- as_id("https://drive.google.com/drive/folders/1_08a11RyvSLHbzk9Kj2sylBVHqSmBX1-")

# upload bg_race to census2022
drive_upload(file.path(output_dir,"puma_race.csv"),
             census_id,
             overwrite = TRUE)










