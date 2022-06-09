#----------------------#
#
# Census Demographics
#     Block group
#     Pop by Race
#
#----------------------#


# Load packages ####
#install.packages(c('censusapi','doParallel'))
library(censusapi)
library(tidyverse)
library(googlesheets4)
library(doParallel)
library(googledrive)



# Set API key ####
Sys.setenv(CENSUS_KEY="24a0e6e31ba71d8b3e0f70ba0b4037fd194d6aec")


# Interactive metadata inspection ####
apis <- listCensusApis()

apis |>
  filter(str_detect(title, "American Community Survey") & vintage == "2020")


listCensusMetadata(
  name = "2020/acs/acs5",
  type = "variables")

listCensusMetadata(
  name = "2020/acs/acs5",
  type = "geography")


# Blockgroup variable dictionary (race/ethnicity vars) ####

# specify desired variables
bgpop_vars <- c("B03002_003E", "B02001_003E",
  "B02001_004E", "B02001_005E",	
  "B03002_012E", "B02001_006E",
  "B02001_007E", "B02001_008E")

# fetch relevant metadata
bg_metadata <- listCensusMetadata(
  name = "2020/acs/acs5",
  type = "variables"
)

# create dictionary for desired variables
dictBG <- bg_metadata |>
  filter(name %in% bgpop_vars) |>
  select(name, label) |>
  mutate(label = str_remove(label, "^E.+[:punct:]{2}"),
         label = str_remove_all(label, "[:punct:]"))




# Prep for API Call ####


# first, set up a look up table at county-level, since bg calls
# can only be made one county at a time

# read in comprehensive geoid dataframe
geoid <- read_delim(file = "https://www2.census.gov/geo/docs/maps-data/data/rel2020/blkgrp/tab20_blkgrp20_blkgrp10_natl.txt")

# create reference table of all counties in CONUS
counties <- geoid |>
  
  # select and rename the 2020 block group field
  select(geoid = GEOID_BLKGRP_20) |>
  
  # create unique county identifier: geoid_nt (nt = no tract)
  mutate(state = str_sub(geoid, 1, 2),
         county = str_sub(geoid, 3, 5),
         geoid_nt = paste0(state, county)) |>
  
  # sort
  arrange(state, county)|>
  
  # subset to only CONUS
  filter(state %in% fips & !is.element(state, c('02','15'))) |>
  
  # remove redundant rows
  filter(!duplicated(geoid_nt))





# Race population at block group level; parallel processing ####

# register n nodes for parallel processing of the loop below
cl <- makeCluster(64)
registerDoParallel(cl)
getDoParWorkers()

# loop through each county to fetch block group stats, then collate
bgRACE <- foreach(i=seq_len(nrow(counties)), .combine = "rbind") %dopar% {
  
  # Block group race/ethnicity
  censusapi::getCensus(name = "acs/acs5",
                       vintage = 2020, 
                       vars = bgpop_vars, 
                       region = "block group:*",
                       regionin = paste0("state:",counties$state[i],
                                         "+county:", counties$county[i]))
}

# add unique GEOID identifier
bgRACE <- bgRACE |>
  mutate(GEOID = paste0(state,county,tract,block_group)) |>
  relocate(GEOID, .before = "state") |>
  tibble()


# save as csv to local file system
write.csv(bgRACE,
          file.path(output_dir, "bg_race.csv")
          )




# Google Drive Connection ####

# authorize google drive connection
drive_auth(email = "gold1@stolaf.edu")

# specify shared drive location
census_id <- as_id("https://drive.google.com/drive/folders/1_08a11RyvSLHbzk9Kj2sylBVHqSmBX1-")

# upload bg_race to census2022
drive_upload(file.path(output_dir, "bg_race.csv"),
             census_id, 
             overwrite = TRUE)






