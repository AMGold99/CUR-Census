
# Import Census Demographics Google Sheets
ss <- as_sheets_id('https://docs.google.com/spreadsheets/d/1awonjlYsNQPUci1jm6lI7t_T5FYGSYR3ZOA9zWuhy34/edit#gid=1706685824')
CD_puma <- read_sheet(ss = ss,
                      sheet = "PUMA")


vars_S1703 <- names(CD_puma)[str_detect(names(CD_puma),"S1703")]

# S1703: Levels of Poverty ####
pumaS1703 <- getCensus(name = "acs/acs5/subject",
                       vintage = 2020, 
                       vars = c("GEO_ID",vars_S1703), 
                       region = "public use microdata area:*") |>
  filter(state != "72") #|>
select(state, GEO_ID, NAME, public_use_microdata_area, all_of(vars_S1703)) |>
  rename(puma = "public_use_microdata_area") |>
  tibble()

# variable dictionary for S1703 table
dictS1703 <- CD_puma |> 
  select(all_of(vars_S1703)) |> 
  slice(1) |> 
  pivot_longer(
    cols = everything(),
    names_to = "name",
    values_to = "label"
  ) |>
  mutate(label = unlist(label))


# B17001: Poverty Status by demographic ####

B17_demo <- glue::glue("B17001{toupper(letters[1:9])}")

B17_list <- vector(mode = "list")

#demographic <- B17_demo[1]
for (demographic in B17_demo) {
  
  male_pov_vars <- paste0(demographic, "_0", c('09',seq(10,14)), "E")
  fem_pov_vars <- paste0(demographic, "_0", seq(23,28), "E")
  
  male_above_vars <- paste0(demographic, "_0", seq(38,43), "E")
  fem_above_vars <- paste0(demographic, "_0", seq(52,57), "E")
  
  male_15_vars <- paste0(demographic, "_0", c("08",37), "E")
  
  fem_15_vars <- paste0(demographic, "_0", c('22','51'), "E")
  
  
  
  B17_obj <- getCensus(name = "acs/acs5",
                       vintage = 2020, 
                       vars = glue::glue("group({demographic})"), 
                       region = "public use microdata area:*") |> 
    
    filter(state != "72") |>
    
    select(state, GEO_ID, public_use_microdata_area,NAME,
           all_of(male_pov_vars),
           all_of(fem_pov_vars),
           all_of(male_above_vars),
           all_of(fem_above_vars),
           all_of(male_15_vars),
           all_of(fem_15_vars),
           total_pop = all_of(paste0(demographic, "_001E"))) |>
    
    rowwise(GEO_ID) |>
    
    mutate(wa_pov_male = sum(across(all_of(male_pov_vars))),
           wa_pov_fem = sum(across(all_of(fem_pov_vars))),
           wa_above_male = sum(across(all_of(male_above_vars))),
           wa_above_fem = sum(across(all_of(fem_above_vars))),
           wa_male = wa_pov_male + wa_above_male,
           wa_fem = wa_pov_fem + wa_above_fem,
           wa_16 = wa_male + wa_fem,
           wa_15 = sum(wa_16, .data[[fem_15_vars[1]]],
                       .data[[fem_15_vars[2]]],
                       .data[[male_15_vars[1]]],
                       .data[[male_15_vars[2]]]),
           ratio_1516 = wa_15/wa_16,
           maleratio = wa_male/total_pop,
           femratio = wa_fem/total_pop
    ) |>
    
    select(state, GEO_ID, NAME, 
           puma = public_use_microdata_area, 
           wa_pov_male, wa_pov_fem, 
           wa_male, wa_fem,
           ratio_1516,
           maleratio, femratio,
           total_pop)
  
  
  B17_list[[demographic]] <- B17_obj
  
  remove(B17_obj)
  
  cat("Completed", demographic, "\n")
  
}


# Write csv files ####

for (i in seq_len(length(B17_list))) {
  
  write_csv(B17_list[[i]],
            file = file.path(output_dir, "PUMA",
                             names(B17_list)[i], ".csv")
  )
  
}


# create Sex Ratio ####

sexratio <- B17_list[[1]] |>
  select(GEO_ID,puma)

for (i in seq_len(length(B17_list))) {
  new_maleratio <- paste0("male", names(B17_list)[[i]])
  new_femratio <- paste0("fem", names(B17_list)[[i]])
  
  temp <- B17_list[[1]] |>
    select(GEO_ID, maleratio, femratio) |>
    rename({{ new_maleratio }} := "maleratio",
           {{ new_femratio}} := "femratio")
  
  sexratio <- left_join(sexratio,temp)
}