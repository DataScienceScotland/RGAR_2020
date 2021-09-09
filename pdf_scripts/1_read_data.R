# Read files ------------------------------------------------------------------
# These files are used multiple times - so we read them in once here. Other
# files are only used once, so they are read in as and when needed.
datasets[["input_data"]] <- paths[["input_data"]] %>%
  excel_sheets() %>%
  set_names() %>%
  map(read_excel, path = paths[["input_data"]], skip = 5, na = "NA")

# 2 COVID-19 ----------------------------------------------------------------

# Deaths by location ------------------------------------------------------

datasets[["COVID_location_deaths"]] <- datasets[["input_data"]][["COVID_deaths_location"]] %>%
  filter(Location != "All") %>% 
  mutate(Date = as.Date(Date),
         Location = ordered(Location, levels = c(
           "Other institution",
           "Home / Non-institution",
           "Care Home",
           "Hospital"))) %>% 
  janitor::clean_names()

# Excess deaths by age ------------------------------------------------------

datasets[["COVID_deaths_age"]] <- datasets[["input_data"]][["COVID_deaths_age"]] %>% 
  mutate(Date = as.Date(Date)) %>% 
  filter(Age %in% c("65 to 74", "75 to 84", "85+")) %>% 
  janitor::clean_names() %>% 
  rename("max" = x5_year_max, 
         "min" = x5_year_min,
         "average" = x5_year_average)

# Death Rates SIMD ------------------------------------------------------

datasets[["COVID_deathrate"]] <- datasets[["input_data"]][["COVID_death_rate"]] %>% 
  mutate(SIMD = as.factor(SIMD)) %>% 
  filter(SIMD %in% c(1, 5)) %>% 
  select(-`Mortality rate`) %>% 
  spread(SIMD, Rate) %>% 
  left_join(y = datasets[["input_data"]][["COVID_death_rate"]] %>% 
          filter(SIMD == 1) %>% 
            mutate(`Mortality rate` = round(`Mortality rate`, digits = 1)) %>% 
          select(`Mortality rate`, Type)) %>% 
  rename("Mortality rate in the most deprived areas (SIMD 1)" = `1`, 
         "Mortality rate in the least deprived areas (SIMD 5)" = `5`,
         "Cause of death" = Type,
         "Ratio between mortality rates in the most and least deprived areas" = `Mortality rate`) 

# Excess deaths by type ------------------------------------------------------
cause_of_death_full <- datasets[["input_data"]][["COVID_cause_of_death"]] %>% 
  janitor::clean_names() %>% 
  rename("date" = month,
         "max" = x5_year_max, 
         "min" = x5_year_min,
         "average" = x5_year_average) %>% 
  mutate(order = 1:nrow(.),
         date = as.Date(paste("01", date, "2020"),  format='%d %b %Y'))

datasets[["COVID_cause_of_death_1"]] <- cause_of_death_full %>% 
  filter(!(type %in% c("Suicide", "Alcohol", "Drugs"))) %>% 
  arrange(order) 

datasets[["COVID_cause_of_death_2"]] <- cause_of_death_full %>% 
  filter(type %in% c("Suicide", "Alcohol", "Drugs"))


# 3 Health inequality -------------------------------------------------------------

# Life Expectancy --------------------------------------------------------------------
datasets[["INEQUALITY_life_expectancy"]] <- datasets[["input_data"]][["INEQUALITY_life_expectancy"]] %>% 
  janitor::clean_names()

# Healthy Life Expectancy --------------------------------------------------------------------
datasets[["INEQUALITY_healthy_LE"]] <- datasets[["input_data"]][["INEQUALITY_healthy_LE"]] %>% 
  janitor::clean_names() %>% 
  mutate(simd = case_when(simd == 10 ~ "10% <b>least deprived</b> areas",
                          simd == 1 ~ "10% <b>most deprived</b> areas"),
         hle_at_birth = paste0(format(round(hle_at_birth, digits = 1), nsmall = 1), 
                               " years in good health (", 
                               round(percentage, digits = 0), 
                               "%)")) %>%
  select("Sex" = sex,
         "Areas" = simd,
         hle_at_birth) %>%
  spread(Sex, hle_at_birth) 



# drug related deaths--------------------------------------------------------------------
datasets[["INEQUALITY_drug_deaths"]] <- datasets[["input_data"]][["INEQUALITY_drug_deaths"]] %>% 
janitor::clean_names() %>% 
rename(count = "rate")




# 4 Brexit ----------------------------------------------------------------

# Net migration Natural change --------------------------------------------

datasets[["MIGRATION_migration"]] <- datasets[["input_data"]][["MIGRATION_migration"]] %>% 
  janitor::clean_names() 

# Non-British nationals --------------------------------------------

datasets[["MIGRATION_non_british"]] <- datasets[["input_data"]][["MIGRATION_non_british"]] %>% 
  janitor::clean_names() 

# Population projections --------------------------------------------

datasets[["MIGRATION_projections"]] <- datasets[["input_data"]][["MIGRATION_projections"]] %>% 
  janitor::clean_names() %>% 
  mutate(year = as.numeric(year),
         population = population*1000000)


# 5 Ageing Population -------------------------------------------------------

# Changing age profiles ---------------------------------------------------

datasets[["AGEING_age_profile"]] <- datasets[["input_data"]][["AGEING_age_profile"]] %>% 
  janitor::clean_names() %>%
  mutate(male = as.numeric(male))

# Changing causes of death ------------------------------------------------
datasets[["AGEING_causes_of_death"]] <- datasets[["input_data"]][["AGEING_causes_of_death"]] %>% 
  janitor::clean_names() %>%
  rename("type" = cause)





# 6 Marriage --------------------------------------------------------------------
datasets[["SAME_SEX_MARRIAGE_marriages"]] <- datasets[["input_data"]][["MARRIAGE_marriages"]] %>% 
  janitor::clean_names() 

# Marriage types --------------------------------------------------------------------
datasets[["SAME_SEX_MARRIAGE_marriage_type"]] <- datasets[["input_data"]][["MARRIAGE_marriage_type"]] %>% 
  janitor::clean_names() %>% 
  mutate(type = ordered(type, levels = c("Church of Scotland",	
                                         "Roman Catholic",
                                         "Other",
                                         "Humanist",
                                         "Civil")))







# 7 Long term changes------------------------------------------------

# Stillbirths and infant deaths------------------------------------------------

datasets[["CHANGES_child_mortality"]] <- datasets[["input_data"]][["CHANGES_child_mortality"]] %>%
  gather(key = age, value = count, c(2:6)) %>% 
  rename("year" = Year)


# Births and deaths------------------------------------------------

datasets[["CHANGES_births_deaths"]] <- datasets[["input_data"]][["CHANGES_births_deaths"]] %>% 
  janitor::clean_names()



# Marriage age------------------------------------------------

datasets[["CHANGES_marriage_age"]] <- datasets[["input_data"]][["CHANGES_marriage_age"]] %>% 
  janitor::clean_names() %>% 
  mutate(year = as.ordered(year))