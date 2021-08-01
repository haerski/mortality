library("tidyverse")
library("censusapi")
library("feather")

Sys.setenv(CENSUS_KEY = "YOUR KEY HERE")
apis <- listCensusApis() %>% tibble()

# population
# date_code = 12 is an estimate for July 1, 2019
# total population + density
pop <- getCensus(
                 name = "pep/population",
                 vintage = 2019,
                 region = "county:*",
                 vars = c("POP", "DENSITY"),
                 DATE_CODE = 12)
pop <- tibble(pop) %>%
  select(-DATE_CODE)
write_feather(pop, "pop_den.feather")




# population by sex and age
# Ethnicity and race are not taken into account
pop_sa <- getCensus(
                 name = "pep/charagegroups",
                 vintage = 2019,
                 region = "county:*",
                 vars = c("POP", "SEX", "AGEGROUP"),
                 DATE_CODE = 12)
pop_sa <- tibble(pop_sa) %>%
  select(-DATE_CODE)

write_feather(pop_sa, "pop_sex_age.feather")
