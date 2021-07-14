library("tidyverse")
library("feather")

hes <- read_csv(
        "Vaccine_Hesitancy_for_COVID-19__County_and_local_estimates.csv",
        col_types = list(
            `FIPS Code` = col_character(),
            `County Name` = col_skip(),
            State = col_skip(),
            `Estimated hesitant` = col_double(),
            `Estimated hesitant or unsure` = col_skip(),
            `Estimated strongly hesitant` = col_skip(),
            `Social Vulnerability Index (SVI)` = col_double(),
            `SVI Category` = col_skip(),
            `CVAC level of concern for vaccination rollout` = col_double(),
            `CVAC Level Of Concern` = col_skip(),
            `Percent adults fully vaccinated against COVID-19 (as of 6/10/21)` = col_skip(),
            `Percent Hispanic` = col_skip(),
            `Percent non-Hispanic American Indian/Alaska Native` = col_skip(),
            `Percent non-Hispanic Asian` = col_skip(),
            `Percent non-Hispanic Black` = col_skip(),
            `Percent non-Hispanic Native Hawaiian/Pacific Islander` = col_skip(),
            `Percent non-Hispanic White` = col_skip(),
            `Geographical Point` = col_skip(),
            `State Code` = col_skip(),
            `County Boundary` = col_skip(),
            `State Boundary` = col_skip()
        )
)

hes_clean <- hes %>%
  rename(
         "hesitant" = `Estimated hesitant`,
         "svi" = `Social Vulnerability Index (SVI)`,
         "cvac" = `CVAC level of concern for vaccination rollout`
  ) %>%
  mutate(fips = str_pad(`FIPS Code`, 5, pad = "0")) %>%
  mutate(state = str_sub(fips, 1, 2), county = str_sub(fips, 3, 5)) %>%
  select(-`FIPS Code`, -fips) %>%
  relocate(state, county)

write_feather(hes_clean, "vaccine_hesitancy.feather")
