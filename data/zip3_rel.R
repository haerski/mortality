library("tidyverse")
library("feather")

zip_rel <- read_csv("zcta_county_rel_10.txt",
            col_types = list(
              ZCTA5 = col_character(),
              STATE = col_character(),
              COUNTY = col_character(),
              GEOID = col_skip(),
              POPPT = col_double(),
              HUPT = col_double(),
              AREAPT = col_double(),
              AREALANDPT = col_double(),
              ZPOP = col_double(),
              ZHU = col_double(),
              ZAREA = col_double(),
              ZAREALAND = col_double(),
              COPOP = col_double(),
              COHU = col_double(),
              COAREA = col_double(),
              COAREALAND = col_double(),
              ZPOPPCT = col_double(),
              ZHUPCT = col_double(),
              ZAREAPCT = col_double(),
              ZAREALANDPCT = col_double(),
              COPOPPCT = col_double(),
              COHUPCT = col_double(),
              COAREAPCT = col_double(),
              COAREALANDPCT = col_double())
)
zip3_rel <- zip_rel %>%
  select(ZCTA5, STATE, COUNTY, POPPT:AREALANDPT) %>%
  mutate(zip3 = str_sub(ZCTA5, 1, 3)) %>%
  group_by(STATE, COUNTY, zip3) %>%
  summarize(across(POPPT:AREALANDPT, sum), .groups = "drop") %>%
  rename(state = STATE, county = COUNTY)

write_feather(zip3_rel, "zip3_rel.feather")
