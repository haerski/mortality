library(tidyverse)
library(feather)

zip3_rel <- read_feather("zip3_rel.feather")

deaths <- read_csv("covid_deaths_usafacts.csv") %>%
  mutate(countyFIPS = as.character(countyFIPS)) %>%
  mutate(countyFIPS = str_pad(countyFIPS, 5, pad = "0")) %>%
  mutate(state = str_sub(countyFIPS, 1, 2), county = str_sub(countyFIPS, 3, 5)) %>%
  select(!c(`County Name`, State, StateFIPS, countyFIPS)) %>%
  relocate(state, county)

deaths_zip3_wide <- deaths %>%
  inner_join(zip3_rel, by = c("state", "county")) %>%
  group_by(zip3) %>%
  summarize(across(`2020-01-22`:`2021-07-12`, ~ sum(.x * POPPT / sum(POPPT))))

deaths_zip3 <- deaths_zip3_wide %>%
  pivot_longer(-1, names_to = "date", values_to = "deaths") %>%
  mutate(date = parse_date(date))

write_feather(deaths_zip3, "deaths_zip3.feather")
