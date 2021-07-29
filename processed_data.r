library(tidyverse)
library(tidymodels)
library(probably)
library(themis)
library(feather)
library(magrittr)
library(skimr)
library(vip)
library(ggbeeswarm)
library(finetune)
library(lubridate)
library(glue)
library(slider)
per <- read_feather("data/simulation_data/all_persons.feather")

clients <-
  per %>%
  group_by(client) %>%
  summarize(
    zip3 = first(zip3),
    size = n(),
    volume = sum(FaceAmt),
    avg_qx = mean(qx),
    avg_age = mean(Age),
    per_male = sum(Sex == "Male") / size,
    per_blue_collar = sum(collar == "blue") / size,
    expected = sum(qx * FaceAmt),
    actual_2021 = sum(FaceAmt[year == 2021], na.rm = TRUE),
    ae_2021 = actual_2021 / (expected / 2),
    actual_2020 = sum(FaceAmt[year == 2020], na.rm = TRUE),
    ae_2020 = actual_2020 / expected,
    actual_2019 = sum(FaceAmt[year == 2019], na.rm = TRUE),
    ae_2019 = actual_2019 / expected,
    adverse = as_factor(if_else(ae_2020 > 3, "ae > 3", "ae < 3"))
  ) %>%
  relocate(adverse, ae_2020, .after = zip3) %>%
  mutate(adverse = fct_relevel(adverse, c("ae > 3", "ae < 3")))


zip_data <-
  read_feather("data/data.feather") %>%
  mutate(
    density = POP / AREALAND,
    AREALAND = NULL,
    AREA = NULL,
    HU = NULL,
    vaccinated = NULL,
    per_lib = NULL,
    per_green = NULL,
    per_other = NULL,
    per_rep = NULL,
    unempl_2020 = NULL,
    deaths_covid = NULL,
    deaths_all = NULL
  ) %>%
  rename(
    unemp = unempl_2019,
    hes_uns = hes_unsure,
    str_hes = strong_hes,
    income = Median_Household_Income_2019
  )


clients %<>%
  inner_join(zip_data, by = "zip3") %>%
  drop_na()

deaths <-
  read_feather("data/deaths_zip3.feather") %>%
  mutate(date = ceiling_date(date, unit = "week")) %>%
  group_by(zip3, date) %>%
  summarize(totdeaths = max(deaths)) %>%
  mutate(zip_deaths = totdeaths - lag(totdeaths, default = 0)) %>%
  select(-totdeaths) %>%
  ungroup()

# Looking at only people that died
per %<>%
  drop_na() %>%
  select(-month)

# Fixing week 53
wk53 <-
  per %>%
  filter(year == 2019, week == 53) %>%
  mutate(year = 2020, week = 1)
per %<>%
  rows_update(wk53, by = c("client", "participant"))

per %<>%
  nest_by(week, year) %>%
  mutate(date = ceiling_date(ymd(glue("{year}-01-01")) + weeks(week - 1), unit = "week")) %>%
  ungroup() %>%
  select(-week, -year) %>%
  unnest(cols = c(data))

no_deaths <-
  clients %>%
  anti_join(per, by = "client") %>%
  select(client) %>%
  mutate(date = ceiling_date(ymd("2019-01-01"), unit = "week"), claims = 0)

weekly_claims <-
  per %>%
  group_by(date, client) %>%
  summarize(claims = sum(FaceAmt), .groups = "drop") %>%
  bind_rows(no_deaths) %>%
  complete(date, client, fill = list(claims = 0))

weekly_data <-
  clients %>%
  left_join(weekly_claims, by = c("client")) %>%
  relocate(date) %>%
  relocate(claims, .after = zip3) %>%
  left_join(deaths, by = c("date", "zip3")) %>%
  relocate(zip_deaths, .after = claims) %>%
  mutate(zip_deaths = replace_na(zip_deaths, 0), ae = claims / (expected / 52.18))

smoother <- function(x) { weighted.mean(x, dnorm(seq(-1, 0, length.out = length(x)), sd = 0.33)) }
sliding_smoother <-
  timetk::slidify(smoother, .period = 13, .align = "right")

weekly_data <-
  weekly_data %>%
  group_by(client) %>%
  mutate(smoothed_ae = sliding_smoother(ae), smoothed_deaths = sliding_smoother(zip_deaths), .before = size) %>%
  drop_na()


client_shrinkage <-
  weekly_data %>%
  summarize(dep_var = first(volume * avg_qx)) %>%
  mutate(shrinkage = rescale(log(dep_var), to = c(0.3, 1)))

processed_data <-
  weekly_data %>%
  left_join(client_shrinkage, by = "client") %>%
  ungroup() %>%
  mutate(shrunk_ae = smoothed_ae * shrinkage, .after = smoothed_ae)


processed_data <-
  processed_data %>%
  ungroup() %>%
  mutate(class = factor(if_else(shrunk_ae > 2.5, "Adverse", "Not adverse"), levels = c("Adverse", "Not adverse")), .after = shrunk_ae)

states <-
  read_delim("data/state.txt", delim = "|") %>%
  select(STATE, STATE_NAME)

zip_to_state <-
  read_csv("data/zcta_county_rel_10.txt") %>%
  select(ZCTA5, STATE) %>%
  mutate(zip3 = str_sub(ZCTA5, 1, 3), .keep = "unused") %>%
  group_by(zip3) %>%
  count(STATE, sort = TRUE) %>%
  slice_head() %>%
  ungroup() %>%
  left_join(states, by = "STATE") %>%
  select(-STATE, -n)

processed_data <-
  processed_data %>%
  nest_by(zip3) %>%
  left_join(zip_to_state, by = "zip3") %>%
  unnest(cols = c(data))

ihme <-
  read_csv("data/2020_12_23/reference_hospitalization_all_locs.csv") %>%
  rename(STATE_NAME = location_name) %>%
  semi_join(processed_data, by = "STATE_NAME") %>%
  select(STATE_NAME, date, deaths_mean) %>%
  mutate(date = ceiling_date(date, unit = "week")) %>%
  group_by(STATE_NAME, date) %>%
  summarize(ihme_deaths = sum(deaths_mean))

processed_data <-
  processed_data %>%
  left_join(ihme, by = c("date", "STATE_NAME")) %>%
  ungroup() %>%
  mutate(ihme_deaths = replace_na(ihme_deaths, 0))

write_feather(processed_data, "data/processed_data_20_12_23.feather")
