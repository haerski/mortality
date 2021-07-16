library(tidyverse)
library(tidymodels)
library(feather)

read_data <- function(n) {
  exp_name <- str_glue("data/simulation_data/experience_weekly_{n}.RDS")
  per_name <- str_glue("data/simulation_data/person_{n}.RDS")
  exp <- read_rds(exp_name)
  per <- read_rds(per_name)

  dies <-
    exp %>%
    filter(death == 1) %>%
    select(client, participant, week, month, year)
  aug_per <-
    per %>%
    left_join(dies, by = c("client", "participant"))

  aug_per
}

all_persons <- (1:10) %>% map_dfr(read_data)

qx_table <- read_csv("data/soa_base_2017.csv")

all_persons <-
  all_persons %>%
  left_join(qx_table, by = c("Age", "Sex", "collar")) %>%
  relocate(qx, .after = collar)

# This generates a data frame with all individual and date of death (NULL if they don't die)
write_feather(all_persons, "data/simulation_data/all_persons.feather")

# Load it insted of reprocessing
per <- read_feather("data/simulation_data/all_persons.feather")

# This is how much we expect to pay for each client in a year
exp <-
  per %>%
  group_by(client) %>%
  summarize(expected = sum(qx * FaceAmt), zip3 = first(zip3))

# This is how much we payed for each client in 2019
# This is terrible code, please fix
act_2019 <-
  per %>%
  filter(year == 2019) %>%
  group_by(client) %>%
  summarize(actual2019 = sum(FaceAmt))
act_2020 <-
  per %>%
  filter(year == 2020) %>%
  group_by(client) %>%
  summarize(actual2020 = sum(FaceAmt))
act_2021 <-
  per %>%
  filter(year == 2021) %>%
  group_by(client) %>%
  summarize(actual2021 = sum(FaceAmt))

# This is also terrible...
exp <-
  exp %>%
  left_join(act_2019) %>%
  left_join(act_2020) %>%
  left_join(act_2021) %>%
  replace_na(list(actual2019 = 0)) %>%
  replace_na(list(actual2020 = 0)) %>%
  replace_na(list(actual2021 = 0)) %>%
  mutate(AE2019 = actual2019 / expected) %>%
  mutate(AE2020 = actual2020 / expected) %>%
  mutate(AE2021 = actual2021 / ((7 / 12) * expected))

exp

other_data <-
  read_feather("data/data.feather") %>%
  select(zip3, POP, AREALAND, `Deaths involving COVID-19`, per_dem, `Social Vulnerability Index (SVI)`, `CVAC level of concern for vaccination rollout`) %>%
  rename(covid_deaths = `Deaths involving COVID-19`, svi = `Social Vulnerability Index (SVI)`, cvac = `CVAC level of concern for vaccination rollout`) %>%
  mutate(density = POP / AREALAND, AREALAND = NULL)

data <-
  exp %>%
  left_join(other_data) %>%
  mutate(zip3 = factor(zip3)) %>%
  mutate(Y = factor(AE2020 > 1.1))

glimpse(data)

data %>%
  count(Y)

## MODELLING ##
splits <- initial_split(data, strata = Y)
data_test <- testing(splits)
data_train <- training(splits)

data_test %>%
  count(Y)
data_train %>%
  count(Y)

client_rec <-
  recipe(Y ~ ., data = data_train) %>%
  update_role(client, new_role = "client ID") %>%
  update_role(actual2020, actual2021, AE2020, AE2021, new_role = "future")

summary(client_rec)

rf_mod <-
  rand_forest(trees = 1000) %>%
  set_engine("ranger") %>%
  set_mode("classification")

rf_wf <-
  workflow() %>%
  add_model(rf_mod) %>%
  add_recipe(client_rec)

rf_fit <-
  rf_wf %>%
  fit(data_train)

rf_fit %>%
  pull_workflow_fit()

rf_pred <-
  rf_fit %>%
  predict(data_test)

rf_pred
data_test$Y
