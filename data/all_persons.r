library(tidyverse)
library(tidymodels)
library(feather)

read_data <- function(n) {
  exp_name <- str_glue("simulation_data/experience_weekly_{n}.RDS")
  per_name <- str_glue("simulation_data/person_{n}.RDS")
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

qx_table <- read_csv("soa_base_2017.csv")

all_persons <-
  all_persons %>%
  left_join(qx_table, by = c("Age", "Sex", "collar")) %>%
  relocate(qx, .after = collar)

# This generates a data frame with all individual and date of death (NULL if they don't die)
write_feather(all_persons, "simulation_data/all_persons.feather")
