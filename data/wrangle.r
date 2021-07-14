library("tidyverse")
library("feather")
library("zipcodeR")


# wrangling here
pop <- read_feather("pop_den.feather")
get_zip3_list <- function(state, county) {
  search_fips(state, county) %>%
  filter(!is.na(population)) %>%
  transmute(zip3 = str_sub(zipcode, 1, 3)) %>%
  unique()
}

# TODO: use the data from search_fips to compute the "true" density and population of a zip code
pop_zip3 <- pop %>%
  mutate(zip3list = map2(state, county, get_zip3_list)) %>%
  unnest(cols = c(zip3list)) %>%
  group_by(zip3) %>%
  summarize(density = sum(DENSITY * POP / sum(POP)),
            pop = sum(POP)) %>%
  drop_na() # gets rid of Puerto Rico and Virgin islands

write_feather(pop_zip3, "pop_den_zip3.feather")
