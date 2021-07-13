library("tidyverse")
library("tidycensus")

census_api_key("d7575c8e329c59a4e3368f616ec4559547c90aae")

age10 <- get_decennial(geography = "zcta",
                       variables = "P013001",
                       year = 2010)
age10 %>% head()


library("censusapi")
Sys.setenv(CENSUS_KEY = "d7575c8e329c59a4e3368f616ec4559547c90aae")
apis <- listCensusApis() %>% tibble()

apis %>%
  filter(vintage == 2010) %>%
  View()

census_vars <- listCensusMetadata(
  name = "dec/sf1",
  vintage = 2010,
  type = "variables"
)

census_geo <- listCensusMetadata(
  name = "dec/sf1",
  vintage = 2010,
  type = "geography"
)

pop <- getCensus(
  name = "dec/sf1",
  vintage = 2010,
  vars = c("NAME", "P001001", "H010001"),
  region = "zip code tabulation area:*")
