library(tidyverse)
library(sf)
library(feather)

zip_geom <- st_read("cb_2019_us_zcta510_500k-1/cb_2019_us_zcta510_500k.shp")
zip3_data <- read_feather("../data/data.feather")

zip_geom <-
  zip_geom %>%
  mutate(zip3 = str_sub(ZCTA5CE10, 1, 3)) %>%
  select(zip3, geometry) %>%
  semi_join(zip3_data) %>%
  group_by(zip3) %>%
  summarize(is_coverage = TRUE)

# remove AK and HI
zip_geom_no_ak_hi <-
  zip_geom %>%
  filter(!zip3 %in% c("995","996","997","998","999","968","967"))

write_sf(zip_geom, "zip_geom.shp")
write_sf(zip_geom_no_ak_hi, "zip_geom_no_ak_hi.shp")
