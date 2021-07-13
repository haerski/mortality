# Vaccine hesitancy data
See [this story from the CDC](https://data.cdc.gov/stories/s/Vaccine-Hesitancy-for-COVID-19/cnd2-a6zw).
Contains PUMA-level estimation for vaccine hesitancy, along with county-level information of other data.

# Census API information

We use the [tidycensus](https://walker-data.com/tidycensus/index.html) package to obtain the data.
Install it using `install.packages("tidycensus")`.

See the file `census.R` for some ideas.

Use `geography="zcta"` with `get_acs()` or `get_decennial()` to get results by ZIP code.

We also use [censusapi](https://www.hrecht.com/censusapi/)
