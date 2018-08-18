require (tidyverse)
require (openksh)

regional_ksh <- get_regional_ec_active_population() %>%
  left_join ( get_regional_economic_activity_rate() , by =  c("name", "level", "years")) %>%
  left_join ( get_regional_demography_age_group(),  by = c("name", "level", "years")) %>%
  left_join ( get_regional_building_permits(), by = c("name", "level", "years")) %>%
  left_join ( get_regional_dwellings (), by = c("name", "level", "years")) %>%
  left_join ( get_regional_dwellings_change (),  by = c("name", "level", "years")) %>%
  left_join ( get_regional_dwellings_change (),  by = c("name", "level", "years")) %>%
  left_join ( get_regional_net_earnings (),  by = c("name", "years")) %>%
  left_join ( get_regional_gdp (),  by = c("name", "years")) %>%
  left_join ( get_regional_theatre_cinema_admissions(), by = c("name", "years"))

saveRDS(regional_ksh, "data-raw/territorial_ksh_data.rds")

county_ksh <- get_regional_ec_active_population(region_level = "county") %>%
  left_join ( get_regional_economic_activity_rate(region_level = "county") , by =  c("name", "level", "years")) %>%
  left_join ( get_regional_demography_age_group(region_level = "county"),  by = c("name", "level", "years")) %>%
  left_join ( get_regional_building_permits(region_level = "county"), by = c("name", "level", "years")) %>%
  left_join ( get_regional_dwellings (region_level = "county"), by = c("name", "level", "years")) %>%
  left_join ( get_regional_dwellings_change (region_level = "county"),  by = c("name", "level", "years")) %>%
  left_join ( get_regional_dwellings_change (region_level = "county"),  by = c("name", "level", "years")) %>%
  left_join ( get_regional_net_earnings (region_level = "county"),  by = c("name", "years")) %>%
  left_join ( get_regional_gdp (region_level = "county"),  by = c("name", "years")) %>%
  left_join ( get_regional_theatre_cinema_admissions(region_level = "county"), by = c("name", "years"))


saveRDS(county_ksh, "data-raw/country_ksh_data.rds")
